
rm(list = ls())
graphics.off()
#Tentative de replication du code eviews


library(readxl)
library(tseries)
library(mFilter)
library(tsm)
library(dlm)


rm(list = ls())
graphics.off()
MySheet <- read_excel("D:/2021-22/ENSAE_papiers/Cours/S2/2.1 . Applied Macroeconometrics/applied_macroeconometrics/filtering/pottest.xlsx")

gdp<-MySheet[[3]]
infl<-MySheet[[5]]

lgdp<-log(gdp)
dlgdp<-100*diff(lgdp)


lgdp_hp<- hpfilter(lgdp, freq=1600,type="frequency",drift=FALSE)    #Vérifier s'il faut un drift ou pas
lgdpt_hp<-lgdp_hp$trend


lgdpc_hp= lgdp - lgdpt_hp


lgdpzz <- ts(dlgdp, start = c(1960, 2), frequency = 4)

plot.ts(lgdpzz)



fn <- function(parm) {  dlmModPoly(order = 1, dV = exp(parm[1]), dW = exp(parm[2]))}


fit <- dlmMLE(lgdpzz, rep(0, 2), build = fn, hessian = TRUE)
(conv <- fit$convergence)





loglik <- dlmLL(lgdpzz, dlmModPoly(1))
n.coef <- 2
r.aic <- (2 * (loglik)) + 2 * (sum(n.coef))  #dlmLL caculates the neg. LL
r.bic <- (2 * (loglik)) + (log(length(lgdpzz))) * (n.coef)


mod <- fn(fit$par)
obs.error.var <- V(mod)
state.error.var <- W(mod)


filtered <- dlmFilter(lgdpzz, mod = mod)
smoothed <- dlmSmooth(filtered)


resids <- residuals(filtered, sd = FALSE)
mu <- dropFirst(smoothed$s)
mu.1 <- mu[1]
mu.end <- mu[length(mu)]



par(mfrow = c(2, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(lgdpzz, col = "darkgrey", xlab = "", ylab = "", lwd = 1.5)
lines(mu, col = "black")
legend("topright", legend = c("Observed Deflator", "Stochastic level"), 
       lwd = c(2, 1), col = c("darkgrey", "black"), bty = "n")

plot.ts(resids, ylab = "", xlab = "", col = "darkgrey", 
        lwd = 1.5)
abline(h = 0)
legend("topright", legend = "Residuals", lwd = 1.5, col = "darkgrey", 
       bty = "n")