rm(list = ls())
graphics.off()



install.packages("devtools")
devtools::install_github("KevinKotze/tsm")
install.packages("dlm")

library(tsm)
library(dlm)



dat <- sarb_quarter$KBP6006L/sarb_quarter$KBP6006D
dat.tmp <- diff(log(na.omit(dat)) * 100, lag = 1)
head(dat)

inf <- ts(dat.tmp, start = c(1960, 2), frequency = 4)

plot.ts(inf)


fn <- function(parm) {
  dlmModPoly(order = 1, dV = exp(parm[1]), dW = exp(parm[2]))
}


fit <- dlmMLE(inf, rep(0, 2), build = fn, hessian = TRUE)
(conv <- fit$convergence)

loglik <- dlmLL(inf, dlmModPoly(1))
n.coef <- 2
r.aic <- (2 * (loglik)) + 2 * (sum(n.coef))  #dlmLL caculates the neg. LL
r.bic <- (2 * (loglik)) + (log(length(inf))) * (n.coef)


mod <- fn(fit$par)
obs.error.var <- V(mod)
state.error.var <- W(mod)


filtered <- dlmFilter(inf, mod = mod)
smoothed <- dlmSmooth(filtered)


resids <- residuals(filtered, sd = FALSE)
mu <- dropFirst(smoothed$s)
mu.1 <- mu[1]
mu.end <- mu[length(mu)]




par(mfrow = c(2, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(inf, col = "darkgrey", xlab = "", ylab = "", lwd = 1.5)
lines(mu, col = "black")
legend("topright", legend = c("Observed Deflator", "Stochastic level"), 
       lwd = c(2, 1), col = c("darkgrey", "black"), bty = "n")

plot.ts(resids, ylab = "", xlab = "", col = "darkgrey", 
        lwd = 1.5)
abline(h = 0)
legend("topright", legend = "Residuals", lwd = 1.5, col = "darkgrey", 
       bty = "n")