rm(list = ls())
graphics.off()

#install.packages("readxl")
#install.packages("mFilter")
library(RCurl)
library(mFilter)
library(tidyverse)
library(readxl)

#gdp annuel
urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_gdp_quarter_sa.csv'
gdp<-read.csv2(urlfile, header=TRUE)
mygdpts<-ts(data=gdp,start=(1975),end=(2022),frequency=4)


France<-gdp[[2]]

logFrance<-log(France)
Francets<-ts(data=logFrance,start=(1975),end=(2022),frequency=4)     

Francets<- na.omit(Francets) 
plot(Francets)


France_hp <- hpfilter(Francets, freq = 1600, type = "lambda",drift=FALSE)


# Plot time series
plot.ts(Francets, ylab = "")  

# include HP trend
lines(France_hp$trend, col = "red")
legend("topleft", legend = c("Log GDP France", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")

#Plot cycle
plot.ts(France_hp$cycle, ylab = "") 
legend("topleft", legend = c("HP cycle France"), lty = 1, col = c("black"), 
       bty = "n")


#install.packages("MARSS")
#install.packages("dlm")
#installed.packages("broom")
library(MARSS)
library(dlm)

#initial value pot_France
France_hp_cycle <- France_hp[["cycle"]]
pot_France_begin <- France_hp_cycle[[1]]
pot_France_begint1 <- France_hp_cycle[[2]]



urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_inflation.csv'
df_infla<-read.csv2(urlfile, header=TRUE)
France_infla <-df_infla[2]
France_inflats<-ts(data=France_infla,start=(1975),end=(2022),frequency=4)
France_infla <- matrix(data = France_inflats)
France_infla <- na.omit(France_infla)




France_PIB <- matrix(data = Francets)
France_logPIB <- log(France_PIB)


#préparation matrice var d'observation


delta_log_pib<-diff(France_logPIB)*100
France_laginfla<-lag(France_infla)
France_laginfla<-France_laginfla[-188]

M3<-matrix(,nrow=187,ncol=3)
M3[,1]<-delta_log_pib
France_infla<-France_infla[-188]
M3[,2]<-France_infla
M3[,3]<-France_laginfla
M3 <- na.omit(M3)




#model2: multivar with lags
B2 <- matrix(list(1.05, 1, "b2", 0), 2, 2)
Z2 <- matrix(list(1, "alpha3", 0, -1, 0, 0), nrow=3, ncol=2)
A2 <- matrix(list("delta", "alpha1", 0), nrow=3, ncol=1)
Q2 <- matrix(list("q1", 0, 0, 0), 2, 2)
u2 <- matrix(list(0,0),nrow = 2, ncol = 1)
d2 <- t(M3)
D2 <- matrix(list (0, 0, 0, 0, 0, 0, 0, "alpha2", 1), 3, 3)
R2 <- matrix(list ("r11", 0, 0, 0, "r22", 0, 0, 0, 0.01), 3, 3)
x02 <- matrix(list(pot_France_begint1, pot_France_begin), nrow = 2, ncol = 1)
model.list2 <- list(B = B2, Q=Q2, Z = Z2, A = A2, d=d2, D=D2, U=u2, R=R2, x0= x02, tinitx = 1)
fit <- MARSS(d2, model=model.list2, fit = TRUE)
France_KF3 <- fitted(fit, type="ytT", interval = c("confidence"),level = 0.95, output = c("data.frame", "matrix"))
France_KF4 <- tsSmooth(fit,
                    type = c("xtT", "xtt", "xtt1", "ytT", "ytt", "ytt1"),
                    interval = c("confidence"),
                    level = 0.95, fun.kf = c("MARSSkfas"))


ggplot2::autoplot(fit, plot.type = "fitted.xtT")

France_KF4_trend <- France_logPIB [2:188,1] - France_KF4[1:187,3]
plot (France_KF4_trend)


