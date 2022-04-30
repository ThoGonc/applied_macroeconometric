rm(list = ls())
graphics.off()
library(mFilter)
library(tidyverse)
library(readxl)

#préparation des données pour France
#PIB
urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_gdp_quarter_sa.csv'
dsin<-read.csv2(urlfile, header=TRUE)
myts<-ts(data=dsin,start=(1975),end=(2022),frequency=4)

#HP smoother
France<-dsin[[2]]
France <- na.omit(France) 
logFrance <- log(France)
Francets<-ts(data=logFrance,start=(1975),end=(2022),frequency=4)
Francets[189] <- NA_real_
Francets <- na.omit(Francets)

#filtre HP
France_hp<- hpfilter(Francets, freq=1600,type="lambda",drift=FALSE)
plot(France_hp)

#initial value pot_France
France_hp_cycle <- France_hp[["cycle"]]
pot_France_begin <- France_hp_cycle[[2]]
pot_France_begint1 <- France_hp_cycle[[3]]

#Inflation
urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_inflation.csv'
df_infla<-read.csv2(urlfile, header=TRUE)
France_infla <-df_infla[2]
France_inflats<-ts(data=France_infla,start=(1975),end=(2022),frequency=4)
France_infla <- matrix(data = France_inflats)
France_infla <- na.omit(France_infla)

#création du lag inflation et delta log(PIB)
France_laginfla <- lag(France_infla)

France_PIB <- matrix(data = Francets)
France_logPIB <- log(France_PIB)
France_deltalogPIB <- diff(France_logPIB)
France_deltalogPIB[187,1] <- NA_real_

#préparation de la matrice var d'observation
mat_obs <- matrix(, nrow = 188, ncol = 3)
mat_obs[,1] <- France_deltalogPIB*100
mat_obs[,2] <- France_infla
mat_obs[,3] <- France_laginfla
mat_obs <- na.omit(mat_obs)


#install.packages("MARSS")
library(MARSS)


b1 <- 0.46116
b2 <- 0.01277
alpha3 <- 0.001536
delta <- 0.4656
alpha1 <- 0.024034
alpha2 <- 0.971968

#model3: multivar with lags fixed coef from Eviews
B2 <- matrix(list(b1, 1, b2, 0), 2, 2)
Z2 <- matrix(list(1, alpha3, 0, -1, 0, 0), nrow=3, ncol=2)
A2 <- matrix(list(delta, alpha1, 0), nrow=3, ncol=1)
Q2 <- matrix(list("q1", 0, 0, 0), 2, 2)
u2 <- matrix(list(0,0),nrow = 2, ncol = 1)
d2 <- t(mat_obs)
D2 <- matrix(list (0, 0, 0, 0, 0, 0, 0, alpha2, 1), 3, 3)
R2 <- matrix(list ("r11", 0, 0, 0, "r22", 0, 0, 0, 0.01), 3, 3)
x02 <- matrix(list(0, 0), nrow = 2, ncol = 1)
model.list2 <- list(B = B2, Q=Q2, Z = Z2, A = A2, d=d2, D=D2, U=u2, R=R2, x0= x02, tinitx = 1)
fit <- MARSS(d2, model=model.list2, fit = TRUE)
France_KF3 <- fitted(fit, type="ytT", interval = c("confidence"),level = 0.95, output = c("data.frame", "matrix"))
France_KF4 <- tsSmooth(fit,
                       type = c("xtT", "xtt", "xtt1", "ytT", "ytt", "ytt1"),
                       interval = c("confidence"),
                       level = 0.95, fun.kf = c("MARSSkfas"))
cycleKF<-ts(data=France_KF4$.estimate,start=(1975),end=(2022),frequency=4)
plot(cycleKF)

#model4: multivar with lags fixed coef de OLS R
B2 <- matrix(list(0.46116, 1, 0.01277, 0), 2, 2)
Z2 <- matrix(list(1, 0.01654, 0, -1, 0, 0), nrow=3, ncol=2)
A2 <- matrix(list(0.4656, -0.18415, 0), nrow=3, ncol=1)
Q2 <- matrix(list("q1", 0, 0, 0), 2, 2)
u2 <- matrix(list(0,0),nrow = 2, ncol = 1)
d2 <- t(mat_obs)
D2 <- matrix(list (0, 0, 0, 0, 0, 0, 0, 0.97804, 1), 3, 3)
R2 <- matrix(list ("r11", 0, 0, 0, "r22", 0, 0, 0, 0.01), 3, 3)
x02 <- matrix(list(0, 0), nrow = 2, ncol = 1)
model.list2 <- list(B = B2, Q=Q2, Z = Z2, A = A2, d=d2, D=D2, U=u2, R=R2, x0= x02, tinitx = 1)
fit <- MARSS(d2, model=model.list2, fit = TRUE)
France_KF3 <- fitted(fit, type="ytT", interval = c("confidence"),level = 0.95, output = c("data.frame", "matrix"))
France_KF4 <- tsSmooth(fit,
                       type = c("xtT", "xtt", "xtt1", "ytT", "ytt", "ytt1"),
                       interval = c("confidence"),
                       level = 0.95, fun.kf = c("MARSSkfas"))





#model espace-état, multivarié (avec lags), coefficients indéterminés
B2 <- matrix(list("b1", 1, "b2", 0), 2, 2)
Z2 <- matrix(list(1, "alpha3", 0, -1, 0, 0), nrow=3, ncol=2)
A2 <- matrix(list("delta", "alpha1", 0), nrow=3, ncol=1)
Q2 <- matrix(list("q1", 0, 0, 0), 2, 2)
u2 <- matrix(list(0,0),nrow = 2, ncol = 1)
d2 <- t(mat_obs)
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

France_KF4_trend <- France_logPIB [2:185,1] - France_KF4[1:184,3]
plot (France_KF4_trend)
plot (France_logPIB)
cycleKF<-ts(data=France_KF4$.estimate,start=(1975),end=(2022),frequency=4)
plot(cycleKF)