rm(list = ls())
graphics.off()

#install.packages("readxl")
#install.packages("mFilter")
library(RCurl)
library(mFilter)
library(tidyverse)
library(readxl)

urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_pib_volume.csv'
dsin<-read.csv2(urlfile, header=TRUE)
myts<-ts(data=dsin,start=(1970),end=(2022),frequency=4)


#HP smoother
USA<-dsin[[33]]
USA <- na.omit(USA) 
USAts<-ts(data=log(USA),start=(1970),end=(2021),frequency=4)
USA_hp<- hpfilter(USAts, freq=100,type="frequency",drift=TRUE)
plot(USA_hp)

#install.packages("MARSS")
#install.packages("dlm")
installed.packages("broom")
library(MARSS)
library(dlm)

#initial value pot_USA
USA_hp_cycle <- USA_hp[["cycle"]]
pot_USA_begin <- log(USA_hp_cycle[[1]])
pot_USA_begint1 <- log(USA_hp_cycle[[2]])


#data inflation Q1_1970 Q4_2020 (205 values)

urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_inflation.csv'
df_infla<-read.csv2(urlfile, header=TRUE)
USA_infla <-df_infla[32]
USA_inflats<-ts(data=USA_infla,start=(1970),end=(2021),frequency=4)
USA_infla <- matrix(data = USA_inflats)

#data PIB Q1_1970 Q4_2020 (205 values)
USA_PIB <- matrix(data = USAts)
USA_logPIB <- USA_PIB

#préparation matrice var d'observation
mat_obs <- matrix(, nrow = 205, ncol = 3)
mat_obs[,1] <- USA_logPIB 
mat_obs[,2] <- USA_infla


#création du lag inflation et delta PIB
for (i in seq_along(USA_infla)) {
  if (i == 1) {
    mat_obs[i, 3] <- NA_real_
  } else {
    mat_obs[i, 1] <- mat_obs[i, 1]- mat_obs[i - 1, 1]
    mat_obs[i, 3] <- mat_obs[i - 1, 2]
  }
}

mat_obs <- na.omit(mat_obs)


#model1: univar, avec inflation
B1 <- matrix("b")
Z1 <- matrix(1)
A1 <- matrix("a")
u1 <- "zero"
D1 <- t(matrix(data = USA_inflats))
x0 <- 28
model.list1 <- list(B = B1, Z = Z1, A = A1, d=D1, R=matrix(1), V0="identity", tinitx = 1)
fit <- MARSS(USAts, model=model.list1, fit = TRUE, fun.kf = ("MARSSkfss"))
USA_KF2 <- fitted(fit, type="ytt1", interval = c("none", "confidence", "prediction"),level = 0.95, output = c("data.frame", "matrix"))
summary(fit)
ggplot2::autoplot(fit, plot.type = "fitted.ytT")

#model2: multivar with lags
B2 <- matrix(list("b1", 1, "b2", 0), 2, 2)
Z2 <- matrix(list(1, "alpha3", 0, -1, 0, 0), nrow=3, ncol=2)
A2 <- matrix(list("delta", "alpha1", 0), nrow=3, ncol=1)
Q2 <- matrix(list("q1", 0, 0, 0), 2, 2)
u2 <- matrix(list(0,0),nrow = 2, ncol = 1)
d2 <- t(mat_obs)
D2 <- matrix(list (0, 0, 0, 0, 0, 0, 0, "alpha2", 1), 3, 3)
R2 <- matrix(list ("r11", 0, 0, 0, "r22", 0, 0, 0, 0.001), 3, 3)
x02 <- matrix(list(pot_USA_begint1, pot_USA_begin), nrow = 2, ncol = 1)
model.list2 <- list(B = B2, Q=Q2, Z = Z2, A = A2, d=d2, D=D2, U=u2, R=R2, x0= x02, tinitx = 1)
fit <- MARSS(d2, model=model.list2, fit = TRUE)
USA_KF3 <- fitted(fit, type="ytt1", interval = c("none", "confidence", "prediction"),level = 0.95, output = c("data.frame", "matrix"))
USA_KF4 <- tsSmooth(fit,
                    type = c("xtT", "xtt", "xtt1", "ytT", "ytt", "ytt1"),
                    interval = c("none", "confidence", "prediction"),
                    level = 0.95, fun.kf = c("MARSSkfas", "MARSSkfss"))


#model2: multivar with lags, coefficients fixés
B2 <- matrix(list(1.03, 1, -0.26, 0), 2, 2)
Z2 <- matrix(list(1, -0.18, 0, -1, 0, 0), 3, 2)
A2 <- matrix(list(0.77, 2.4, 0), 3, 1)
Q2 <- matrix(list("q1", 0, 0, 0), 2, 2)
u2 <- matrix(list(0,0),nrow = 2, ncol = 1)
d2 <- t(mat_obs)
D2 <- matrix(list (0, 0, 0, 0, 0, 0, 0, 0.82, 1), 3, 3)
R2 <- matrix(list ("r11", 0, 0, 0, "r22", 0, 0, 0, 0.01), 3, 3)
x02 <- matrix(list(pot_USA_begint1, pot_USA_begin), nrow = 2, ncol = 1)
model.list2 <- list(B = B2, Q=Q2, Z = Z2, A = A2, d=d2, D=D2, U=u2, R=R2, x0= x02, tinitx = 1)
fit <- MARSS(d2, model=model.list2, fit = TRUE)
USA_KF3 <- fitted(fit, type="ytt1", interval = c("none", "confidence", "prediction"),level = 0.95, output = c("data.frame", "matrix"))
USA_KF4 <- tsSmooth(fit,
                    type = c("xtT", "xtt", "xtt1", "ytT", "ytt", "ytt1"),
                    interval = c("confidence"),
                    level = 0.95, fun.kf = c("MARSSkfss"))



USA_KF4exp <- exp(USA_KF4[,3])
ggplot2::autoplot(fit, plot.type = "fitted.xtt1")

USA_KF4_trend <- USA_logPIB [2:205,1] - USA_KF4[1:204,3]
plot (USA_KF4_trend)
plot (USA_logPIB)

