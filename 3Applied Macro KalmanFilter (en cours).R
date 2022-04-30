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
France<-dsin[[2]]
France <- na.omit(France) 
Franceexp <- exp(France)
Francets<-ts(data=log(Franceexp),start=(1975),end=(2022),frequency=4)
France_hp<- hpfilter(Francets, freq=1600,type="lambda",drift=FALSE)
plot(France_hp)

#install.packages("MARSS")
#install.packages("dlm")
installed.packages("broom")
library(MARSS)
library(dlm)

#initial value pot_France
France_hp_cycle <- France_hp[["cycle"]]
pot_France_begin <- France_hp_cycle[[1]]
pot_France_begint1 <- France_hp_cycle[[2]]

#data inflation Q1_1970 Q4_2020 (205 values)

urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_inflation.csv'
df_infla<-read.csv2(urlfile, header=TRUE)
France_infla <-df_infla[2]
France_inflats<-ts(data=France_infla,start=(1975),end=(2022),frequency=4)
France_infla <- matrix(data = France_inflats)
France_infla <- na.omit(France_infla)


#data PIB Q1_1970 Q4_2020 (205 values)
France_PIB <- matrix(data = Francets)
France_logPIB <- log(France_PIB)

#préparation matrice var d'observation
mat_obs <- matrix(, nrow = 188, ncol = 3)
mat_obs[,1] <- France_logPIB
mat_obs[,2] <- France_infla


#création du lag inflation et delta PIB
for (i in seq_along(France_infla)) {
  if (i == 1) {
    mat_obs[i, 3] <- NA_real_
  } else {
    mat_obs[i, 1] <- mat_obs[i, 1]- mat_obs[i - 1, 1]
    mat_obs[i, 3] <- mat_obs[i - 1, 2]
  }
}

mat_obs <- na.omit(mat_obs)


#model2: multivar with lags
B2 <- matrix(list(1.05, 1, "b2", 0), 2, 2)
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


France_KF4exp <- exp(France_KF4[,3])
ggplot2::autoplot(fit, plot.type = "fitted.xtT")

France_KF4_trend <- France_logPIB [2:205,1] - France_KF4[1:204,3]
plot (France_KF4_trend)
plot (France_logPIB)

