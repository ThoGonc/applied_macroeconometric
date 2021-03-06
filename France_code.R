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
France<-dsin[[6]]
France <- na.omit(France) 
Francets<-ts(data=France,start=(1970),end=(2021),frequency=4)
France_hp<- hpfilter(Francets, freq=1600,type="lambda",drift=FALSE)
plot(France_hp)

#install.packages("MARSS")
#install.packages("dlm")
#installed.packages("broom")
library(MARSS)
library(dlm)

#initial value pot_France
France_hp_cycle <- France_hp[["cycle"]]
pot_France_begin <- log(France_hp_cycle[[1]])
pot_France_begint1 <- log(France_hp_cycle[[2]])


#data inflation Q1_1970 Q4_2020 (205 values)

urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_inflation.csv'
df_infla<-read.csv2(urlfile, header=TRUE)
France_infla <-df_infla[6]
France_inflats<-ts(data=France_infla,start=(1970),end=(2021),frequency=4)
France_infla <- matrix(data = France_inflats)

#data PIB Q1_1970 Q4_2020 (205 values)
France_PIB <- matrix(data = Francets)
France_logPIB <- log(France_PIB)

#pr�paration matrice var d'observation
mat_obs <- matrix(, nrow = 205, ncol = 3)
mat_obs[,1] <- France_logPIB 
mat_obs[,2] <- France_infla





#cr�ation du lag inflation et delta PIB
for (i in seq_along(France_infla)) {
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
D1 <- t(matrix(data = France_inflats))
x0 <- 28
model.list1 <- list(B = B1, Z = Z1, A = A1, d=D1, R=matrix(1), V0="identity", tinitx = 1)
fit <- MARSS(Francets, model=model.list1, fit = TRUE, fun.kf = ("MARSSkfss"))
France_KF2 <- fitted(fit, type="ytt1", interval = c("none", "confidence", "prediction"),level = 0.95, output = c("data.frame", "matrix"))
summary(fit)
ggplot2::autoplot(fit, plot.type = "fitted.ytT")

#model2: multivar with lags
B2 <- matrix(list("b1", 1, "b2", 0), 2, 2)
Z2 <- matrix(list(1, "alpha3", 0, -1, 0, 0), 3, 2)
A2 <- matrix(list("delta", "alpha1", 0), 3, 1)
Q2 <- matrix(list("q1", 0, 0, 0), 2, 2)
u2 <- matrix(list(0,0),nrow = 2, ncol = 1)
d2 <- t(mat_obs)
D2 <- matrix(list (0, 0, 0, 0, 0, 0, 0, "alpha2", 1), 3, 3)
R2 <- matrix(list ("r11", 0, 0, 0, "r22", 0, 0, 0, 1), 3, 3)
x02 <- matrix(list(pot_France_begint1, pot_France_begin), nrow = 2, ncol = 1)
model.list2 <- list(B = B2, Q=Q2, Z = Z2, A = A2, d=d2, D=D2, U=u2, R=R2, x0= x02, tinitx = 1)
fit <- MARSS(d2, model=model.list2, fit = TRUE)
France_KF3 <- fitted(fit, type="ytt1", interval = c("none", "confidence", "prediction"),level = 0.95, output = c("data.frame", "matrix"))
France_KF4 <- tsSmooth(fit,
                    type = c("xtT", "xtt", "xtt1", "ytT", "ytt", "ytt1"),
                    interval = c("none", "confidence", "prediction"),
                    level = 0.95, fun.kf = c("MARSSkfas", "MARSSkfss"))

#exploitation des resultats
mat_OG <- matrix(, nrow = 204, ncol = 1)
for (i in 1:204) {
  if (i == 1) {
    mat_OG[i, 1] <- NA_real_
  } else {
    mat_OG[i, 1] <- France_KF4[i,3]- France_KF4[i-1,3]
  }
}


France_KF4exp <- exp(France_KF4[,3])
ggplot2::autoplot(fit, plot.type = "fitted.xtT")



