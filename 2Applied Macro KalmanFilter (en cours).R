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
USAts<-ts(data=USA,start=(1970),end=(2021),frequency=4)
USA_hp<- hpfilter(USAts, freq=100,type="frequency",drift=TRUE)
plot(USA_hp)

PaysBas<-dsin[[34]]
PaysBas <- na.omit(PaysBas) 
PaysBasts<-ts(data=PaysBas,start=(1970),end=(2021),frequency=4)
PaysBas_hp<- hpfilter(PaysBasts, freq=100,type="frequency",drift=TRUE)


#install.packages("MARSS")
#install.packages("dlm")
library(MARSS)
library(dlm)

#initial value pot_USA
USA_hp_trend <- USA_hp[["trend"]]
pot_USA_begin <- USA_hp_trend[[1]]


#data inflation Q1_1970 Q4_2020 (205 values)

urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_inflation.csv'
df_infla<-read.csv2(urlfile, header=TRUE)
USA_infla <-df_infla[32]
USA_inflats<-ts(data=USA_infla,start=(1970),end=(2021),frequency=4)
USA_infla <- matrix(data = USA_inflats)

#data PIB Q1_1970 Q4_2020 (205 values)
USA_PIB <- matrix(data = USAts)
PaysBas_PIB <- matrix(data = PaysBasts)


#préparation matrice var d'observation
mat_obs <- matrix(, nrow = 205, ncol = 3)
mat_obs[,1] <- USA_infla
mat_obs[,2] <- USA_PIB
mat_obs[,3] <- PaysBas_PIB



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
B2 <- matrix(list("b1", 0, "b2", 1), 2, 2)
Z2 <- matrix(list(1, "alpha3", 0, -1, 0, 0), 3, 2)
A2 <- matrix(list("delta", "alpha1", 0), 3, 1)
Q2 <- matrix(list("q1", 0, 0, 0), 2, 2)
u2 <- "zero"
d2 <- t(mat_obs)
D2 <- matrix(list (0, 0, 0, 0, "alpha2", 0, 0, "alpha3", 1), 3, 3)
x02 <- 28
model.list2 <- list(B = B2, Z = Z2, A = A2, d=d2, D=D2, R="identity", V0="identity", tinitx = 1)
fit <- MARSS(d2, model=model.list2, fit = TRUE)
USA_KF3 <- fitted(fit, type="ytt1", interval = c("none", "confidence", "prediction"),level = 0.95, output = c("data.frame", "matrix"))







<<<<<<< Updated upstream
#model 3
=======
#model2

>>>>>>> Stashed changes
#model specification
TUCref <- 25
B1 <- matrix(list("a", 0, 0, "b"), 2, 1)
Z1 <- matrix(list(1, 0, 0, 1, "c", "d"), 3, 2)
C <- matrix(list(0, 0, 0, -1, 0, - "d"), 3, 2)
D <- matrix(list(0, "TUCref", 100), 3, 1)
U1 <- matrix(list(0, 0), 2, 1)
Q1 <- matrix(list("q111", "q112", "q112", "q122"), 2, 2)
U2 <- matrix(list(0, 0, 0), 3, 1)
Q2 <- matrix(list(0, 0, 0, 0, "q222", "q223", 0, "q223", "q233"), 3, 3)
X0 <- matrix(list(28.07, 0.03), 2, 1)
Q0 <- diag(1, 2)
model.list <- list(B = B1, U = U1, Q = Q1, Z = Z1, A = U2, R = Q2, 
                   x0 = X0, V0 = Q0, tinitx = 0)

#data 
z <- USA
dat <- data.frame(Yr = floor(time(z) + .Machine$double.eps), 
                  Qtr = cycle(z), Temp=z)
dat <- t(dat)
class(dat)

fit <- MARSS(dat, model=model.list)

#test1
fit <- MARSS(USAts)
USA_KF1 <- head(fitted(fit, type="ytt1"))

#example
dat0 <- cumsum(rnorm(100,0,0.5)) + rnorm(100,0,0.5)
fit <- MARSS(USAts)
