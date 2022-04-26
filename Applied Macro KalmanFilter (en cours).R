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


#data inflation

urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_inflation.csv'
df_infla<-read.csv2(urlfile, header=TRUE)
USA_infla <-df_infla[32]
USA_inflats<-ts(data=USA_infla,start=(1970),end=(2021),frequency=4)



#model1: univar, avec inflation
B1 <- matrix("b")
Z1 <- matrix(1)
A1 <- matrix("a")
u1 <- "zero"
D1 <- t(matrix(data = USA_inflats))
x0 <- 28
model.list1 <- list(B = B1, Z = Z1, A = A1, d=D1, R=matrix(1), V0="identity", tinitx = 1)
fit <- MARSS(USAts, model=model.list1, fit = TRUE)
USA_KF1 <- fitted(fit, type="ytt1", interval = c("none", "confidence", "prediction"),level = 0.95, output = c("data.frame", "matrix"))






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

#exemple2
set.seed(123)
u <- 0.01
r <- 0.02
q <- 0.1
b <- 0.9
TT <- 208
x0 <- 28
xt.ns <- rep(x0, TT)
for (i in 2:TT) xt.ns[i] <- b * xt.ns[i - 1] + u + rnorm(1, 0, 
                                                         sqrt(q))
yt.ns <- xt.ns + rnorm(TT, 0, sqrt(r))

plot(yt.ns, xlab = "", ylab = "", main = "xt and yt", pch = 16, 
     col = "red")
lines(xt.ns, lwd = 2)

fit <- MARSS(yt.ns, model = list(B = matrix("b")))
