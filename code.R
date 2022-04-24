rm(list = ls())
graphics.off()

#install.packages("readxl")
#install.packages("mfilter")
library(RCurl)
library(mFilter)
library(tidyverse)

urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_pib_volume.csv'
dsin<-read.csv2(urlfile, header=TRUE)
myts<-ts(data=dsin,start=(1970),end=(2022),frequency=4)




#Frequence du parametre HP smoother





France<-dsin[[6]]
Francets<-ts(data=France,start=(1970),end=(2021),frequency=4)


#hp filter
France_hp<- hpfilter(Francets, freq=100,type="frequency",drift=TRUE)
#plot(France_hp)

cycle_France_hp<-France_hp$cycle
trend_France_hp<-France_hp$trend
francetss_hp<-France_hp$x

#plot(France_hp)
ts.plot(francetss_hp, trend_France_hp, gpars = list(col = c("black", "red")))



# Baxter-King filter (Band pass filter)

France_bk <- bkfilter(Francets,pl = 6, pu = 32)
#plot(France_bk)

cycle_France_bk<-France_bk$cycle
trend_France_bk<-France_bk$trend
francetss_bk<-France_bk$x

ts.plot(francetss_bk, trend_France_bk, gpars = list(col = c("black", "red")))


#Detrend data with a linear filter

lin.mod <- lm(Francets ~ time(Francets))
lin.trend <- lin.mod$fitted.values  # fitted values pertain to time trend
linear <- ts(lin.trend, start = c(1970, 1), frequency = 4)  # create a time series variable for trend
lin.cycle <- Francets - linear  # cycle is the difference between the data and linear trend

ts.plot(linear, Francets, gpars = list(col = c("black", "red")))







Suede<-dsin[[7]]
Suedets<-ts(data=Suede,start=(1970),end=(2021),frequency=4)
Suede_hp<- hpfilter(Suedets, freq=100,type="frequency",drift=TRUE)
devAskNewPage(ask = FALSE)
plot(Suede_hp)



USA<-dsin[[33]]
USA <- na.omit(USA) 

USAts<-ts(data=USA,start=(1970),end=(2021),frequency=4)


USA_hp<- hpfilter(USAts, freq=100,type="frequency",drift=TRUE)

plot(USA_hp)



Suisse<-dsin[[10]]
Suisse <- na.omit(Suisse) 

Suissets<-ts(data=Suisse,start=1980,end=(2021),frequency=4)
Suisse_hp<- hpfilter(Suissets, freq=100,type="frequency",drift=TRUE)
plot(Suisse_hp)






rm(list = ls())









# Kalman filter procedure

#install.packages("MARSS")
library(MARSS)
