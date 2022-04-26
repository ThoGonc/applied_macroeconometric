rm(list = ls())
graphics.off()

#install.packages("readxl")
#install.packages("mfilter")
library(RCurl)
library(mFilter)
library(tidyverse)

#gdp annuel
urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_gdp_quarter_sa.csv'
gdp<-read.csv2(urlfile, header=TRUE)
mygdpts<-ts(data=gdp,start=(1970),end=(2022),frequency=4)

#gdp trimestriel
#https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_gdp_quarter.csv

#gdp_trimestriel_reel_US_FRED no seasonnaly adjust
#https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_real_gdp_quarter_nosa.csv


#gdp annuel
#https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied.csv

#gdp_trimestriel_reel_US_FRED no seasonnaly adjust
#https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_gdp_quarter_sa.csv



#Frequence du parametre HP smoother

France<-gdp[[6]]

logFrance<-log(France)
#Franced<-diff(France)
#Francelgdp<-log(France)
#Francedlgdp<-100*diff(Francelgdp)
Francets<-ts(data=France,start=(1975),end=(2021),frequency=4)     

plot(Francets)


hp.decom <- hpfilter(Francets, freq = 1600, type = "lambda")

plot.ts(Francets, ylab = "")  # plot time series
lines(hp.decom$trend, col = "red")  # include HP trend
legend("topleft", legend = c("data", "HPtrend"), lty = 1, 
       col = c("black", "red"), bty = "n")
plot.ts(hp.decom$cycle, ylab = "")  # plot cycle
legend("topleft", legend = c("HPcycle"), lty = 1, col = c("black"), 
       bty = "n")



#verif pourquoi on a une data en 2022Q1 sans chiffres dans la base




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
linear <- ts(lin.trend, start = c(1970, 1), lambda = 4)  # create a time series variable for trend
lin.cycle <- Francets - linear  # cycle is the difference between the data and linear trend

ts.plot(linear, Francets, gpars = list(col = c("black", "red")))







Suede<-dsin[[7]]
Suedets<-ts(data=Suede,start=(1970),end=(2021),lambda=4)
Suede_hp<- hpfilter(Suedets, freq=100,type="lambda",drift=TRUE)
devAskNewPage(ask = FALSE)
plot(Suede_hp)



USA<-dsin[[33]]
USA <- na.omit(USA) 

USAts<-ts(data=USA,start=(1970),end=(2021),lambda=4)


USA_hp<- hpfilter(USAts, freq=100,type="lambda",drift=TRUE)

plot(USA_hp)



Suisse<-dsin[[10]]
Suisse <- na.omit(Suisse) 

Suissets<-ts(data=Suisse,start=1980,end=(2021),lambda=4)
Suisse_hp<- hpfilter(Suissets, freq=100,type="lambda",drift=TRUE)
plot(Suisse_hp)






rm(list = ls())









# Kalman filter procedure

#install.packages("MARSS")
library(MARSS)
