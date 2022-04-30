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
mygdpts<-ts(data=gdp,start=(1975),end=(2022),frequency=4)


#Frequence du parametre HP smoother

France<-gdp[[2]]
France <- na.omit(France) 
logFrance<-log(France)

Francets<-ts(data=logFrance,start=(1975),end=(2022),frequency=4)     



France_hp <- hpfilter(logFrance, freq = 1600, type = "lambda",drift=FALSE)



cycle_France_hp<-France_hp$cycle
trend_France_hp<-France_hp$trend


diff_lgdp<-diff(logFrance)*100


urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_inflation.csv'
df_infla<-read.csv2(urlfile, header=TRUE)
France_infla <-df_infla[2]
France_inflats<-ts(data=France_infla,start=(1975),end=(2022),frequency=4)
France_infla <- matrix(data = France_inflats)





#1ere equation Trend

Trend<-lm(diff_lgdp~1+offset(diff(cycle_France_hp)))
summary(Trend)

Delta1_France <- Trend$coefficients


#3 eme equation d'etat (Cycle)

cycle<-lm(cycle_France_hp~0+lag(cycle_France_hp,1)+lag(cycle_France_hp,2))
summary(cycle)

B_France <- cycle$coefficients
b1_France <- B_France[1]
b2_France <- B_France[2]


#2eme equation

trend_France_hp_reg<-trend_France_hp[-1,]

France_infla_reg<-France_infla[-1,]
France_infla_reg<-na.omit(France_infla_reg)


lag_France_infla<-lag(France_infla,1)

lag_France_infla_reg<-lag_France_infla[-1,]
lag_France_infla_reg<-lag_France_infla[-2,]

lag_France_infla_reg<-na.omit(lag_France_infla_reg)
France_infla_reg<-France_infla[-1,]


France_infla_reg <- na.omit(France_infla_reg) 
France_infla<- na.omit(France_infla) 

inflation_markup_model<-lm(France_infla_reg~lag(France_infla_reg,1)+trend_France_hp_reg)
summary(inflation_markup_model)

Alphas_France <-inflation_markup_model$coefficients
Alpha1_France <- Alphas_France[1]
Alpha2_France <- Alphas_France[2]
Alpha3_France <- Alphas_France[3]











