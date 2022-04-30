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



Germany<-gdp[[3]]

logGermany<-log(Germany)
Germanyts<-ts(data=logGermany,start=(1975),end=(2022),frequency=4)     

Germanyts<- na.omit(Germanyts) 
plot(Germanyts)


Germany_hp <- hpfilter(Germanyts, freq = 1600, type = "lambda",drift=FALSE)


#Plot time series
plot.ts(Germanyts, ylab = "")

# include HP trend
lines(Germany_hp$trend, col = "red")
legend("topleft", legend = c("Log GDP Germany", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")

#Plot cycle
plot.ts(Germany_hp$cycle, ylab = "")  
legend("topleft", legend = c("HP cycle Germany"), lty = 1, col = c("black"), 
       bty = "n")





Italia<-gdp[[4]]

logItalia<-log(Italia)
Italiats<-ts(data=logItalia,start=(1975),end=(2022),frequency=4)     

Italiats<- na.omit(Italiats) 
plot(Italiats)


Italia_hp <- hpfilter(Italiats, freq = 1600, type = "lambda",drift=FALSE)

#Plot time series
plot.ts(Italiats, ylab = "")  

# include HP trend
lines(Italia_hp$trend, col = "red")  
legend("topleft", legend = c("Log GDP Italia", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")

#Plot cycle
plot.ts(Italia_hp$cycle, ylab = "")  
legend("topleft", legend = c("HP cycle Italia"), lty = 1, col = c("black"), 
       bty = "n")




Spain<-gdp[[5]]

logSpain<-log(Spain)
Spaints<-ts(data=logSpain,start=(1975),end=(2022),frequency=4)     

Spaints<- na.omit(Spaints) 
plot(Spaints)


Spain_hp <- hpfilter(Spaints, freq = 1600, type = "lambda",drift=FALSE)


#Plot time series
plot.ts(Spaints, ylab = "")  

# include HP trend
lines(Spain_hp$trend, col = "red")  
legend("topleft", legend = c("Log GDP Spain", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")

#Plot cycle
plot.ts(Spain_hp$cycle, ylab = "")  
legend("topleft", legend = c("HP cycle Spain"), lty = 1, col = c("black"), 
       bty = "n")




Japan<-gdp[[6]]

logJapan<-log(Japan)
Japants<-ts(data=logJapan,start=(1975),end=(2022),frequency=4)     

Japants<- na.omit(Japants) 
plot(Japants)


Japan_hp <- hpfilter(Japants, freq = 1600, type = "lambda",drift=FALSE)


#Plot time series
plot.ts(Japants, ylab = "")  

# include HP trend
lines(Japan_hp$trend, col = "red")
legend("topleft", legend = c("Log GDP Japan", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")

#Plot cycle
plot.ts(Japan_hp$cycle, ylab = "")  
legend("topleft", legend = c("HP cycle Japan"), lty = 1, col = c("black"), 
       bty = "n")



United_Kingdom<-gdp[[7]]

logUnited_Kingdom<-log(United_Kingdom)
United_Kingdomts<-ts(data=logUnited_Kingdom,start=(1975),end=(2022),frequency=4)     

United_Kingdomts<- na.omit(United_Kingdomts) 
plot(United_Kingdomts)


United_Kingdom_hp <- hpfilter(United_Kingdomts, freq = 1600, type = "lambda",drift=FALSE)


#Plot time series
plot.ts(United_Kingdomts, ylab = "")
# include HP trend
lines(United_Kingdom_hp$trend, col = "red")
legend("topleft", legend = c("Log GDP United Kingdom", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")

#Plot cycle
plot.ts(United_Kingdom_hp$cycle, ylab = "")
legend("topleft", legend = c("HP cycle United Kingdom"), lty = 1, col = c("black"), 
       bty = "n")



United_States<-gdp[[8]]

logUnited_States<-log(United_States)
United_Statests<-ts(data=logUnited_States,start=(1975),end=(2022),frequency=4)     

United_Statests<- na.omit(United_Statests) 
plot(United_Statests)


United_States_hp <- hpfilter(United_Statests, freq = 1600, type = "lambda",drift=FALSE)


#Plot time series
plot.ts(United_Statests, ylab = "")  

# include HP trend
lines(United_States_hp$trend, col = "red")  
legend("topleft", legend = c("Log GDP United States", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")

#Plot cycle
plot.ts(United_States_hp$cycle, ylab = "")  
legend("topleft", legend = c("HP cycle USA"), lty = 1, col = c("black"), 
       bty = "n")









logFrance<-na.omit(logFrance)
France_hp_reg <- hpfilter(logFrance, freq = 1600, type = "lambda",drift=FALSE)

cycle_France_hp<-France_hp_reg$cycle
trend_France_hp<-France_hp_reg$trend

diff_lgdp<-diff(logFrance)*100


urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_inflation.csv'
df_infla<-read.csv2(urlfile, header=TRUE)
France_infla <-df_infla[2]
France_inflats<-ts(data=France_infla,start=(1975),end=(2022),frequency=4)
France_infla <- matrix(data = France_inflats)


#Coefficients 


#1ere Trend
Trend<-lm(diff_lgdp~1+offset(diff(cycle_France_hp)))
summary(Trend)


#3 eme equation Cycle

cycle<-lm(cycle_France_hp~0+lag(cycle_France_hp,1)+lag(cycle_France_hp,2))
summary(cycle)



trend_France_hp<-trend_France_hp[-188,]
trend_France_hp<-na.omit(trend_France_hp)


France_infla_reg<-France_infla[-1,]
France_infla_reg<-na.omit(France_infla_reg)



lag_France_infla<-lag(France_infla,1)

lag_France_infla_reg<-lag_France_infla[-1,]
lag_France_infla_reg<-lag_France_infla[-2,]

lag_France_infla_reg<-na.omit(lag_France_infla_reg)
France_infla_reg<-France_infla[-1,]




France_infla_reg <- na.omit(France_infla_reg) 
France_infla<- na.omit(France_infla) 

inflation_markup_model<-lm(France_infla_reg~lag(France_infla_reg,1)+trend_France_hp)
summary(inflation_markup_model)




























































































































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
linear <- ts(lin.trend, start = c(1975, 1), lambda = 4)  # create a time series variable for trend
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
