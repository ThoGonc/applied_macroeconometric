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








logFrance<-na.omit(logFrance)
France_hp_reg <- hpfilter(logFrance, freq = 1600, type = "lambda",drift=FALSE)

cycle_France_hp<-France_hp_reg$cycle
trend_France_hp<-France_hp_reg$trend

cycle_France_hpts<-ts(data=cycle_France_hp,start=(1975),end=(2022),frequency=4)
trend_France_hpts<-ts(data=trend_France_hp,start=(1975),end=(2022),frequency=4)



diff_lgdp<-diff(logFrance)*100


urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_inflation.csv'
df_infla<-read.csv2(urlfile, header=TRUE)
France_infla <-df_infla[2]
France_inflats<-ts(data=France_infla,start=(1975),end=(2022),frequency=4)
France_infla <- matrix(data = France_inflats)






#Détermination des coefficients pour le modèle espace-etat 

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

France_infla_reg <- na.omit(France_infla_reg) 
France_infla<- na.omit(France_infla) 

inflation_markup_model<-lm(France_infla_reg~lag(France_infla_reg,1)+trend_France_hp)
summary(inflation_markup_model)




#Nom des coefficients

#equation d'etat
B_France <- cycle$coefficients
b1_France <- B_France[1]
b2_France <- B_France[2]

#equations d'observation
Delta1_France <- Trend$coefficients
Alphas_France <-inflation_markup_model$coefficients
Alpha1_France <- Alphas_France[1]
Alpha2_France <- Alphas_France[2]
Alpha3_France <- Alphas_France[3]

#valeurs initiales
OGbegint0 <- cycle_France_hp[[3]]
OGbegint_moins1 <- cycle_France_hp[[2]]


#préparation matrice variables d'observation du modèle espace etat
mat_obs <- matrix(, nrow = 187, ncol = 3)
mat_obs[,1] <- diff_lgdp
mat_obs[,2] <- France_infla_reg
mat_obs[,3] <- lag(France_infla_reg,1)
mat_obs <- na.omit(mat_obs)



#modele espace-etat multivar avec lags
B2 <- matrix(list(b1_France, 1, b2_France, 0), 2, 2)
Z2 <- matrix(list(1, Alpha3_France, 0, -1, 0, 0), nrow=3, ncol=2)
A2 <- matrix(list(Delta1_France, Alpha1_France, 0), nrow=3, ncol=1)
Q2 <- matrix(list("q1", 0, 0, 0), 2, 2)
u2 <- matrix(list(0,0),nrow = 2, ncol = 1)
d2 <- t(mat_obs)
D2 <- matrix(list (0, 0, 0, 0, 0, 0, 0, Alpha2_France, 1), 3, 3)
R2 <- matrix(list ("r11", 0, 0, 0, "r22", 0, 0, 0, 0.01), 3, 3)
x02 <- matrix(list(OGbegint0, OGbegint_moins1), nrow = 2, ncol = 1)
model.list2 <- list(B = B2, Q=Q2, Z = Z2, A = A2, d=d2, D=D2, U=u2, R=R2, x0= x02, tinitx = 1)
fit <- MARSS(d2, model=model.list2, fit = TRUE)
France_KF3 <- fitted(fit, type="ytT", interval = c("confidence"),level = 0.95, output = c("data.frame", "matrix"))
France_KF4 <- tsSmooth(fit,
                       type = c("xtT", "xtt", "xtt1", "ytT", "ytt", "ytt1"),
                       interval = c("confidence"),
                       level = 0.95, fun.kf = c("MARSSkfas"))

cycle_KF_OUTPUTGAP_France <-ts(data=France_KF4$.estimate,start=(1975),end=(2022),frequency=4)
plot(cycle_KF_OUTPUTGAP_France)

PIB_POTENTIEL_KF_France <- logFrance - France_KF4$.estimate[1:188]/100
plot(PIB_POTENTIEL_KF_France)
plot(logFrance)


# Plot time series
plot.ts(PIB_POTENTIEL_KF_France, ylab = "")  

# include HP trend
lines(logFrance, col = "red")
lines(trend_France_hp, col = "blue")
legend("topleft", legend = c("PIB_Potentiel Kalman Filter France", "Log France", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")



view(cycle_France_hpts)
view(cycle_KF_OUTPUTGAP_France)
view(trend_France_hpts)
view(PIB_POTENTIEL_KF_France)
view(logFrance)















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










logFrance<-na.omit(logFrance)
France_hp_reg <- hpfilter(logFrance, freq = 1600, type = "lambda",drift=FALSE)

cycle_France_hp<-France_hp_reg$cycle
trend_France_hp<-France_hp_reg$trend

cycle_France_hpts<-ts(data=cycle_France_hp,start=(1975),end=(2022),frequency=4)
trend_France_hpts<-ts(data=trend_France_hp,start=(1975),end=(2022),frequency=4)



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







France_infla_reg <- na.omit(France_infla_reg) 
France_infla<- na.omit(France_infla) 

inflation_markup_model<-lm(France_infla_reg~lag(France_infla_reg,1)+trend_France_hp)
summary(inflation_markup_model)









Delta1_France <- Trend$coefficients

B_France <- cycle$coefficients
b1_France <- B_France[1]
b2_France <- B_France[2]

Alphas_France <-inflation_markup_model$coefficients
Alpha1_France <- Alphas_France[1]
Alpha2_France <- Alphas_France[2]
Alpha3_France <- Alphas_France[3]



#préparation matrice var d'observation
mat_obs <- matrix(, nrow = 187, ncol = 3)
mat_obs[,1] <- diff_lgdp
mat_obs[,2] <- France_infla_reg
mat_obs[,3] <- lag(France_infla_reg,1)
mat_obs <- na.omit(mat_obs)



#model3: multivar with lags fixed coef from Eviews
B2 <- matrix(list(b1, 1, b2, 0), 2, 2)
Z2 <- matrix(list(1, alpha3, 0, -1, 0, 0), nrow=3, ncol=2)
A2 <- matrix(list(delta, Alpha1_France, 0), nrow=3, ncol=1)
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




cycleKF_OUTPUTGAP_France <- cycleKF
plot(cycleKF_OUTPUTGAP_France)

PIB_POTENTIEL_KF_France <- logFrance - France_KF4$.estimate[1:188]/100
plot(PIB_POTENTIEL_KF_France)
plot(logFrance)


# Plot time series
plot.ts(PIB_POTENTIEL_KF_France, ylab = "")  

# include HP trend
lines(logFrance, col = "red")
lines(trend_France_hp, col = "blue")
legend("topleft", legend = c("PIB_Potentiel Kalman Filter France", "Log France", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")




























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
