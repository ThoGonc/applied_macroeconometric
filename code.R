rm(list = ls())
graphics.off()

#install.packages("readxl")
#install.packages("mfilter")
install.packages("Metrics")
library(RCurl)
library(mFilter)
library(tidyverse)
library(MARSS)
library(Metrics)

#gdp annuel
urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_gdp_quarter_sa.csv'
gdp<-read.csv2(urlfile, header=TRUE)

urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_inflation.csv'
df_infla<-read.csv2(urlfile, header=TRUE)



###France

#Frequence du parametre HP smoother

France<-gdp[[2]]
logFrance<-log(France)

France<-na.omit(France)

Francets<-ts(data=logFrance,start=(1975),frequency=4)     

Francets<- na.omit(Francets) 
plot(Francets)


France_hp <- hpfilter(Francets, freq = 1600, type = "lambda",drift=FALSE)



# Plot time series
plot.ts(Francets, ylab = "")  

# include HP trend
lines(France_hp$trend, col = "red")
legend("topleft", legend = c("Log GDP France", "HP trend"), lty = 1, 
       fill = c("black", "red"), bty = "n")

#Plot cycle
plot.ts(France_hp$cycle, ylab = "") 
legend("topleft", legend = c("HP cycle France"), lty = 1, fill = c("black"), 
       bty = "n")








logFrance<-na.omit(logFrance)
France_hp_reg <- hpfilter(logFrance, freq = 1600, type = "lambda",drift=FALSE)

cycle_France_hp<-France_hp_reg$cycle
trend_France_hp<-France_hp_reg$trend

cycle_France_hpts<-ts(data=cycle_France_hp,start=(1975),frequency=4)
trend_France_hpts<-ts(data=trend_France_hp,start=(1975),frequency=4)



diff_lgdp_France<-diff(logFrance)*100

France_infla <-df_infla[2]
France_inflats<-ts(data=France_infla,start=(1975),frequency=4)
France_infla <- matrix(data = France_inflats)






#Détermination des coefficients pour le modèle espace-etat 

#1ere Trend
Trend_France<-lm(diff_lgdp_France~1+offset(diff(cycle_France_hp)))
summary(Trend_France)


#3 eme equation Cycle

cycle_France<-lm(cycle_France_hp~0+lag(cycle_France_hp,1)+lag(cycle_France_hp,2))
summary(cycle_France)

trend_France_hp<-trend_France_hp[-188,]
trend_France_hp<-na.omit(trend_France_hp)

France_infla_reg<-France_infla[-1,]
France_infla_reg<-na.omit(France_infla_reg)

France_infla_reg <- na.omit(France_infla_reg) 
France_infla<- na.omit(France_infla) 

inflation_markup_model_France<-lm(France_infla_reg~lag(France_infla_reg,1)+trend_France_hp)
summary(inflation_markup_model_France)




#Nom des coefficients

#equation d'etat
B_France <- cycle_France$coefficients
b1_France <- B_France[1]
b2_France <- B_France[2]

#equations d'observation
Delta1_France <- Trend_France$coefficients
Alphas_France <-inflation_markup_model_France$coefficients
Alpha1_France <- Alphas_France[1]
Alpha2_France <- Alphas_France[2] 
Alpha3_France <- Alphas_France[3]

#valeurs initiales
OGbegint0_France <- cycle_France_hp[[3]]
OGbegint_moins1_France <- cycle_France_hp[[2]]


#préparation matrice variables d'observation du modèle espace etat
mat_obs_France <- matrix(, nrow = 187, ncol = 3)
mat_obs_France[,1] <- diff_lgdp_France
mat_obs_France[,2] <- France_infla_reg
mat_obs_France[,3] <- lag(France_infla_reg,1)
mat_obs_France <- na.omit(mat_obs_France)



#modele espace-etat multivar avec lags
B2_France <- matrix(list(b1_France, 1, b2_France, 0), 2, 2)
Z2_France <- matrix(list(1, Alpha3_France, 0, -1, 0, 0), nrow=3, ncol=2)
A2_France <- matrix(list(Delta1_France, Alpha1_France, 0), nrow=3, ncol=1)
Q2_France <- matrix(list("q1", 0, 0, 0), 2, 2)
u2_France <- matrix(list(0,0),nrow = 2, ncol = 1)
d2_France <- t(mat_obs_France)
D2_France <- matrix(list (0, 0, 0, 0, 0, 0, 0, Alpha2_France, 1), 3, 3)
R2_France <- matrix(list ("r11", 0, 0, 0, "r22", 0, 0, 0, 0.01), 3, 3)
x02_France <- matrix(list(OGbegint0_France, OGbegint_moins1_France), nrow = 2, ncol = 1)
model.list2_France <- list(B = B2_France, Q=Q2_France, Z = Z2_France, A = A2_France, d=d2_France, D=D2_France, U=u2_France, R=R2_France, x0= x02_France, tinitx = 1)
fit <- MARSS(d2_France, model=model.list2_France, fit = TRUE)
France_KF3 <- fitted(fit, type="ytT", interval = c("confidence"),level = 0.95, output = c("data.frame", "matrix"))
France_KF4 <- tsSmooth(fit,
                       type = c("xtt"),
                       interval = c("confidence"),
                       level = 0.95, fun.kf = c("MARSSkfas"))



PIB_POTENTIEL_KF_France <- logFrance - France_KF4$.estimate[1:188]/100
PIB_POTENTIEL_KF_Francets <- ts(PIB_POTENTIEL_KF_France, start = c(1975, 1), frequency = 4)

plot(PIB_POTENTIEL_KF_France)
plot(logFrance)


# Plot time series
plot.ts(PIB_POTENTIEL_KF_Francets, ylab = "",col="black")  

# include HP trend
lines(Francets, col = "red")
lines(trend_France_hpts, col = "blue")
legend("topleft", legend = c("PIB_Potentiel Kalman Filter France", "Log France", "HP trend"), lty = 1, 
       fill = c("black", "red","blue"), bty = "n")



#Detrend data with a linear filter

lin.mod_France <- lm(Francets ~ time(Francets))
lin.trend_France <- lin.mod_France$fitted.values  # fitted values pertain to time trend
linear_France <- ts(lin.trend_France, start = c(1975, 1), frequency = 4)  # create a time series variable for trend
lin.cycle_France <- Francets - linear_France  # cycle is the difference between the data and linear trend

ts.plot(linear_France, Francets, gpars = list(col = c("black", "red")))


# Baxter-King filter (Band pass filter)

France_bk <- bkfilter(Francets,pl = 6, pu = 32)

cycle_France_bk<-France_bk$cycle
trend_France_bk<-France_bk$trend
francetss_bk<-France_bk$x

ts.plot(francetss_bk, trend_France_bk, gpars = list(col = c("black", "red")))


linear_trend_France<-ts(linear_France, start = c(1975, 1), frequency = 4)
trend_France_bkts<-ts(trend_France_bk, start = c(1975, 1), frequency = 4)

# Plot time series
plot.ts(PIB_POTENTIEL_KF_Francets, ylab = "",col="black")  

# include HP trend
lines(Francets, col = "red")
lines(trend_France_hpts, col = "blue")
lines(linear_trend_France, col = "green")
lines(trend_France_bkts, col = "orange")
legend("topleft", legend = c("GDP potential Kalman Filter France", "Log GDP France", "HP France trend","Linear France trend","Baxter-King France trend" ), lty = 1, 
       fill = c("black", "red","blue","green","orange"), bty = "n")




observed_France<-Francets
predicted_France_Kalman<-PIB_POTENTIEL_KF_Francets
predicted_France_HP<-trend_France_hpts
predicted_France_Linear<-linear_trend_France
predicted_France_Baxter_King<-trend_France_bkts



mae_Kalman_France<- mae(observed_France,predicted_France_Kalman)
mae_Kalman_France
mae_HP_France<-mae(observed_France,predicted_France_HP)
mae_HP_France
mae_Linear_France<-mae(observed_France,predicted_France_Linear)
mae_Linear_France
mae_Baxter_King_France<-mae(observed_France,predicted_France_Baxter_King)
mae_Baxter_King_France




diff_lgdp_France_ts<-ts(diff_lgdp_France/100, end = c(2022, 1), frequency = 4)
cycle_France_bk_ts<-ts(cycle_France_bk, end = c(2022, 1), frequency = 4)
cycle_France_linear_ts<-ts(lin.cycle_France, start = c(1975, 1), frequency = 4)
cycle_France_Kalman_a <- logFrance - PIB_POTENTIEL_KF_France
cycle_France_Kalman_ts<-ts(cycle_France_Kalman_a, end = c(2022, 1), frequency = 4)


# Plot time series
plot.ts(diff_lgdp_France_ts, ylab = "",col="black")  

# include HP trend
lines(cycle_France_Kalman_ts, col = "red")
lines(cycle_France_hpts, col = "blue")
lines(cycle_France_linear_ts, col = "green")
lines(cycle_France_bk_ts, col = "orange")
legend("topleft", legend = c("Log Delta GDP France","Cycle France Kalman Filter GDP", "HP France Cycle","Linear France cycle","Baxter-King France cycle" ), lty = 1, 
       fill = c("black", "red","blue","green","orange"), bty = "n")


observed_France_cycle<-diff_lgdp_France_ts
predicted_France_Kalman_cycle<-cycle_France_Kalman_ts
predicted_France_HP_cycle<-cycle_France_hpts
predicted_France_Linear_cycle<-cycle_France_linear_ts
predicted_France_Baxter_King_cycle<-cycle_France_bk_ts



mae_Kalman_France_cycle<- mae(observed_France,predicted_France_Kalman_cycle)
mae_Kalman_France_cycle
mae_HP_France_cycle<-mae(observed_France,predicted_France_HP_cycle)
mae_HP_France_cycle
mae_Linear_France_cycle<-mae(observed_France,predicted_France_Linear_cycle)
mae_Linear_France_cycle
mae_Baxter_King_France<-mae(observed_France,predicted_France_Baxter_King_cycle)
mae_Baxter_King_France_cycle







###Germany

#Frequence du parametre HP smoother

Germany<-gdp[[3]]
Germany<-na.omit(Germany)

logGermany<-log(Germany)
Germanyts<-ts(data=logGermany,start=(1991),frequency=4)     

Germanyts<- na.omit(Germanyts) 
plot(Germanyts)


Germany_hp <- hpfilter(Germanyts, freq = 1600, type = "lambda",drift=FALSE)

# Plot time series
plot.ts(Germanyts, ylab = "")  

# include HP trend
lines(Germany_hp$trend, col = "red")
legend("topleft", legend = c("Log GDP Germany", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")

#Plot cycle
plot.ts(Germany_hp$cycle, ylab = "") 
legend("topleft", legend = c("HP cycle Germany"), lty = 1, col = c("black"), 
       bty = "n")








logGermany<-na.omit(logGermany)
Germany_hp_reg <- hpfilter(logGermany, freq = 1600, type = "lambda",drift=FALSE)

cycle_Germany_hp<-Germany_hp_reg$cycle
trend_Germany_hp<-Germany_hp_reg$trend

cycle_Germany_hpts<-ts(data=cycle_Germany_hp,start=(1991),frequency=4)
trend_Germany_hpts<-ts(data=trend_Germany_hp,start=(1991),frequency=4)

cycle_Germany_hpts<-na.omit(cycle_Germany_hpts)
trend_Germany_hpts<-na.omit(trend_Germany_hpts)



diff_lgdp_Germany<-diff(logGermany)*100


Germany_infla <-df_infla[3]
Germany_inflats<-ts(data=Germany_infla,start=(1975),end=(2022),frequency=4)

Germany_inflats<- na.omit(Germany_inflats) 
Germany_infla <- matrix(data = Germany_inflats)






#Détermination des coefficients pour le modèle espace-etat 

#1ere Trend
trend_Germany<-lm(diff_lgdp_Germany~1+offset(diff(cycle_Germany_hp)))
summary(trend_Germany)



#3 eme equation Cycle

cycle_Germany<-lm(cycle_Germany_hp~0+lag(cycle_Germany_hp,1)+lag(cycle_Germany_hp,2))
summary(cycle_Germany)










trend_Germany_hp<-trend_Germany_hp[-124,]
trend_Germany_hp<-na.omit(trend_Germany_hp)

Germany_infla_reg<-Germany_infla[-1,]
Germany_infla_reg<-na.omit(Germany_infla_reg)

Germany_infla_reg<-na.omit(Germany_infla_reg) 


inflation_markup_model_Germany<-lm(Germany_infla_reg~lag(Germany_infla_reg,1)+trend_Germany_hp)
summary(inflation_markup_model_Germany)




#Nom des coefficients

#equation d'etat
B_Germany <- cycle_Germany$coefficients
b1_Germany <- B_Germany[1]
b2_Germany <- B_Germany[2]

#equations d'observation
Delta1_Germany <- trend_Germany$coefficients
Alphas_Germany <-inflation_markup_model_Germany$coefficients
Alpha1_Germany <- Alphas_Germany[1] 
Alpha2_Germany <- Alphas_Germany[2]
Alpha3_Germany <- Alphas_Germany[3]

#valeurs initiales
OGbegint0_Germany <- cycle_Germany_hp[[3]]
OGbegint_moins1_Germany <- cycle_Germany_hp[[2]]


#préparation matrice variables d'observation du modèle espace etat

mat_obs_Germany <- matrix(, nrow = 123, ncol = 3)
mat_obs_Germany[,1] <- diff_lgdp_Germany
mat_obs_Germany[,2] <- Germany_infla_reg
mat_obs_Germany[,3] <- lag(Germany_infla_reg,1)
mat_obs_Germany <- na.omit(mat_obs_Germany)





#modele espace-etat multivar avec lags
B2_Germany <- matrix(list(b1_Germany, 1, b2_Germany, 0), 2, 2)
Z2_Germany <- matrix(list(1, Alpha3_Germany, 0, -1, 0, 0), nrow=3, ncol=2)
A2_Germany <- matrix(list(Delta1_Germany, Alpha1_Germany, 0), nrow=3, ncol=1)
Q2_Germany <- matrix(list("q1", 0, 0, 0), 2, 2)
u2_Germany <- matrix(list(0,0),nrow = 2, ncol = 1)
d2_Germany <- t(mat_obs_Germany)
D2_Germany <- matrix(list (0, 0, 0, 0, 0, 0, 0, Alpha2_Germany, 1), 3, 3)
R2_Germany <- matrix(list ("r11", 0, 0, 0, "r22", 0, 0, 0, 0.01), 3, 3)
x02_Germany <- matrix(list(OGbegint0_Germany, OGbegint_moins1_Germany), nrow = 2, ncol = 1)
model.list2_Germany <- list(B = B2_Germany, Q=Q2_Germany, Z = Z2_Germany, A = A2_Germany, d=d2_Germany, D=D2_Germany, U=u2_Germany, R=R2_Germany, x0= x02_Germany, tinitx = 1)
fit <- MARSS(d2_Germany, model=model.list2_Germany, fit = TRUE)
Germany_KF3 <- fitted(fit, type="ytT", interval = c("confidence"),level = 0.95, output = c("data.frame", "matrix"))
Germany_KF4 <- tsSmooth(fit,
                       type = c("xtt"),
                       interval = c("confidence"),
                       level = 0.95, fun.kf = c("MARSSkfas"))



PIB_POTENTIEL_KF_Germany <- logGermany - Germany_KF4$.estimate[1:124]/100
PIB_POTENTIEL_KF_Germanyts <- ts(PIB_POTENTIEL_KF_Germany, start = c(1991, 1), frequency = 4)

plot(PIB_POTENTIEL_KF_Germany)
plot(logGermany)


# Plot time series
plot.ts(PIB_POTENTIEL_KF_Germanyts, ylab = "",col="black")  

# include HP trend
lines(Germanyts, col = "red")
lines(trend_Germany_hpts, col = "blue")
legend("topleft", legend = c("PIB_Potentiel Kalman Filter Germany", "Log Germany", "HP trend"), lty = 1, 
       fill = c("black", "red","blue"), bty = "n")





#Detrend data with a linear filter

lin.mod_Germany <- lm(Germanyts ~ time(Germanyts))
lin.trend_Germany <- lin.mod_Germany$fitted.values  # fitted values pertain to time trend
linear_Germany <- ts(lin.trend_Germany, start = c(1991, 1), frequency = 4)  # create a time series variable for trend
lin.cycle_Germany <- Germanyts - linear_Germany  # cycle is the difference between the data and linear trend

ts.plot(linear_Germany, Germanyts, gpars = list(col = c("black", "red")))

# Baxter-King filter (Band pass filter)

Germany_bk <- bkfilter(Germanyts,pl = 6, pu = 32)

cycle_Germany_bk<-Germany_bk$cycle
trend_Germany_bk<-Germany_bk$trend
Germanytss_bk<-Germany_bk$x

ts.plot(Germanytss_bk, trend_Germany_bk, gpars = list(col = c("black", "red")))



linear_trend_Germany<-ts(linear_Germany, start = c(1991, 1), frequency = 4)
trend_Germany_bkts<-ts(trend_Germany_bk, start = c(1991, 1), frequency = 4)

# Plot time series
plot.ts(PIB_POTENTIEL_KF_Germanyts, ylab = "",col="black")  

# include HP trend
lines(Germanyts, col = "red")
lines(trend_Germany_hpts, col = "blue")
lines(linear_trend_Germany, col = "green")
lines(trend_Germany_bkts, col = "orange")
legend("topleft", legend = c("GDP potential Kalman Filter Germany", "Log GDP Germany", "HP Germany trend","Linear Germany trend","Baxter-King Germany trend" ), lty = 1, 
       fill = c("black", "red","blue","green","orange"), bty = "n")



observed_Germany<-Germanyts
predicted_Germany_Kalman<-PIB_POTENTIEL_KF_Germanyts
predicted_Germany_HP<-trend_Germany_hpts
predicted_Germany_Linear<-linear_trend_Germany
predicted_Germany_Baxter_King<-trend_Germany_bkts



mae_Kalman_Germany_cycle<-mae(observed_Germany,predicted_Germany_Kalman)
mae_Kalman_Germany_cycle
mae_HP_Germany_cycle<-mae(observed_Germany,predicted_Germany_HP)
mae_HP_Germany_cycle
mae_Linear_Germany_cycle<-mae(observed_Germany,predicted_Germany_Linear)
mae_Linear_Germany_cycle
mae_Baxter_King_Germany_cycle<-mae(observed_Germany,predicted_Germany_Baxter_King)
mae_Baxter_King_Germany_cycle




diff_lgdp_Germany_ts<-ts(diff_lgdp_Germany/100, end = c(2022, 1), frequency = 4)
cycle_Germany_bk_ts<-ts(cycle_Germany_bk, end = c(2022, 1), frequency = 4)
cycle_Germany_linear_ts<-ts(lin.cycle_Germany, start = c(1991, 1), frequency = 4)
cycle_Germany_Kalman_a <- logGermany - PIB_POTENTIEL_KF_Germany
cycle_Germany_Kalman_ts<-ts(cycle_Germany_Kalman_a, end = c(2022, 1), frequency = 4)


# Plot time series
plot.ts(diff_lgdp_Germany_ts, ylab = "",col="black")  

# include HP trend
lines(cycle_Germany_Kalman_ts, col = "red")
lines(cycle_Germany_hpts, col = "blue")
lines(cycle_Germany_linear_ts, col = "green")
lines(cycle_Germany_bk_ts, col = "orange")
legend("topleft", legend = c("Log Delta GDP Germany","Cycle Germany Kalman Filter GDP", "HP Germany Cycle","Linear Germany cycle","Baxter-King Germany cycle" ), lty = 1, 
       fill = c("black", "red","blue","green","orange"), bty = "n")


observed_Germany_cycle<-diff_lgdp_Germany_ts
predicted_Germany_Kalman_cycle<-cycle_Germany_Kalman_ts
predicted_Germany_HP_cycle<-cycle_Germany_hpts
predicted_Germany_Linear_cycle<-cycle_Germany_linear_ts
predicted_Germany_Baxter_King_cycle<-cycle_Germany_bk_ts



mae_Kalman_Germany<- mae(observed_Germany,predicted_Germany_Kalman_cycle)
mae_Kalman_Germany
mae_HP_Germany<-mae(observed_Germany,predicted_Germany_HP_cycle)
mae_HP_Germany
mae_Linear_Germany<-mae(observed_Germany,predicted_Germany_Linear_cycle)
mae_Linear_Germany
mae_Baxter_King_Germany<-mae(observed_Germany,predicted_Germany_Baxter_King_cycle)
mae_Baxter_King_Germany






###Italia

#Frequence du parametre HP smoother

Italia<-gdp[[4]]
logItalia<-log(Italia)

Italia<-na.omit(Italia)

Italiats<-ts(data=logItalia,start=(1975),frequency=4)     

Italiats<- na.omit(Italiats) 
plot(Italiats)


Italia_hp <- hpfilter(Italiats, freq = 1600, type = "lambda",drift=FALSE)

# Plot time series
plot.ts(Italiats, ylab = "")  

# include HP trend
lines(Italia_hp$trend, col = "red")
legend("topleft", legend = c("Log GDP Italia", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")

#Plot cycle
plot.ts(Italia_hp$cycle, ylab = "") 
legend("topleft", legend = c("HP cycle Italia"), lty = 1, col = c("black"), 
       bty = "n")








logItalia<-na.omit(logItalia)
Italia_hp_reg <- hpfilter(logItalia, freq = 1600, type = "lambda",drift=FALSE)

cycle_Italia_hp<-Italia_hp_reg$cycle
trend_Italia_hp<-Italia_hp_reg$trend


cycle_Italia_hpts<-ts(data=cycle_Italia_hp,start=(1995),frequency=4)
trend_Italia_hpts<-ts(data=trend_Italia_hp,start=(1995),frequency=4)

cycle_Italia_hpts<-na.omit(cycle_Italia_hpts)
trend_Italia_hpts<-na.omit(trend_Italia_hpts)



diff_lgdp_Italia<-diff(logItalia)*100


Italia_infla <-df_infla[4]
Italia_inflats<-ts(data=Italia_infla,start=(1975),frequency=4)

Italia_inflats<- na.omit(Italia_inflats) 
Italia_infla <- matrix(data = Italia_inflats)






#Détermination des coefficients pour le modèle espace-etat 

#1ere Trend
trend_Italia<-lm(diff_lgdp_Italia~1+offset(diff(cycle_Italia_hp)))
summary(trend_Italia)


#3 eme equation Cycle

cycle_Italia<-lm(cycle_Italia_hp~0+lag(cycle_Italia_hp,1)+lag(cycle_Italia_hp,2))
summary(cycle_Italia)










trend_Italia_hp<-trend_Italia_hp[-108,]
trend_Italia_hp<-na.omit(trend_Italia_hp)

Italia_infla_reg<-Italia_infla[-1,]
Italia_infla_reg<-na.omit(Italia_infla_reg)

Italia_infla_reg<-na.omit(Italia_infla_reg) 


inflation_markup_model_Italia<-lm(Italia_infla_reg~lag(Italia_infla_reg,1)+trend_Italia_hp)
summary(inflation_markup_model_Italia)




#Nom des coefficients

#equation d'etat
B_Italia <- cycle_Italia$coefficients
b1_Italia <- B_Italia[1]
b2_Italia <- B_Italia[2]

#equations d'observation
Delta1_Italia <- trend_Italia$coefficients
Alphas_Italia <-inflation_markup_model_Italia$coefficients
Alpha1_Italia <- Alphas_Italia[1] + 11.1
Alpha2_Italia <- Alphas_Italia[2] -0.9
Alpha3_Italia <- Alphas_Italia[3] + 0.5

#valeurs initiales
OGbegint0_Italia <- cycle_Italia_hp[[3]]
OGbegint_moins1_Italia <- cycle_Italia_hp[[2]]


#préparation matrice variables d'observation du modèle espace etat

mat_obs_Italia <- matrix(, nrow = 107, ncol = 3)
mat_obs_Italia[,1] <- diff_lgdp_Italia
mat_obs_Italia[,2] <- Italia_infla_reg
mat_obs_Italia[,3] <- lag(Italia_infla_reg,1)
mat_obs_Italia <- na.omit(mat_obs_Italia)





#modele espace-etat multivar avec lags
B2_Italia <- matrix(list(b1_Italia, 1, b2_Italia, 0), 2, 2)
Z2_Italia <- matrix(list(1, Alpha3_Italia, 0, -1, 0, 0), nrow=3, ncol=2)
A2_Italia <- matrix(list(Delta1_Italia, Alpha1_Italia, 0), nrow=3, ncol=1)
Q2_Italia <- matrix(list("q1", 0, 0, 0), 2, 2)
u2_Italia <- matrix(list(0,0),nrow = 2, ncol = 1)
d2_Italia <- t(mat_obs_Italia)
D2_Italia <- matrix(list (0, 0, 0, 0, 0, 0, 0, Alpha2_Italia, 1), 3, 3)
R2_Italia <- matrix(list ("r11", 0, 0, 0, "r22", 0, 0, 0, 0.01), 3, 3)
x02_Italia <- matrix(list(OGbegint0_Italia, OGbegint_moins1_Italia), nrow = 2, ncol = 1)
model.list2_Italia <- list(B = B2_Italia, Q=Q2_Italia, Z = Z2_Italia, A = A2_Italia, d=d2_Italia, D=D2_Italia, U=u2_Italia, R=R2_Italia, x0= x02_Italia, tinitx = 1)
fit <- MARSS(d2_Italia, model=model.list2_Italia, fit = TRUE)
Italia_KF3 <- fitted(fit, type="ytT", interval = c("confidence"),level = 0.95)
Italia_KF4 <- tsSmooth(fit,
                        type = c("xtt"),
                        interval = c("confidence"),
                        level = 0.95, fun.kf = c("MARSSkfas"))

PIB_POTENTIEL_KF_Italia <- logItalia - Italia_KF4$.estimate[1:108]/100
PIB_POTENTIEL_KF_Italiats <- ts(PIB_POTENTIEL_KF_Italia, start = c(1995, 1), frequency = 4)
plot(PIB_POTENTIEL_KF_Italia)
plot(logItalia)


# Plot time series
plot.ts(PIB_POTENTIEL_KF_Italiats, ylab = "", col="black")  

# include HP trend
lines(Italiats, col = "red")
lines(trend_Italia_hpts, col = "blue")
legend("topleft", legend = c("PIB_Potentiel Kalman Filter Italia", "Log Italia", "HP trend"), lty = 1, 
       fill = c("black", "red","blue"), bty = "n")




#Detrend data with a linear filter


lin.mod_Italia <- lm(Italiats ~ time(Italiats))
lin.trend_Italia <- lin.mod_Italia$fitted.values  # fitted values pertain to time trend
linear_Italia <- ts(lin.trend_Italia, start = c(1995, 1), frequency = 4)  # create a time series variable for trend
lin.cycle_Italia <- Italiats - linear_Italia  # cycle is the difference between the data and linear trend

ts.plot(linear_Italia, Italiats, gpars = list(col = c("black", "red")))


# Baxter-King filter (Band pass filter)

Italia_bk <- bkfilter(Italiats,pl = 6, pu = 32)

cycle_Italia_bk<-Italia_bk$cycle
trend_Italia_bk<-Italia_bk$trend
Italiatss_bk<-Italia_bk$x

ts.plot(Italiatss_bk, trend_Italia_bk, gpars = list(col = c("black", "red")))





linear_trend_Italia<-ts(linear_Italia, start = c(1995, 1), frequency = 4)
trend_Italia_bkts<-ts(trend_Italia_bk, start = c(1995, 1), frequency = 4)

# Plot time series
plot.ts(PIB_POTENTIEL_KF_Italiats, ylab = "",col="black")  

# include HP trend
lines(Italiats, col = "red")
lines(trend_Italia_hpts, col = "blue")
lines(linear_trend_Italia, col = "green")
lines(trend_Italia_bkts, col = "orange")
legend("topleft", legend = c("GDP potential Kalman Filter Italia", "Log GDP Italia", "HP Italia trend","Linear Italia trend","Baxter-King Italia trend" ), lty = 1, 
       fill = c("black", "red","blue","green","orange"), bty = "n")





observed_Italia<-Italiats
predicted_Italia_Kalman<-PIB_POTENTIEL_KF_Italiats
predicted_Italia_HP<-trend_Italia_hpts
predicted_Italia_Linear<-linear_trend_Italia
predicted_Italia_Baxter_King<-trend_Italia_bkts



mae_Kalman_Italia<-mae(observed_Italia,predicted_Italia_Kalman)
mae_Kalman_Italia
mae_HP_Italia<-mae(observed_Italia,predicted_Italia_HP)
mae_HP_Italia
mae_Linear_Italia<-mae(observed_Italia,predicted_Italia_Linear)
mae_Linear_Italia
mae_Baxter_King_Italia<-mae(observed_Italia,predicted_Italia_Baxter_King)
mae_Baxter_King_Italia



diff_lgdp_Italia_ts<-ts(diff_lgdp_Italia/100, end = c(2022, 1), frequency = 4)
cycle_Italia_bk_ts<-ts(cycle_Italia_bk, end = c(2022, 1), frequency = 4)
cycle_Italia_linear_ts<-ts(lin.cycle_Italia, start = c(1995, 1), frequency = 4)
cycle_Italia_Kalman_a <- logItalia - PIB_POTENTIEL_KF_Italia
cycle_Italia_Kalman_ts<-ts(cycle_Italia_Kalman_a, end = c(2022, 1), frequency = 4)


# Plot time series
plot.ts(diff_lgdp_Italia_ts, ylab = "",col="black")  

# include HP trend
lines(cycle_Italia_Kalman_ts, col = "red")
lines(cycle_Italia_hpts, col = "blue")
lines(cycle_Italia_linear_ts, col = "green")
lines(cycle_Italia_bk_ts, col = "orange")
legend("topleft", legend = c("Log Delta GDP Italia","Cycle Italia Kalman Filter GDP", "HP Italia Cycle","Linear Italia cycle","Baxter-King Italia cycle" ), lty = 1, 
       fill = c("black", "red","blue","green","orange"), bty = "n")





observed_Italia_cycle<-diff_lgdp_Italia_ts
predicted_Italia_Kalman_cycle<-cycle_Italia_Kalman_ts
predicted_Italia_HP_cycle<-cycle_Italia_hpts
predicted_Italia_Linear_cycle<-cycle_Italia_linear_ts
predicted_Italia_Baxter_King_cycle<-cycle_Italia_bk_ts



mae_Kalman_Italia_cycle<- mae(observed_Italia,predicted_Italia_Kalman_cycle)
mae_Kalman_Italia_cycle
mae_HP_Italia_cycle<-mae(observed_Italia,predicted_Italia_HP_cycle)
mae_HP_Italia_cycle
mae_Linear_Italia_cycle<-mae(observed_Italia,predicted_Italia_Linear_cycle)
mae_Linear_Italia_cycle
mae_Baxter_King_Italia_cycle<-mae(observed_Italia,predicted_Italia_Baxter_King_cycle)
mae_Baxter_King_Italia_cycle






###Spain

#Frequence du parametre HP smoother

Spain<-gdp[[5]]
Spain<-na.omit(Spain)

logSpain<-log(Spain)
Spaints<-ts(data=logSpain,start=(1995),frequency=4)     

Spaints<- na.omit(Spaints) 
plot(Spaints)


Spain_hp <- hpfilter(Spaints, freq = 1600, type = "lambda",drift=FALSE)

# Plot time series
plot.ts(Spaints, ylab = "")  

# include HP trend
lines(Spain_hp$trend, col = "red")
legend("topleft", legend = c("Log GDP Spain", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")

#Plot cycle
plot.ts(Spain_hp$cycle, ylab = "") 
legend("topleft", legend = c("HP cycle Spain"), lty = 1, col = c("black"), 
       bty = "n")








logSpain<-na.omit(logSpain)
Spain_hp_reg <- hpfilter(logSpain, freq = 1600, type = "lambda",drift=FALSE)

cycle_Spain_hp<-Spain_hp_reg$cycle
trend_Spain_hp<-Spain_hp_reg$trend

cycle_Spain_hpts<-ts(data=cycle_Spain_hp,start=(1995),frequency=4)
trend_Spain_hpts<-ts(data=trend_Spain_hp,start=(1995),frequency=4)

cycle_Spain_hpts<-na.omit(cycle_Spain_hpts)
trend_Spain_hpts<-na.omit(trend_Spain_hpts)



diff_lgdp_Spain<-diff(logSpain)*100


Spain_infla <-df_infla[5]
Spain_inflats<-ts(data=Spain_infla,start=(1975),end=(2022),frequency=4)

Spain_inflats<- na.omit(Spain_inflats) 
Spain_infla <- matrix(data = Spain_inflats)






#Détermination des coefficients pour le modèle espace-etat 

#1ere Trend
trend_Spain<-lm(diff_lgdp_Spain~1+offset(diff(cycle_Spain_hp)))
summary(trend_Spain)


#3 eme equation Cycle

cycle_Spain<-lm(cycle_Spain_hp~0+lag(cycle_Spain_hp,1)+lag(cycle_Spain_hp,2))
summary(cycle_Spain)










trend_Spain_hp<-trend_Spain_hp[-108,]
trend_Spain_hp<-na.omit(trend_Spain_hp)

Spain_infla_reg<-Spain_infla[-1,]
Spain_infla_reg<-na.omit(Spain_infla_reg)

Spain_infla_reg<-na.omit(Spain_infla_reg) 


inflation_markup_model_Spain<-lm(Spain_infla_reg~lag(Spain_infla_reg,1)+trend_Spain_hp)
summary(inflation_markup_model_Spain)




#Nom des coefficients

#equation d'etat
B_Spain <- cycle_Spain$coefficients
b1_Spain <- B_Spain[1]
b2_Spain <- B_Spain[2]

#equations d'observation
Delta1_Spain <- trend_Spain$coefficients
Alphas_Spain <-inflation_markup_model_Spain$coefficients
Alpha1_Spain <- Alphas_Spain[1]
Alpha2_Spain <- Alphas_Spain[2]
Alpha3_Spain <- Alphas_Spain[3]

#valeurs initiales
OGbegint0_Spain <- cycle_Spain_hp[[3]]
OGbegint_moins1_Spain <- cycle_Spain_hp[[2]]


#préparation matrice variables d'observation du modèle espace etat

mat_obs_Spain <- matrix(, nrow = 107, ncol = 3)
mat_obs_Spain[,1] <- diff_lgdp_Spain
mat_obs_Spain[,2] <- Spain_infla_reg
mat_obs_Spain[,3] <- lag(Spain_infla_reg,1)
mat_obs_Spain <- na.omit(mat_obs_Spain)





#modele espace-etat multivar avec lags
B2_Spain <- matrix(list(b1_Spain, 1, b2_Spain, 0), 2, 2)
Z2_Spain <- matrix(list(1, Alpha3_Spain, 0, -1, 0, 0), nrow=3, ncol=2)
A2_Spain <- matrix(list(Delta1_Spain, Alpha1_Spain, 0), nrow=3, ncol=1)
Q2_Spain <- matrix(list("q1", 0, 0, 0), 2, 2)
u2_Spain <- matrix(list(0,0),nrow = 2, ncol = 1)
d2_Spain <- t(mat_obs_Spain)
D2_Spain <- matrix(list (0, 0, 0, 0, 0, 0, 0, Alpha2_Spain, 1), 3, 3)
R2_Spain <- matrix(list ("r11", 0, 0, 0, "r22", 0, 0, 0, 0.01), 3, 3)
x02_Spain <- matrix(list(OGbegint0_Spain, OGbegint_moins1_Spain), nrow = 2, ncol = 1)
model.list2_Spain <- list(B = B2_Spain, Q=Q2_Spain, Z = Z2_Spain, A = A2_Spain, d=d2_Spain, D=D2_Spain, U=u2_Spain, R=R2_Spain, x0= x02_Spain, tinitx = 1)
fit <- MARSS(d2_Spain, model=model.list2_Spain, fit = TRUE)
Spain_KF3 <- fitted(fit, type="ytT", interval = c("confidence"),level = 0.95, output = c("data.frame", "matrix"))
Spain_KF4 <- tsSmooth(fit,
                       type = c("xtt"),
                       interval = c("confidence"),
                       level = 0.95, fun.kf = c("MARSSkfas"))



PIB_POTENTIEL_KF_Spain <- logSpain - Spain_KF4$.estimate[1:108]/100
PIB_POTENTIEL_KF_Spaints <- ts(PIB_POTENTIEL_KF_Spain, start = c(1995, 1), frequency = 4)


# Plot time series
plot.ts(PIB_POTENTIEL_KF_Spaints, ylab = "",col="black")  

# include HP trend
lines(Spaints, col = "red")
lines(trend_Spain_hpts, col = "blue")
legend("topleft", legend = c("PIB_Potentiel Kalman Filter Spain", "Log Spain", "HP trend"), lty = 1, 
       fill = c("black", "red","blue"), bty = "n")




#Detrend data with a linear filter

lin.mod_Spain <- lm(Spaints ~ time(Spaints))
lin.trend_Spain <- lin.mod_Spain$fitted.values  # fitted values pertain to time trend
linear_Spain <- ts(lin.trend_Spain, start = c(1995, 1), frequency = 4)  # create a time series variable for trend
lin.cycle_Spain <- Spaints - linear_Spain  # cycle is the difference between the data and linear trend

ts.plot(linear_Spain, Spaints, gpars = list(col = c("black", "red")))




# Baxter-King filter (Band pass filter)

Spain_bk <- bkfilter(Spaints,pl = 6, pu = 32)

cycle_Spain_bk<-Spain_bk$cycle
trend_Spain_bk<-Spain_bk$trend
Spaintss_bk<-Spain_bk$x

ts.plot(Spaintss_bk, trend_Spain_bk, gpars = list(col = c("black", "red")))




linear_trend_Spain<-ts(linear_Spain, start = c(1995, 1), frequency = 4)
trend_Spain_bkts<-ts(trend_Spain_bk, start = c(1995, 1), frequency = 4)

# Plot time series
plot.ts(PIB_POTENTIEL_KF_Spaints, ylab = "",col="black")  

# include HP trend
lines(Spaints, col = "red")
lines(trend_Spain_hpts, col = "blue")
lines(linear_trend_Spain, col = "green")
lines(trend_Spain_bkts, col = "orange")
legend("topleft", legend = c("GDP potential Kalman Filter Spain", "Log GDP Spain", "HP Spain trend","Linear Spain trend","Baxter-King Spain trend" ), lty = 1, 
       fill = c("black", "red","blue","green","orange"), bty = "n")


observed_Spain<-Spaints
predicted_Spain_Kalman<-PIB_POTENTIEL_KF_Spaints
predicted_Spain_HP<-trend_Spain_hpts
predicted_Spain_Linear<-linear_trend_Spain
predicted_Spain_Baxter_King<-trend_Spain_bkts



mae_Kalman_Spain<-mae(observed_Spain,predicted_Spain_Kalman)
mae_Kalman_Spain
mae_HP_Spain<-mae(observed_Spain,predicted_Spain_HP)
mae_HP_Spain
mae_Linear_Spain<-mae(observed_Spain,predicted_Spain_Linear)
mae_Linear_Spain
mae_Baxter_King_Spain<-mae(observed_Spain,predicted_Spain_Baxter_King)
mae_Baxter_King_Spain




diff_lgdp_Spain_ts<-ts(diff_lgdp_Spain/100, end = c(2022, 1), frequency = 4)
cycle_Spain_bk_ts<-ts(cycle_Spain_bk, end = c(2022, 1), frequency = 4)
cycle_Spain_linear_ts<-ts(lin.cycle_Spain, start = c(1995, 1), frequency = 4)
cycle_Spain_Kalman_a <- logSpain - PIB_POTENTIEL_KF_Spain
cycle_Spain_Kalman_ts<-ts(cycle_Spain_Kalman_a, end = c(2022, 1), frequency = 4)


# Plot time series
plot.ts(diff_lgdp_Spain_ts, ylab = "",col="black")  

# include HP trend
lines(cycle_Spain_Kalman_ts, col = "red")
lines(cycle_Spain_hpts, col = "blue")
lines(cycle_Spain_linear_ts, col = "green")
lines(cycle_Spain_bk_ts, col = "orange")
legend("topleft", legend = c("Log Delta GDP Spain","Cycle Spain Kalman Filter GDP", "HP Spain Cycle","Linear Spain cycle","Baxter-King Spain cycle" ), lty = 1, 
       fill = c("black", "red","blue","green","orange"), bty = "n")



observed_Spain_cycle<-diff_lgdp_Spain_ts
predicted_Spain_Kalman_cycle<-cycle_Spain_Kalman_ts
predicted_Spain_HP_cycle<-cycle_Spain_hpts
predicted_Spain_Linear_cycle<-cycle_Spain_linear_ts
predicted_Spain_Baxter_King_cycle<-cycle_Spain_bk_ts



mae_Kalman_Spain_cycle<- mae(observed_Spain,predicted_Spain_Kalman_cycle)
mae_Kalman_Spain_cycle
mae_HP_Spain_cycle<-mae(observed_Spain,predicted_Spain_HP_cycle)
mae_HP_Spain_cycle
mae_Linear_Spain_cycle<-mae(observed_Spain,predicted_Spain_Linear_cycle)
mae_Linear_Spain_cycle
mae_Baxter_King_Spain_cycle<-mae(observed_Spain,predicted_Spain_Baxter_King_cycle)
mae_Baxter_King_Spain_cycle


###Japan

#Frequence du parametre HP smoother

Japan<-gdp[[6]]
Japan<-na.omit(Japan)

logJapan<-log(Japan)
Japants<-ts(data=logJapan,start=(1994),frequency=4)     

Japants<- na.omit(Japants) 
plot(Japants)

Japan_hp <- hpfilter(Japants, freq = 1600, type = "lambda",drift=FALSE)

# Plot time series
plot.ts(Japants, ylab = "")  

# include HP trend
lines(Japan_hp$trend, col = "red")
legend("topleft", legend = c("Log GDP Japan", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")

#Plot cycle
plot.ts(Japan_hp$cycle, ylab = "") 
legend("topleft", legend = c("HP cycle Japan"), lty = 1, col = c("black"), 
       bty = "n")




logJapan<-na.omit(logJapan)
Japan_hp_reg <- hpfilter(logJapan, freq = 1600, type = "lambda",drift=FALSE)

cycle_Japan_hp<-Japan_hp_reg$cycle
trend_Japan_hp<-Japan_hp_reg$trend

cycle_Japan_hpts<-ts(data=cycle_Japan_hp,start=(1994),frequency=4)
trend_Japan_hpts<-ts(data=trend_Japan_hp,start=(1994),frequency=4)

cycle_Japan_hpts<-na.omit(cycle_Japan_hpts)
trend_Japan_hpts<-na.omit(trend_Japan_hpts)



diff_lgdp_Japan<-diff(logJapan)*100


Japan_infla <-df_infla[6]
Japan_inflats<-ts(data=Japan_infla,start=(1975),end=(2022),frequency=4)

Japan_inflats<- na.omit(Japan_inflats) 
Japan_infla <- matrix(data = Japan_inflats)






#Détermination des coefficients pour le modèle espace-etat 

#1ere Trend
trend_Japan<-lm(diff_lgdp_Japan~1+offset(diff(cycle_Japan_hp)))
summary(trend_Japan)


#3 eme equation Cycle

cycle_Japan<-lm(cycle_Japan_hp~0+lag(cycle_Japan_hp,1)+lag(cycle_Japan_hp,2))
summary(cycle_Japan)










trend_Japan_hp<-trend_Japan_hp[-112,]
trend_Japan_hp<-na.omit(trend_Japan_hp)

Japan_infla_reg<-Japan_infla[-1,]
Japan_infla_reg<-na.omit(Japan_infla_reg)

Japan_infla_reg<-na.omit(Japan_infla_reg) 


inflation_markup_model_Japan<-lm(Japan_infla_reg~lag(Japan_infla_reg,1)+trend_Japan_hp)
summary(inflation_markup_model_Japan)




#Nom des coefficients

#equation d'etat
B_Japan <- cycle_Japan$coefficients
b1_Japan <- B_Japan[1]
b2_Japan <- B_Japan[2]

#equations d'observation
Delta1_Japan <- trend_Japan$coefficients
Alphas_Japan <-inflation_markup_model_Japan$coefficients
Alpha1_Japan <- Alphas_Japan[1] + 9.5
Alpha2_Japan <- Alphas_Japan[2] -0.2
Alpha3_Japan <- Alphas_Japan[3]

#valeurs initiales
OGbegint0_Japan <- cycle_Japan_hp[[3]]
OGbegint_moins1_Japan <- cycle_Japan_hp[[2]]


#préparation matrice variables d'observation du modèle espace etat

mat_obs_Japan <- matrix(, nrow = 111, ncol = 3)
mat_obs_Japan[,1] <- diff_lgdp_Japan
mat_obs_Japan[,2] <- Japan_infla_reg
mat_obs_Japan[,3] <- lag(Japan_infla_reg,1)
mat_obs_Japan <- na.omit(mat_obs_Japan)





#modele espace-etat multivar avec lags
B2_Japan <- matrix(list(b1_Japan, 1, b2_Japan, 0), 2, 2)
Z2_Japan <- matrix(list(1, Alpha3_Japan, 0, -1, 0, 0), nrow=3, ncol=2)
A2_Japan <- matrix(list(Delta1_Japan, Alpha1_Japan, 0), nrow=3, ncol=1)
Q2_Japan <- matrix(list("q1", 0, 0, 0), 2, 2)
u2_Japan <- matrix(list(0,0),nrow = 2, ncol = 1)
d2_Japan <- t(mat_obs_Japan)
D2_Japan <- matrix(list (0, 0, 0, 0, 0, 0, 0, Alpha2_Japan, 1), 3, 3)
R2_Japan <- matrix(list ("r11", 0, 0, 0, "r22", 0, 0, 0, 0.01), 3, 3)
x02_Japan <- matrix(list(OGbegint0_Japan, OGbegint_moins1_Japan), nrow = 2, ncol = 1)
model.list2_Japan <- list(B = B2_Japan, Q=Q2_Japan, Z = Z2_Japan, A = A2_Japan, d=d2_Japan, D=D2_Japan, U=u2_Japan, R=R2_Japan, x0= x02_Japan, tinitx = 1)
fit <- MARSS(d2_Japan, model=model.list2_Japan, fit = TRUE)
Japan_KF3 <- fitted(fit, type="ytT", interval = c("confidence"),level = 0.95, output = c("data.frame", "matrix"))
Japan_KF4 <- tsSmooth(fit,
                       type = c("xtt"),
                       interval = c("confidence"),
                       level = 0.95, fun.kf = c("MARSSkfas"))



PIB_POTENTIEL_KF_Japan <- logJapan - Japan_KF4$.estimate[1:112]/100
PIB_POTENTIEL_KF_Japants <- ts(PIB_POTENTIEL_KF_Japan, start = c(1994, 1), frequency = 4)

# Plot time series
plot.ts(PIB_POTENTIEL_KF_Japants, ylab = "", col ="black")  

# include HP trend
lines(Japants, col = "red")
lines(trend_Japan_hpts, col = "blue")
legend("topleft", legend = c("PIB_Potentiel Kalman Filter Japan", "Log Japan", "HP trend"), lty = 1, 
       fill = c("black", "red","blue"), bty = "n")






#Detrend data with a linear filter

lin.mod_Japan <- lm(Japants ~ time(Japants))
lin.trend_Japan <- lin.mod_Japan$fitted.values  # fitted values pertain to time trend
linear_Japan <- ts(lin.trend_Japan, start = c(1994, 1), frequency = 4)  # create a time series variable for trend
lin.cycle_Japan <- Japants - linear_Japan  # cycle is the difference between the data and linear trend

ts.plot(linear_Japan, Japants, gpars = list(col = c("black", "red")))



# Baxter-King filter (Band pass filter)

Japan_bk <- bkfilter(Japants,pl = 6, pu = 32)

cycle_Japan_bk<-Japan_bk$cycle
trend_Japan_bk<-Japan_bk$trend
Japantss_bk<-Japan_bk$x

ts.plot(Japantss_bk, trend_Japan_bk, gpars = list(col = c("black", "red")))



linear_trend_Japan<-ts(linear_Japan, start = c(1994, 1), frequency = 4)
trend_Japan_bkts<-ts(trend_Japan_bk, start = c(1994, 1), frequency = 4)

# Plot time series
plot.ts(PIB_POTENTIEL_KF_Japants, ylab = "",col="black")  

# include HP trend
lines(Japants, col = "red")
lines(trend_Japan_hpts, col = "blue")
lines(linear_trend_Japan, col = "green")
lines(trend_Japan_bkts, col = "orange")
legend("topleft", legend = c("GDP potential Kalman Filter Japan", "Log GDP Japan", "HP Japan trend","Linear Japan trend","Baxter-King Japan trend" ), lty = 1, 
       fill = c("black", "red","blue","green","orange"), bty = "n")


observed_Japan<-Japants
predicted_Japan_Kalman<-PIB_POTENTIEL_KF_Japants
predicted_Japan_HP<-trend_Japan_hpts
predicted_Japan_Linear<-linear_trend_Japan
predicted_Japan_Baxter_King<-trend_Japan_bkts



mae_Kalman_Japan<-mae(observed_Japan,predicted_Japan_Kalman)
mae_Kalman_Japan
mae_HP_Japan<-mae(observed_Japan,predicted_Japan_HP)
mae_HP_Japan
mae_Linear_Japan<-mae(observed_Japan,predicted_Japan_Linear)
mae_Linear_Japan
mae_Baxter_King_Japan<-mae(observed_Japan,predicted_Japan_Baxter_King)
mae_Baxter_King_Japan



diff_lgdp_Japan_ts<-ts(diff_lgdp_Japan/100, end = c(2022, 1), frequency = 4)
cycle_Japan_bk_ts<-ts(cycle_Japan_bk, end = c(2022, 1), frequency = 4)
cycle_Japan_linear_ts<-ts(lin.cycle_Japan, start = c(1994, 1), frequency = 4)
cycle_Japan_Kalman_a <- logJapan - PIB_POTENTIEL_KF_Japan
cycle_Japan_Kalman_ts<-ts(cycle_Japan_Kalman_a, end = c(2022, 1), frequency = 4)


# Plot time series
plot.ts(diff_lgdp_Japan_ts, ylab = "",col="black")  

# include HP trend
lines(cycle_Japan_Kalman_ts, col = "red")
lines(cycle_Japan_hpts, col = "blue")
lines(cycle_Japan_linear_ts, col = "green")
lines(cycle_Japan_bk_ts, col = "orange")
legend("topleft", legend = c("Log Delta GDP Japan","Cycle Japan Kalman Filter GDP", "HP Japan Cycle","Linear Japan cycle","Baxter-King Japan cycle" ), lty = 1, 
       fill = c("black", "red","blue","green","orange"), bty = "n")




###United_Kingdom

#Frequence du parametre HP smoother

United_Kingdom<-gdp[[7]]
logUnited_Kingdom<-log(United_Kingdom)

United_Kingdom<-na.omit(United_Kingdom)

United_Kingdomts<-ts(data=logUnited_Kingdom,start=(1975),frequency=4)     

United_Kingdomts<- na.omit(United_Kingdomts) 
plot(United_Kingdomts)


United_Kingdom_hp <- hpfilter(United_Kingdomts, freq = 1600, type = "lambda",drift=FALSE)

# Plot time series
plot.ts(United_Kingdomts, ylab = "")  

# include HP trend
lines(United_Kingdom_hp$trend, col = "red")
legend("topleft", legend = c("Log GDP United_Kingdom", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")

#Plot cycle
plot.ts(United_Kingdom_hp$cycle, ylab = "") 
legend("topleft", legend = c("HP cycle United_Kingdom"), lty = 1, col = c("black"), 
       bty = "n")








logUnited_Kingdom<-na.omit(logUnited_Kingdom)
United_Kingdom_hp_reg <- hpfilter(logUnited_Kingdom, freq = 1600, type = "lambda",drift=FALSE)

cycle_United_Kingdom_hp<-United_Kingdom_hp_reg$cycle
trend_United_Kingdom_hp<-United_Kingdom_hp_reg$trend

cycle_United_Kingdom_hpts<-ts(data=cycle_United_Kingdom_hp,start=(1975),frequency=4)
trend_United_Kingdom_hpts<-ts(data=trend_United_Kingdom_hp,start=(1975),frequency=4)

cycle_United_Kingdom_hpts<-na.omit(cycle_United_Kingdom_hpts)
trend_United_Kingdom_hpts<-na.omit(trend_United_Kingdom_hpts)



diff_lgdp_United_Kingdom<-diff(logUnited_Kingdom)*100


United_Kingdom_infla <-df_infla[7]
United_Kingdom_inflats<-ts(data=United_Kingdom_infla,start=(1975),frequency=4)

United_Kingdom_inflats<- na.omit(United_Kingdom_inflats) 
United_Kingdom_infla <- matrix(data = United_Kingdom_inflats)






#Détermination des coefficients pour le modèle espace-etat 

#1ere équation Trend
trend_United_Kingdom<-lm(diff_lgdp_United_Kingdom~1+offset(diff(cycle_United_Kingdom_hp)))
summary(trend_United_Kingdom)


#3 eme equation Cycle

cycle_United_Kingdom<-lm(cycle_United_Kingdom_hp~0+lag(cycle_United_Kingdom_hp,1)+lag(cycle_United_Kingdom_hp,2))
summary(cycle_United_Kingdom)



#2 eme equation
trend_United_Kingdom_hp<-trend_United_Kingdom_hp[-180,]
trend_United_Kingdom_hp<-na.omit(trend_United_Kingdom_hp)

United_Kingdom_infla_reg<-United_Kingdom_infla[-1,]
United_Kingdom_infla_reg<-na.omit(United_Kingdom_infla_reg)







inflation_markup_model_United_Kingdom<-lm(United_Kingdom_infla_reg~lag(United_Kingdom_infla_reg,1)+trend_United_Kingdom_hp)
summary(inflation_markup_model_United_Kingdom)




#Nom des coefficients

#equation d'etat
B_United_Kingdom <- cycle_United_Kingdom$coefficients
b1_United_Kingdom <- B_United_Kingdom[1]
b2_United_Kingdom <- B_United_Kingdom[2]

#equations d'observation
Delta1_United_Kingdom <- trend_United_Kingdom$coefficients
Alphas_United_Kingdom <-inflation_markup_model_United_Kingdom$coefficients
Alpha1_United_Kingdom <- Alphas_United_Kingdom[1]-5.1
Alpha2_United_Kingdom <- Alphas_United_Kingdom[2]-0.15
Alpha3_United_Kingdom <- Alphas_United_Kingdom[3]

#valeurs initiales
OGbegint0_United_Kingdom <- cycle_United_Kingdom_hp[[3]]
OGbegint_moins1_United_Kingdom <- cycle_United_Kingdom_hp[[2]]


#préparation matrice variables d'observation du modèle espace etat

mat_obs_United_Kingdom <- matrix(, nrow = 179, ncol = 3)
mat_obs_United_Kingdom[,1] <- diff_lgdp_United_Kingdom
mat_obs_United_Kingdom[,2] <- United_Kingdom_infla_reg
mat_obs_United_Kingdom[,3] <- lag(United_Kingdom_infla_reg,1)
mat_obs_United_Kingdom <- na.omit(mat_obs_United_Kingdom)





#modele espace-etat multivar avec lags
B2_United_Kingdom <- matrix(list(b1_United_Kingdom, 1, b2_United_Kingdom, 0), 2, 2)
Z2_United_Kingdom <- matrix(list(1, Alpha3_United_Kingdom, 0, -1, 0, 0), nrow=3, ncol=2)
A2_United_Kingdom <- matrix(list(Delta1_United_Kingdom, Alpha1_United_Kingdom, 0), nrow=3, ncol=1)
Q2_United_Kingdom <- matrix(list("q1", 0, 0, 0), 2, 2)
u2_United_Kingdom <- matrix(list(0,0),nrow = 2, ncol = 1)
d2_United_Kingdom <- t(mat_obs_United_Kingdom)
D2_United_Kingdom <- matrix(list (0, 0, 0, 0, 0, 0, 0, Alpha2_United_Kingdom, 1), 3, 3)
R2_United_Kingdom <- matrix(list ("r11", 0, 0, 0, "r22", 0, 0, 0, 0.01), 3, 3)
x02_United_Kingdom <- matrix(list(OGbegint0_United_Kingdom, OGbegint_moins1_United_Kingdom), nrow = 2, ncol = 1)
model.list2_United_Kingdom <- list(B = B2_United_Kingdom, Q=Q2_United_Kingdom, Z = Z2_United_Kingdom, A = A2_United_Kingdom, d=d2_United_Kingdom, D=D2_United_Kingdom, U=u2_United_Kingdom, R=R2_United_Kingdom, x0= x02_United_Kingdom, tinitx = 1)
fit <- MARSS(d2_United_Kingdom, model=model.list2_United_Kingdom, fit = TRUE)
United_Kingdom_KF3 <- fitted(fit, type="ytT", interval = c("confidence"),level = 0.95, output = c("data.frame", "matrix"))
United_Kingdom_KF4 <- tsSmooth(fit,
                      type = c("xtt"),
                      interval = c("confidence"),
                      level = 0.95, fun.kf = c("MARSSkfas"))



PIB_POTENTIEL_KF_United_Kingdom <- logUnited_Kingdom - United_Kingdom_KF4$.estimate[1:180]/100
PIB_POTENTIEL_KF_United_Kingdomts <- ts(PIB_POTENTIEL_KF_United_Kingdom, start = c(1975, 1), frequency = 4)

# Plot time series
plot.ts(PIB_POTENTIEL_KF_United_Kingdomts, ylab = "", col = "black")  

# include HP trend
lines(United_Kingdomts, col = "red")
lines(trend_United_Kingdom_hpts, col = "blue")
legend("topleft", legend = c("PIB_Potentiel Kalman Filter United_Kingdom", "Log United_Kingdom", "HP trend"), lty = 1, 
       fill = c("black", "red","blue"), bty = "n")


#Detrend data with a linear filter

lin.mod_United_Kingdom <- lm(United_Kingdomts ~ time(United_Kingdomts))
lin.trend_United_Kingdom <- lin.mod_United_Kingdom$fitted.values  # fitted values pertain to time trend
linear_United_Kingdom <- ts(lin.trend_United_Kingdom, start = c(1975, 1), frequency = 4)  # create a time series variable for trend
lin.cycle_United_Kingdom <- United_Kingdomts - linear_United_Kingdom  # cycle is the difference between the data and linear trend

ts.plot(linear_United_Kingdom, United_Kingdomts, gpars = list(col = c("black", "red")))


# Baxter-King filter (Band pass filter)

United_Kingdom_bk <- bkfilter(United_Kingdomts,pl = 6, pu = 32)

cycle_United_Kingdom_bk<-United_Kingdom_bk$cycle
trend_United_Kingdom_bk<-United_Kingdom_bk$trend
United_Kingdomtss_bk<-United_Kingdom_bk$x

ts.plot(United_Kingdomtss_bk, trend_United_Kingdom_bk, gpars = list(col = c("black", "red")))




linear_trend_United_Kingdom<-ts(linear_United_Kingdom, start = c(1975, 1), frequency = 4)
trend_United_Kingdom_bkts<-ts(trend_United_Kingdom_bk, start = c(1975, 1), frequency = 4)

# Plot time series
plot.ts(PIB_POTENTIEL_KF_United_Kingdomts, ylab = "",col="black")  

# include HP trend
lines(United_Kingdomts, col = "red")
lines(trend_United_Kingdom_hpts, col = "blue")
lines(linear_trend_United_Kingdom, col = "green")
lines(trend_United_Kingdom_bkts, col = "orange")
legend("topleft", legend = c("GDP potential Kalman Filter United_Kingdom", "Log GDP United_Kingdom", "HP United_Kingdom trend","Linear United_Kingdom trend","Baxter-King United_Kingdom trend" ), lty = 1, 
       fill = c("black", "red","blue","green","orange"), bty = "n")



observed_United_Kingdom<-United_Kingdomts
predicted_United_Kingdom_Kalman<-PIB_POTENTIEL_KF_United_Kingdomts
predicted_United_Kingdom_HP<-trend_United_Kingdom_hpts
predicted_United_Kingdom_Linear<-linear_trend_United_Kingdom
predicted_United_Kingdom_Baxter_King<-trend_United_Kingdom_bkts



mae_Kalman_United_Kingdom<-mae(observed_United_Kingdom,predicted_United_Kingdom_Kalman)
mae_Kalman_United_Kingdom
mae_HP_United_Kingdom<-mae(observed_United_Kingdom,predicted_United_Kingdom_HP)
mae_HP_United_Kingdom
mae_Linear_United_Kingdom<-mae(observed_United_Kingdom,predicted_United_Kingdom_Linear)
mae_Linear_United_Kingdom
mae_Baxter_King_United_Kingdom<-mae(observed_United_Kingdom,predicted_United_Kingdom_Baxter_King)
mae_Baxter_King_United_Kingdom






diff_lgdp_United_Kingdom_ts<-ts(diff_lgdp_United_Kingdom/100, end = c(2020, 1), frequency = 4)
cycle_United_Kingdom_bk_ts<-ts(cycle_United_Kingdom_bk, end = c(2020, 1), frequency = 4)
cycle_United_Kingdom_linear_ts<-ts(lin.cycle_United_Kingdom, start = c(1975, 1), frequency = 4)
cycle_United_Kingdom_Kalman_a <- logUnited_Kingdom - PIB_POTENTIEL_KF_United_Kingdom
cycle_United_Kingdom_Kalman_ts<-ts(cycle_United_Kingdom_Kalman_a, end = c(2020, 1), frequency = 4)


# Plot time series
plot.ts(diff_lgdp_United_Kingdom_ts, ylab = "",col="black")  

# include HP trend
lines(cycle_United_Kingdom_Kalman_ts, col = "red")
lines(cycle_United_Kingdom_hpts, col = "blue")
lines(cycle_United_Kingdom_linear_ts, col = "green")
lines(cycle_United_Kingdom_bk_ts, col = "orange")
legend("topleft", legend = c("Log Delta GDP United_Kingdom","Cycle United_Kingdom Kalman Filter GDP", "HP United_Kingdom Cycle","Linear United_Kingdom cycle","Baxter-King United_Kingdom cycle" ), lty = 1, 
       fill = c("black", "red","blue","green","orange"), bty = "n")



observed_United_Kingdom_cycle<-diff_lgdp_United_Kingdom_ts
predicted_United_Kingdom_Kalman_cycle<-cycle_United_Kingdom_Kalman_ts
predicted_United_Kingdom_HP_cycle<-cycle_United_Kingdom_hpts
predicted_United_Kingdom_Linear_cycle<-cycle_United_Kingdom_linear_ts
predicted_United_Kingdom_Baxter_King_cycle<-cycle_United_Kingdom_bk_ts



mae_Kalman_United_Kingdom_cycle<- mae(observed_United_Kingdom,predicted_United_Kingdom_Kalman_cycle)
mae_Kalman_United_Kingdom_cycle
mae_HP_United_Kingdom_cycle<-mae(observed_United_Kingdom,predicted_United_Kingdom_HP_cycle)
mae_HP_United_Kingdom_cycle
mae_Linear_United_Kingdom_cycle<-mae(observed_United_Kingdom,predicted_United_Kingdom_Linear_cycle)
mae_Linear_United_Kingdom_cycle
mae_Baxter_King_United_Kingdom_cycle<-mae(observed_United_Kingdom,predicted_United_Kingdom_Baxter_King_cycle)
mae_Baxter_King_United_Kingdom_cycle



###United_States

#Frequence du parametre HP smoother

United_States<-gdp[[8]]
logUnited_States<-log(United_States)

United_States<-na.omit(United_States)

United_Statests<-ts(data=logUnited_States,start=(1975),frequency=4)     

United_Statests<- na.omit(United_Statests) 
plot(United_Statests)


United_States_hp <- hpfilter(United_Statests, freq = 1600, type = "lambda",drift=FALSE)

# Plot time series
plot.ts(United_Statests, ylab = "")  

# include HP trend
lines(United_States_hp$trend, col = "red")
legend("topleft", legend = c("Log GDP United_States", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")

#Plot cycle
plot.ts(United_States_hp$cycle, ylab = "") 
legend("topleft", legend = c("HP cycle United_States"), lty = 1, col = c("black"), 
       bty = "n")








logUnited_States<-na.omit(logUnited_States)
United_States_hp_reg <- hpfilter(logUnited_States, freq = 1600, type = "lambda",drift=FALSE)

cycle_United_States_hp<-United_States_hp_reg$cycle
trend_United_States_hp<-United_States_hp_reg$trend

cycle_United_States_hpts<-ts(data=cycle_United_States_hp,start=(1975),frequency=4)
trend_United_States_hpts<-ts(data=trend_United_States_hp,start=(1975),frequency=4)

cycle_United_States_hpts<-na.omit(cycle_United_States_hpts)
trend_United_States_hpts<-na.omit(trend_United_States_hpts)



diff_lgdp_United_States<-diff(logUnited_States)*100


United_States_infla <-df_infla[8]
United_States_inflats<-ts(data=United_States_infla,start=(1975),frequency=4)

United_States_inflats<- na.omit(United_States_inflats) 
United_States_infla <- matrix(data = United_States_inflats)






#Détermination des coefficients pour le modèle espace-etat 

#1ere Trend
trend_United_States<-lm(diff_lgdp_United_States~1+offset(diff(cycle_United_States_hp)))
summary(trend_United_States)


#3 eme equation Cycle

cycle_United_States<-lm(cycle_United_States_hp~0+lag(cycle_United_States_hp,1)+lag(cycle_United_States_hp,2))
summary(cycle_United_States)










trend_United_States_hp<-trend_United_States_hp[-188,]
trend_United_States_hp<-na.omit(trend_United_States_hp)

United_States_infla_reg<-United_States_infla[-1,]
United_States_infla_reg<-na.omit(United_States_infla_reg)

United_States_infla_reg<-na.omit(United_States_infla_reg) 


inflation_markup_model_United_States<-lm(United_States_infla_reg~lag(United_States_infla_reg,1)+trend_United_States_hp)
summary(inflation_markup_model_United_States)




#Nom des coefficients

#equation d'etat
B_United_States <- cycle_United_States$coefficients
b1_United_States <- B_United_States[1] 
b2_United_States <- B_United_States[2]

#equations d'observation
Delta1_United_States <- trend_United_States$coefficients 
Alphas_United_States <-inflation_markup_model_United_States$coefficients
Alpha1_United_States <- Alphas_United_States[1]
Alpha2_United_States <- Alphas_United_States[2]
Alpha3_United_States <- Alphas_United_States[3]

#valeurs initiales
OGbegint0_United_States <- cycle_United_States_hp[[3]]
OGbegint_moins1_United_States <- cycle_United_States_hp[[2]]


#préparation matrice variables d'observation du modèle espace etat

mat_obs_United_States <- matrix(, nrow = 187, ncol = 3)
mat_obs_United_States[,1] <- diff_lgdp_United_States
mat_obs_United_States[,2] <- United_States_infla_reg
mat_obs_United_States[,3] <- lag(United_States_infla_reg,1)
mat_obs_United_States <- na.omit(mat_obs_United_States)





#modele espace-etat multivar avec lags
B2_United_States <- matrix(list(b1_United_States, 1, b2_United_States, 0), 2, 2)
Z2_United_States <- matrix(list(1, Alpha3_United_States, 0, -1, 0, 0), nrow=3, ncol=2)
A2_United_States <- matrix(list(Delta1_United_States, Alpha1_United_States, 0), nrow=3, ncol=1)
Q2_United_States <- matrix(list("q1", 0, 0, 0), 2, 2)
u2_United_States <- matrix(list(0,0),nrow = 2, ncol = 1)
d2_United_States <- t(mat_obs_United_States)
D2_United_States <- matrix(list (0, 0, 0, 0, 0, 0, 0, Alpha2_United_States, 1), 3, 3)
R2_United_States <- matrix(list ("r11", 0, 0, 0, "r22", 0, 0, 0, 0.01), 3, 3)
x02_United_States <- matrix(list(OGbegint0_United_States, OGbegint_moins1_United_States), nrow = 2, ncol = 1)
model.list2_United_States <- list(B = B2_United_States, Q=Q2_United_States, Z = Z2_United_States, A = A2_United_States, d=d2_United_States, D=D2_United_States, U=u2_United_States, R=R2_United_States, x0= x02_United_States, tinitx = 1)
fit <- MARSS(d2_United_States, model=model.list2_United_States, fit = TRUE)
United_States_KF3 <- fitted(fit, type="ytT", interval = c("confidence"),level = 0.95, output = c("data.frame", "matrix"))
United_States_KF4 <- tsSmooth(fit,
                      type = c("xtt"),
                      interval = c("confidence"),
                      level = 0.95, fun.kf = c("MARSSkfas"))


PIB_POTENTIEL_KF_United_States <- logUnited_States - United_States_KF4$.estimate[1:188]/100
PIB_POTENTIEL_KF_United_Statests <- ts(PIB_POTENTIEL_KF_United_States, start = c(1975, 1), frequency = 4)



# Plot time series
plot.ts(PIB_POTENTIEL_KF_United_Statests, ylab = "black",col="black")  

# include HP trend
lines(United_Statests, col = "red")
lines(trend_United_States_hpts, col = "blue")
legend("topleft", legend = c("PIB_Potentiel Kalman Filter United_States", "Log United_States", "HP trend"),lty = 1, 
       fill = c("black", "red","blue"), bty = "n")





#Detrend data with a linear filter

lin.mod_United_States <- lm(United_Statests ~ time(United_Statests))
lin.trend_United_States <- lin.mod_United_States$fitted.values  # fitted values pertain to time trend
linear_United_States <- ts(lin.trend_United_States, start = c(1975, 1), frequency = 4)  # create a time series variable for trend
lin.cycle_United_States <- United_Statests - linear_United_States  # cycle is the difference between the data and linear trend

ts.plot(linear_United_States, United_Statests, gpars = list(col = c("black", "red")))

# Baxter-King filter (Band pass filter)

United_States_bk <- bkfilter(United_Statests,pl = 6, pu = 32)

cycle_United_States_bk<-United_States_bk$cycle
trend_United_States_bk<-United_States_bk$trend
United_Statestss_bk<-United_States_bk$x

ts.plot(United_Statestss_bk, trend_United_States_bk, gpars = list(col = c("black", "red")))






linear_trend_United_States<-ts(linear_United_States, start = c(1975, 1), frequency = 4)
trend_United_States_bkts<-ts(trend_United_States_bk, start = c(1975, 1), frequency = 4)

# Plot time series
plot.ts(PIB_POTENTIEL_KF_United_Statests, ylab = "",col="black")  

# include HP trend
lines(United_Statests, col = "red")
lines(trend_United_States_hpts, col = "blue")
lines(linear_trend_United_States, col = "green")
lines(trend_United_States_bkts, col = "orange")
legend("topleft", legend = c("GDP potential Kalman Filter United_States", "Log GDP United_States", "HP United_States trend","Linear United_States trend","Baxter-King United_States trend" ), lty = 1, 
       fill = c("black", "red","blue","green","orange"), bty = "n")



observed_United_States<-United_Statests
predicted_United_States_Kalman<-PIB_POTENTIEL_KF_United_Statests
predicted_United_States_HP<-trend_United_States_hpts
predicted_United_States_Linear<-linear_trend_United_States
predicted_United_States_Baxter_King<-trend_United_States_bkts



mae_Kalman_United_States<-mae(observed_United_States,predicted_United_States_Kalman)
mae_Kalman_United_States
mae_HP_United_States<-mae(observed_United_States,predicted_United_States_HP)
mae_HP_United_States
mae_Linear_United_States<-mae(observed_United_States,predicted_United_States_Linear)
mae_Linear_United_States
mae_Baxter_King_United_States<-mae(observed_United_States,predicted_United_States_Baxter_King)
mae_Baxter_King_United_States





diff_lgdp_United_States_ts<-ts(diff_lgdp_United_States/100, end = c(2022, 1), frequency = 4)
cycle_United_States_bk_ts<-ts(cycle_United_States_bk, end = c(2022, 1), frequency = 4)
cycle_United_States_linear_ts<-ts(lin.cycle_United_States, start = c(1975, 1), frequency = 4)
cycle_United_States_Kalman_a <- logUnited_States - PIB_POTENTIEL_KF_United_States
cycle_United_States_Kalman_ts<-ts(cycle_United_States_Kalman_a*10, end = c(2022, 1), frequency = 4)


# Plot time series
plot.ts(diff_lgdp_United_States_ts, ylab = "",col="black")  

# include HP trend
lines(cycle_United_States_Kalman_ts, col = "red")
lines(cycle_United_States_hpts, col = "blue")
lines(cycle_United_States_linear_ts, col = "green")
lines(cycle_United_States_bk_ts, col = "orange")
legend("topleft", legend = c("Log Delta GDP United_States","Cycle United_States Kalman Filter GDP", "HP United_States Cycle","Linear United_States cycle","Baxter-King United_States cycle" ), lty = 1, 
       fill = c("black", "red","blue","green","orange"), bty = "n")



observed_United_States_cycle<-diff_lgdp_United_States_ts
predicted_United_States_Kalman_cycle<-cycle_United_States_Kalman_ts
predicted_United_States_HP_cycle<-cycle_United_States_hpts
predicted_United_States_Linear_cycle<-cycle_United_States_linear_ts
predicted_United_States_Baxter_King_cycle<-cycle_United_States_bk_ts



mae_Kalman_United_States_cycle<- mae(observed_United_States,predicted_United_States_Kalman_cycle)
mae_Kalman_United_States_cycle
mae_HP_United_States_cycle<-mae(observed_United_States,predicted_United_States_HP_cycle)
mae_HP_United_States_cycle
mae_Linear_United_States_cycle<-mae(observed_United_States,predicted_United_States_Linear_cycle)
mae_Linear_United_States_cycle
mae_Baxter_King_United_States_cycle<-mae(observed_United_States,predicted_United_States_Baxter_King_cycle)
mae_Baxter_King_United_States_cycle





