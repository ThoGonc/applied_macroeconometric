rm(list = ls())
graphics.off()

#install.packages("readxl")               #delete # to install packages 
#install.packages("mfilter")
#install.packages("Metrics")
##install.packages("tidyverse")
#install.packages("RCurl")
library(RCurl)
library(mFilter)
library(tidyverse)
library(MARSS)
library(Metrics)

#We upload the gdp quarterly on Github
urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_gdp_quarter_sa.csv'
gdp<-read.csv2(urlfile, header=TRUE)

#We upload the inflation on Github
urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_inflation.csv'
df_infla<-read.csv2(urlfile, header=TRUE)



###France


France<-gdp[[2]]
#We transform the GDP in log terms 
logFrance<-log(France)

France<-na.omit(France)
Francets<-ts(data=logFrance,start=(1975),frequency=4)     
Francets<- na.omit(Francets) 
plot(Francets)


#Application of the HP filter with lambda = 1600 
France_hp <- hpfilter(Francets, freq = 1600, type = "lambda",drift=FALSE)



# Plot time series
plot.ts(Francets, ylab = "Log GDP")  

# include HP trend
lines(France_hp$trend, col = "red")
legend("topleft", legend = c("Log GDP France", "HP trend"), lty = 1, 
       fill = c("black", "red"), bty = "n")

#Plot cycle
plot.ts(France_hp$cycle, ylab = "Cyclical component HP filter (log GDP)")
abline(h=0, col="blue")
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






#Determination of coefficients state space model 

#Trend equation
Trend_France<-lm(diff_lgdp_France~1+offset(diff(cycle_France_hp)))
summary(Trend_France)


#Cycle equation

cycle_France<-lm(cycle_France_hp~0+lag(cycle_France_hp,1)+lag(cycle_France_hp,2))
summary(cycle_France)

trend_France_hp<-trend_France_hp[-188,]
trend_France_hp<-na.omit(trend_France_hp)

France_infla_reg<-France_infla[-1,]
France_infla_reg<-na.omit(France_infla_reg)

France_infla_reg <- na.omit(France_infla_reg) 
France_infla<- na.omit(France_infla) 

#Inflation Markup model
inflation_markup_model_France<-lm(France_infla_reg~lag(France_infla_reg,1)+trend_France_hp)
summary(inflation_markup_model_France)




#Coefficient names

#State equation
B_France <- cycle_France$coefficients
b1_France <- B_France[1]
b2_France <- B_France[2]

#Measurement equation
Delta1_France <- Trend_France$coefficients
Alphas_France <-inflation_markup_model_France$coefficients
Alpha1_France <- Alphas_France[1]
Alpha2_France <- Alphas_France[2]
Alpha3_France <- Alphas_France[3]

#initial values from HP filtering
OGbegint0_France <- cycle_France_hp[[3]]
OGbegint_moins1_France <- cycle_France_hp[[2]]

# Observation variable matrix for the state space model
mat_obs_France <- matrix(, nrow = 187, ncol = 3)
mat_obs_France[,1] <- diff_lgdp_France
mat_obs_France[,2] <- France_infla_reg
mat_obs_France[,3] <- lag(France_infla_reg,1)
mat_obs_France <- na.omit(mat_obs_France)



#State space model multivarate with lags
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
                       type = c("xtt1"),
                       interval = c("confidence"),
                       level = 0.95, fun.kf = c("MARSSkfas"))



PIB_POTENTIEL_KF_France <- logFrance[2:187] - France_KF4$.estimate[1:186]/100
PIB_POTENTIEL_KF_Francets <- ts(PIB_POTENTIEL_KF_France, start = c(1975, 2), frequency = 4)

plot(PIB_POTENTIEL_KF_France)
plot(logFrance)



# Plot time series potential GDP 
plot.ts( PIB_POTENTIEL_KF_Francets, ylab = "Log GDP",col="red")  

# include GDP
lines(Francets, col = "black")
legend("topleft", legend = c("Kalman filter trend","Log GDP"), lty = 1, 
       fill = c("red", "black"), bty = "n")



# Plot time series
plot.ts(PIB_POTENTIEL_KF_Francets, ylab = "",col="black")  

# include HP trend and GDP
lines(Francets, col = "red")
lines(trend_France_hpts, col = "blue")
legend("topleft", legend = c("PIB_Potentiel Kalman Filter France", "Log France", "HP trend"), lty = 1, 
       fill = c("black", "red","blue"), bty = "n")



#Detrend data with a linear filter

lin.mod_France <- lm(Francets ~ time(Francets))
lin.trend_France <- lin.mod_France$fitted.values  # fitted values pertain to time trend
linear_France <- ts(lin.trend_France, start = c(1975, 1), frequency = 4)  # create a time series variable for trend
lin.cycle_France <- Francets - linear_France  # cycle is the difference between the data and linear trend

#Plot the linear trend with GDP
ts.plot(linear_France, Francets, gpars = list(col = c("black", "red")))


# Baxter-King filter (Band pass filter)

France_bk <- bkfilter(Francets,pl = 6, pu = 32)

cycle_France_bk<-France_bk$cycle
trend_France_bk<-France_bk$trend
francetss_bk<-France_bk$x

#Plot the trend from BK filter with GDP
ts.plot(francetss_bk, trend_France_bk, gpars = list(col = c("black", "red")))


linear_trend_France<-ts(linear_France, start = c(1975, 1), frequency = 4)
trend_France_bkts<-ts(trend_France_bk, start = c(1975, 1), frequency = 4)

# Plot time series potential GDP  
plot.ts(PIB_POTENTIEL_KF_Francets, ylab = "Log GDP",col="black")  

# include gdp
lines(Francets, col = "red")
# include HP trend
lines(trend_France_hpts, col = "blue")
# include linear trend
lines(linear_trend_France, col = "green")
# include BK trend
lines(trend_France_bkts, col = "orange")
legend("topleft", legend = c("GDP potential Kalman Filter","Log GDP", "GDP potential HP Filter","GDP potential Linear trend","GDP potential Baxter-King Filter" ), lty = 1, 
       fill = c("black","red", "blue","green","orange"), bty = "n")




observed_France<-Francets
predicted_France_Kalman<-PIB_POTENTIEL_KF_Francets
predicted_France_HP<-trend_France_hpts
predicted_France_Linear<-linear_trend_France
predicted_France_Baxter_King<-trend_France_bkts


#Calculation of MAE
mae_Kalman_France_trend<- mae(observed_France,predicted_France_Kalman)
mae_Kalman_France_trend
mae_HP_France_trend<-mae(observed_France,predicted_France_HP)
mae_HP_France_trend
mae_Linear_France_trend<-mae(observed_France,predicted_France_Linear)
mae_Linear_France_trend



diff_lgdp_France_ts<-ts(diff_lgdp_France/100, end = c(2022, 1), frequency = 4)
cycle_France_bk_ts<-ts(cycle_France_bk, end = c(2022, 1), frequency = 4)
cycle_France_linear_ts<-ts(lin.cycle_France, start = c(1975, 1), frequency = 4)
cycle_France_Kalman_a <- France_KF4$.estimate[1:186]/100
cycle_France_Kalman_ts<-ts(cycle_France_Kalman_a, end = c(2021, 4), frequency = 4)


# Plot time series cycle
plot.ts(cycle_France_linear_ts, ylab = "Output_Gap estimate",col="black")  

# include HP cycle
lines(cycle_France_hpts, col = "blue")
# include Kalman cycle
lines(cycle_France_Kalman_ts, col = "green")
# include BK cycle
lines(cycle_France_bk_ts, col = "orange")
legend("bottomleft", legend = c("Output Gap estimate linear trend", "Output Gap estimate HP filter" , "Output Gap estimate Kalman Filter","Output Gap estimate Baxter-King Filter" ), lty = 1, 
       fill = c("black", "blue","green","orange"), bty = "n")





#Plot Kalman cycle
plot.ts(cycle_France_Kalman_ts, ylab = "Cyclical component KF filter (log GDP)",col="black")  

# include HP trend
legend("topleft", legend = c("Kalman filter cycle"), lty = 1, 
       fill = c("black"), bty = "n")





###Germany  : This is the same code as for France with adjustments for sample size



Germany<-gdp[[3]]
Germany<-na.omit(Germany)

logGermany<-log(Germany)
Germanyts<-ts(data=logGermany,start=(1991),frequency=4)     

Germanyts<- na.omit(Germanyts) 
plot(Germanyts)


Germany_hp <- hpfilter(Germanyts, freq = 1600, type = "lambda",drift=FALSE)


plot.ts(Germanyts, ylab = "Log GDP")  
lines(Germany_hp$trend, col = "red")
legend("topleft", legend = c("Log GDP Germany", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")


plot.ts(Germany_hp$cycle, ylab = "Cyclical component HP filter (log GDP)") 
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



trend_Germany<-lm(diff_lgdp_Germany~1+offset(diff(cycle_Germany_hp)))
summary(trend_Germany)


cycle_Germany<-lm(cycle_Germany_hp~0+lag(cycle_Germany_hp,1)+lag(cycle_Germany_hp,2))
summary(cycle_Germany)


trend_Germany_hp<-trend_Germany_hp[-124,]
trend_Germany_hp<-na.omit(trend_Germany_hp)

Germany_infla_reg<-Germany_infla[-1,]
Germany_infla_reg<-na.omit(Germany_infla_reg)

Germany_infla_reg<-na.omit(Germany_infla_reg) 


inflation_markup_model_Germany<-lm(Germany_infla_reg~lag(Germany_infla_reg,1)+trend_Germany_hp)
summary(inflation_markup_model_Germany)


B_Germany <- cycle_Germany$coefficients
b1_Germany <- B_Germany[1]
b2_Germany <- B_Germany[2]


Delta1_Germany <- trend_Germany$coefficients
Alphas_Germany <-inflation_markup_model_Germany$coefficients
Alpha1_Germany <- Alphas_Germany[1] 
Alpha2_Germany <- Alphas_Germany[2]
Alpha3_Germany <- Alphas_Germany[3]


OGbegint0_Germany <- cycle_Germany_hp[[3]]
OGbegint_moins1_Germany <- cycle_Germany_hp[[2]]


mat_obs_Germany <- matrix(, nrow = 123, ncol = 3)
mat_obs_Germany[,1] <- diff_lgdp_Germany
mat_obs_Germany[,2] <- Germany_infla_reg
mat_obs_Germany[,3] <- lag(Germany_infla_reg,1)
mat_obs_Germany <- na.omit(mat_obs_Germany)

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
                       type = c("xtt1"),
                       interval = c("confidence"),
                       level = 0.95, fun.kf = c("MARSSkfas"))



PIB_POTENTIEL_KF_Germany <- logGermany[2:123] - Germany_KF4$.estimate[1:122]/100
PIB_POTENTIEL_KF_Germanyts <- ts(PIB_POTENTIEL_KF_Germany, start = c(1991, 2), frequency = 4)

plot(PIB_POTENTIEL_KF_Germany)
plot(logGermany)


plot.ts( PIB_POTENTIEL_KF_Germanyts, ylab = "Log GDP",col="red")  


lines(Germanyts, col = "black")
legend("topleft", legend = c("Kalman filter trend","Log GDP"), lty = 1, 
       fill = c("red", "black"), bty = "n")

plot.ts(PIB_POTENTIEL_KF_Germanyts, ylab = "",col="black")  


lines(Germanyts, col = "red")
lines(trend_Germany_hpts, col = "blue")
legend("topleft", legend = c("PIB_Potentiel Kalman Filter Germany", "Log Germany", "HP trend"), lty = 1, 
       fill = c("black", "red","blue"), bty = "n")


lin.mod_Germany <- lm(Germanyts ~ time(Germanyts))
lin.trend_Germany <- lin.mod_Germany$fitted.values  # fitted values pertain to time trend
linear_Germany <- ts(lin.trend_Germany, start = c(1991, 1), frequency = 4)  # create a time series variable for trend
lin.cycle_Germany <- Germanyts - linear_Germany  # cycle is the difference between the data and linear trend

ts.plot(linear_Germany, Germanyts, gpars = list(col = c("black", "red")))


Germany_bk <- bkfilter(Germanyts,pl = 6, pu = 32)

cycle_Germany_bk<-Germany_bk$cycle
trend_Germany_bk<-Germany_bk$trend
Germanytss_bk<-Germany_bk$x

ts.plot(Germanytss_bk, trend_Germany_bk, gpars = list(col = c("black", "red")))



linear_trend_Germany<-ts(linear_Germany, start = c(1991, 1), frequency = 4)
trend_Germany_bkts<-ts(trend_Germany_bk, start = c(1991, 1), frequency = 4)

plot.ts(PIB_POTENTIEL_KF_Germanyts, ylab = "Log GDP",col="black")  


lines(Germanyts, col = "red")
lines(trend_Germany_hpts, col = "blue")
lines(linear_trend_Germany, col = "green")
lines(trend_Germany_bkts, col = "orange")
legend("topleft", legend = c("GDP potential Kalman Filter","Log GDP", "GDP potential HP Filter","GDP potential Linear trend","GDP potential Baxter-King Filter" ), lty = 1, 
       fill = c("black","red", "blue","green","orange"), bty = "n")


observed_Germany<-Germanyts
predicted_Germany_Kalman<-PIB_POTENTIEL_KF_Germanyts
predicted_Germany_HP<-trend_Germany_hpts
predicted_Germany_Linear<-linear_trend_Germany
predicted_Germany_Baxter_King<-trend_Germany_bkts



mae_Kalman_Germany_trend<- mae(observed_Germany,predicted_Germany_Kalman)
mae_Kalman_Germany_trend
mae_HP_Germany_trend<-mae(observed_Germany,predicted_Germany_HP)
mae_HP_Germany_trend
mae_Linear_Germany_trend<-mae(observed_Germany,predicted_Germany_Linear)
mae_Linear_Germany_trend






diff_lgdp_Germany_ts<-ts(diff_lgdp_Germany/100, end = c(2022, 1), frequency = 4)
cycle_Germany_bk_ts<-ts(cycle_Germany_bk, end = c(2022, 1), frequency = 4)
cycle_Germany_linear_ts<-ts(lin.cycle_Germany, start = c(1991, 1), frequency = 4)
cycle_Germany_Kalman_a <- Germany_KF4$.estimate[1:122]/100
cycle_Germany_Kalman_ts<-ts(cycle_Germany_Kalman_a, end = c(2021, 4), frequency = 4)


plot.ts(cycle_Germany_hpts, ylab = "Output_Gap estimate",col="black")  

lines(cycle_Germany_Kalman_ts, col = "blue")
abline(h=0,col="red")
legend("bottomleft", legend = c( "Output Gap estimate HP filter","Output Gap estimate Kalman Filter"), lty = 1, 
       fill = c("black", "blue"), bty = "n")


plot.ts(cycle_Germany_linear_ts, ylab = "Output_Gap estimate",col="black")  


lines(cycle_Germany_hpts, col = "blue")
lines(cycle_Germany_Kalman_ts, col = "green")
lines(cycle_Germany_bk_ts, col = "orange")
legend("bottomleft", legend = c("Output Gap estimate linear trend","Output Gap estimate HP filter", "Output Gap estimate Kalman Filter","Output Gap estimate Baxter-King Filter" ), lty = 1, 
       fill = c("black", "blue","green","orange"), bty = "n")


plot.ts(cycle_Germany_Kalman_ts, ylab = "Cyclical component KF filter (log GDP)",col="black")  

# include HP trend
legend("topleft", legend = c("Kalman filter cycle"), lty = 1, 
       fill = c("black"), bty = "n")





###Italy This is the same code as for France with adjustments for sample size


Italy<-gdp[[4]]
logItaly<-log(Italy)

Italy<-na.omit(Italy)

Italyts<-ts(data=logItaly,start=(1975),frequency=4)     

Italyts<- na.omit(Italyts) 
plot(Italyts)


Italy_hp <- hpfilter(Italyts, freq = 1600, type = "lambda",drift=FALSE)

plot.ts(Italyts, ylab = "Log GDP")  

lines(Italy_hp$trend, col = "red")
legend("topleft", legend = c("Log GDP Italy", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")

plot.ts(Italy_hp$cycle, ylab = "Cyclical component HP filter (log GDP)") 
legend("topleft", legend = c("HP cycle Italy"), lty = 1, col = c("black"), 
       bty = "n")


logItaly<-na.omit(logItaly)
Italy_hp_reg <- hpfilter(logItaly, freq = 1600, type = "lambda",drift=FALSE)

cycle_Italy_hp<-Italy_hp_reg$cycle
trend_Italy_hp<-Italy_hp_reg$trend


cycle_Italy_hpts<-ts(data=cycle_Italy_hp,start=(1995),frequency=4)
trend_Italy_hpts<-ts(data=trend_Italy_hp,start=(1995),frequency=4)

cycle_Italy_hpts<-na.omit(cycle_Italy_hpts)
trend_Italy_hpts<-na.omit(trend_Italy_hpts)



diff_lgdp_Italy<-diff(logItaly)*100


Italy_infla <-df_infla[4]
Italy_inflats<-ts(data=Italy_infla,start=(1975),frequency=4)

Italy_inflats<- na.omit(Italy_inflats) 
Italy_infla <- matrix(data = Italy_inflats)


trend_Italy<-lm(diff_lgdp_Italy~1+offset(diff(cycle_Italy_hp)))
summary(trend_Italy)


cycle_Italy<-lm(cycle_Italy_hp~0+lag(cycle_Italy_hp,1)+lag(cycle_Italy_hp,2))
summary(cycle_Italy)


trend_Italy_hp<-trend_Italy_hp[-108,]
trend_Italy_hp<-na.omit(trend_Italy_hp)

Italy_infla_reg<-Italy_infla[-1,]
Italy_infla_reg<-na.omit(Italy_infla_reg)

Italy_infla_reg<-na.omit(Italy_infla_reg) 


inflation_markup_model_Italy<-lm(Italy_infla_reg~lag(Italy_infla_reg,1)+trend_Italy_hp)
summary(inflation_markup_model_Italy)


B_Italy <- cycle_Italy$coefficients
b1_Italy <- B_Italy[1]
b2_Italy <- B_Italy[2]


Delta1_Italy <- trend_Italy$coefficients
Alphas_Italy <-inflation_markup_model_Italy$coefficients
Alpha1_Italy <- Alphas_Italy[1] + 11.1
Alpha2_Italy <- Alphas_Italy[2] - 0.9
Alpha3_Italy <- Alphas_Italy[3] + 0.5


OGbegint0_Italy <- cycle_Italy_hp[[3]]
OGbegint_moins1_Italy <- cycle_Italy_hp[[2]]

mat_obs_Italy <- matrix(, nrow = 107, ncol = 3)
mat_obs_Italy[,1] <- diff_lgdp_Italy
mat_obs_Italy[,2] <- Italy_infla_reg
mat_obs_Italy[,3] <- lag(Italy_infla_reg,1)
mat_obs_Italy <- na.omit(mat_obs_Italy)



B2_Italy <- matrix(list(b1_Italy, 1, b2_Italy, 0), 2, 2)
Z2_Italy <- matrix(list(1, Alpha3_Italy, 0, -1, 0, 0), nrow=3, ncol=2)
A2_Italy <- matrix(list(Delta1_Italy, Alpha1_Italy, 0), nrow=3, ncol=1)
Q2_Italy <- matrix(list("q1", 0, 0, 0), 2, 2)
u2_Italy <- matrix(list(0,0),nrow = 2, ncol = 1)
d2_Italy <- t(mat_obs_Italy)
D2_Italy <- matrix(list (0, 0, 0, 0, 0, 0, 0, Alpha2_Italy, 1), 3, 3)
R2_Italy <- matrix(list ("r11", 0, 0, 0, "r22", 0, 0, 0, 0.01), 3, 3)
x02_Italy <- matrix(list(OGbegint0_Italy, OGbegint_moins1_Italy), nrow = 2, ncol = 1)
model.list2_Italy <- list(B = B2_Italy, Q=Q2_Italy, Z = Z2_Italy, A = A2_Italy, d=d2_Italy, D=D2_Italy, U=u2_Italy, R=R2_Italy, x0= x02_Italy, tinitx = 1)
fit <- MARSS(d2_Italy, model=model.list2_Italy, fit = TRUE)
Italy_KF3 <- fitted(fit, type="ytT", interval = c("confidence"),level = 0.95)
Italy_KF4 <- tsSmooth(fit,
                        type = c("xtt1"),
                        interval = c("confidence"),
                        level = 0.95, fun.kf = c("MARSSkfas"))

PIB_POTENTIEL_KF_Italy <- logItaly[2:107] - Italy_KF4$.estimate[1:106]/100
PIB_POTENTIEL_KF_Italyts <- ts(PIB_POTENTIEL_KF_Italy, start = c(1995, 2), frequency = 4)
plot(PIB_POTENTIEL_KF_Italy)
plot(logItaly)


plot.ts(PIB_POTENTIEL_KF_Italyts, ylab = "Log GDP",col="red")  


lines(Italyts, col = "black")
legend("topleft", legend = c("Kalman filter trend","Log GDP"), lty = 1, 
       fill = c("red", "black"), bty = "n")


plot.ts(cycle_Italy_linear_ts, ylab = "Output_Gap estimate",col="black")  


lines(cycle_Italy_hpts, col = "blue")
lines(cycle_Italy_Kalman_ts, col = "green")
lines(cycle_Italy_bk_ts, col = "orange")
legend("bottomleft", legend = c("Output Gap estimate linear trend","Output Gap estimate HP filter", "Output Gap estimate Kalman Filter","Output Gap estimate Baxter-King Filter" ), lty = 1, 
       fill = c("black", "blue","green","orange"), bty = "n")




lin.mod_Italy <- lm(Italyts ~ time(Italyts))
lin.trend_Italy <- lin.mod_Italy$fitted.values  # fitted values pertain to time trend
linear_Italy <- ts(lin.trend_Italy, start = c(1995, 1), frequency = 4)  # create a time series variable for trend
lin.cycle_Italy <- Italyts - linear_Italy  # cycle is the difference between the data and linear trend

ts.plot(linear_Italy, Italyts, gpars = list(col = c("black", "red")))



Italy_bk <- bkfilter(Italyts,pl = 6, pu = 32)

cycle_Italy_bk<-Italy_bk$cycle
trend_Italy_bk<-Italy_bk$trend
Italytss_bk<-Italy_bk$x

ts.plot(Italytss_bk, trend_Italy_bk, gpars = list(col = c("black", "red")))



linear_trend_Italy<-ts(linear_Italy, start = c(1995, 1), frequency = 4)
trend_Italy_bkts<-ts(trend_Italy_bk, start = c(1995, 1), frequency = 4)


plot.ts(PIB_POTENTIEL_KF_Italyts, ylab = "Log GDP",col="black")  

lines(Italyts, col = "red")
lines(trend_Italy_hpts, col = "blue")
lines(linear_trend_Italy, col = "green")
lines(trend_Italy_bkts, col = "orange")
legend("topleft", legend = c("GDP potential Kalman Filter","Log GDP", "GDP potential HP Filter","GDP potential Linear trend","GDP potential Baxter-King Filter" ), lty = 1, 
       fill = c("black","red", "blue","green","orange"), bty = "n")




observed_Italy<-Italyts
predicted_Italy_Kalman<-PIB_POTENTIEL_KF_Italyts
predicted_Italy_HP<-trend_Italy_hpts
predicted_Italy_Linear<-linear_trend_Italy
predicted_Italy_Baxter_King<-trend_Italy_bkts


mae_Kalman_Italy_trend<- mae(observed_Italy,predicted_Italy_Kalman)
mae_Kalman_Italy_trend
mae_HP_Italy_trend<-mae(observed_Italy,predicted_Italy_HP)
mae_HP_Italy_trend
mae_Linear_Italy_trend<-mae(observed_Italy,predicted_Italy_Linear)
mae_Linear_Italy_trend


diff_lgdp_Italy_ts<-ts(diff_lgdp_Italy/100, end = c(2022, 1), frequency = 4)
cycle_Italy_bk_ts<-ts(cycle_Italy_bk, end = c(2022, 1), frequency = 4)
cycle_Italy_linear_ts<-ts(lin.cycle_Italy, start = c(1995, 1), frequency = 4)
cycle_Italy_Kalman_a <- Italy_KF4$.estimate[1:106]/100
cycle_Italy_Kalman_ts<-ts(cycle_Italy_Kalman_a, end = c(2021, 4), frequency = 4)


plot.ts(cycle_Italy_linear_ts, ylab = "Output_Gap estimate",col="black")  

lines(cycle_Italy_hpts, col = "blue")
lines(cycle_Italy_Kalman_ts, col = "green")
lines(cycle_Italy_bk_ts, col = "orange")
legend("bottomleft", legend = c("Output Gap estimate HP filter", "Output Gap estimate linear trend", "Output Gap estimate Kalman Filter","Output Gap estimate Baxter-King Filter" ), lty = 1, 
       fill = c("black", "blue","green","orange"), bty = "n")



plot.ts(cycle_Italy_Kalman_ts, ylab = "Cyclical component KF filter (log GDP)",col="black")  

legend("topleft", legend = c("Kalman filter cycle"), lty = 1, 
       fill = c("black"), bty = "n")




###Spain This is the same code as for France with adjustments for sample size


Spain<-gdp[[5]]
Spain<-na.omit(Spain)

logSpain<-log(Spain)
Spaints<-ts(data=logSpain,start=(1995),frequency=4)     

Spaints<- na.omit(Spaints) 
plot(Spaints)


Spain_hp <- hpfilter(Spaints, freq = 1600, type = "lambda",drift=FALSE)

plot.ts(Spaints, ylab = "LOG GDP")  


lines(Spain_hp$trend, col = "red")
legend("topleft", legend = c("Log GDP Spain", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")

plot.ts(Spain_hp$cycle, ylab = "Cyclical component HP filter (log GDP)") 
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


trend_Spain<-lm(diff_lgdp_Spain~1+offset(diff(cycle_Spain_hp)))
summary(trend_Spain)


cycle_Spain<-lm(cycle_Spain_hp~0+lag(cycle_Spain_hp,1)+lag(cycle_Spain_hp,2))
summary(cycle_Spain)


trend_Spain_hp<-trend_Spain_hp[-108,]
trend_Spain_hp<-na.omit(trend_Spain_hp)

Spain_infla_reg<-Spain_infla[-1,]
Spain_infla_reg<-na.omit(Spain_infla_reg)

Spain_infla_reg<-na.omit(Spain_infla_reg) 


inflation_markup_model_Spain<-lm(Spain_infla_reg~lag(Spain_infla_reg,1)+trend_Spain_hp)
summary(inflation_markup_model_Spain)


B_Spain <- cycle_Spain$coefficients
b1_Spain <- B_Spain[1]
b2_Spain <- B_Spain[2]


Delta1_Spain <- trend_Spain$coefficients
Alphas_Spain <-inflation_markup_model_Spain$coefficients
Alpha1_Spain <- Alphas_Spain[1]
Alpha2_Spain <- Alphas_Spain[2]
Alpha3_Spain <- Alphas_Spain[3]


OGbegint0_Spain <- cycle_Spain_hp[[3]]
OGbegint_moins1_Spain <- cycle_Spain_hp[[2]]


mat_obs_Spain <- matrix(, nrow = 107, ncol = 3)
mat_obs_Spain[,1] <- diff_lgdp_Spain
mat_obs_Spain[,2] <- Spain_infla_reg
mat_obs_Spain[,3] <- lag(Spain_infla_reg,1)
mat_obs_Spain <- na.omit(mat_obs_Spain)


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
                       type = c("xtt1"),
                       interval = c("confidence"),
                       level = 0.95, fun.kf = c("MARSSkfas"))



PIB_POTENTIEL_KF_Spain <- logSpain[2:107] - Spain_KF4$.estimate[1:106]/100
PIB_POTENTIEL_KF_Spaints <- ts(PIB_POTENTIEL_KF_Spain, start = c(1995, 2), frequency = 4)



plot.ts( PIB_POTENTIEL_KF_Spaints, ylab = "Log GDP",col="red")  

lines(Spaints, col = "black")
legend("topleft", legend = c("Kalman filter trend","Log GDP"), lty = 1, 
       fill = c("red", "black"), bty = "n")


plot.ts(PIB_POTENTIEL_KF_Spaints, ylab = "",col="black")  

lines(Spaints, col = "red")
lines(trend_Spain_hpts, col = "blue")
legend("topleft", legend = c("PIB_Potentiel Kalman Filter Spain", "Log Spain", "HP trend"), lty = 1, 
       fill = c("black", "red","blue"), bty = "n")

lin.mod_Spain <- lm(Spaints ~ time(Spaints))
lin.trend_Spain <- lin.mod_Spain$fitted.values  # fitted values pertain to time trend
linear_Spain <- ts(lin.trend_Spain, start = c(1995, 1), frequency = 4)  # create a time series variable for trend
lin.cycle_Spain <- Spaints - linear_Spain  # cycle is the difference between the data and linear trend

ts.plot(linear_Spain, Spaints, gpars = list(col = c("black", "red")))

Spain_bk <- bkfilter(Spaints,pl = 6, pu = 32)

cycle_Spain_bk<-Spain_bk$cycle
trend_Spain_bk<-Spain_bk$trend
Spaintss_bk<-Spain_bk$x

ts.plot(Spaintss_bk, trend_Spain_bk, gpars = list(col = c("black", "red")))


linear_trend_Spain<-ts(linear_Spain, start = c(1995, 1), frequency = 4)
trend_Spain_bkts<-ts(trend_Spain_bk, start = c(1995, 1), frequency = 4)


plot.ts(PIB_POTENTIEL_KF_Spaints, ylab = "Log GDP",col="black")  


lines(Spaints, col = "red")
lines(trend_Spain_hpts, col = "blue")
lines(linear_trend_Spain, col = "green")
lines(trend_Spain_bkts, col = "orange")
legend("topleft", legend = c("GDP potential Kalman Filter","Log GDP", "GDP potential HP Filter","GDP potential Linear trend","GDP potential Baxter-King Filter" ), lty = 1, 
       fill = c("black","red", "blue","green","orange"), bty = "n")


observed_Spain<-Spaints
predicted_Spain_Kalman<-PIB_POTENTIEL_KF_Spaints
predicted_Spain_HP<-trend_Spain_hpts
predicted_Spain_Linear<-linear_trend_Spain
predicted_Spain_Baxter_King<-trend_Spain_bkts



mae_Kalman_Spain_trend<- mae(observed_Spain,predicted_Spain_Kalman)
mae_Kalman_Spain_trend
mae_HP_Spain_trend<-mae(observed_Spain,predicted_Spain_HP)
mae_HP_Spain_trend
mae_Linear_Spain_trend<-mae(observed_Spain,predicted_Spain_Linear)
mae_Linear_Spain_trend


diff_lgdp_Spain_ts<-ts(diff_lgdp_Spain/100, end = c(2022, 1), frequency = 4)
cycle_Spain_bk_ts<-ts(cycle_Spain_bk, end = c(2022, 1), frequency = 4)
cycle_Spain_linear_ts<-ts(lin.cycle_Spain, start = c(1995, 1), frequency = 4)
cycle_Spain_Kalman_a <- Spain_KF4$.estimate[1:106]/100
cycle_Spain_Kalman_ts<-ts(cycle_Spain_Kalman_a, end = c(2021, 4), frequency = 4)


plot.ts(cycle_Spain_linear_ts, ylab = "Output_Gap estimate",col="black")  

lines(cycle_Spain_hpts, col = "blue")
lines(cycle_Spain_Kalman_ts, col = "green")
lines(cycle_Spain_bk_ts, col = "orange")
legend("bottomleft", legend = c("Output Gap estimate linear trend","Output Gap estimate HP filter", "Output Gap estimate Kalman Filter","Output Gap estimate Baxter-King Filter" ), lty = 1, 
       fill = c("black", "blue","green","orange"), bty = "n")



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


plot.ts(cycle_Spain_Kalman_ts, ylab = "Cyclical component KF filter (log GDP)",col="black")  

legend("topleft", legend = c("Kalman filter cycle"), lty = 1, 
       fill = c("black"), bty = "n")



###Japan : This is the same code as for France with adjustments for sample size


Japan<-gdp[[6]]
Japan<-na.omit(Japan)

logJapan<-log(Japan)
Japants<-ts(data=logJapan,start=(1994),frequency=4)     

Japants<- na.omit(Japants) 
plot(Japants)

Japan_hp <- hpfilter(Japants, freq = 1600, type = "lambda",drift=FALSE)

plot.ts(Japants, ylab = "Log GDP")  

lines(Japan_hp$trend, col = "red")
legend("topleft", legend = c("Log GDP Japan", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")

plot.ts(Japan_hp$cycle, ylab = "Cyclical component HP filter (log GDP)") 
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


trend_Japan<-lm(diff_lgdp_Japan~1+offset(diff(cycle_Japan_hp)))
summary(trend_Japan)


cycle_Japan<-lm(cycle_Japan_hp~0+lag(cycle_Japan_hp,1)+lag(cycle_Japan_hp,2))
summary(cycle_Japan)


trend_Japan_hp<-trend_Japan_hp[-112,]
trend_Japan_hp<-na.omit(trend_Japan_hp)

Japan_infla_reg<-Japan_infla[-1,]
Japan_infla_reg<-na.omit(Japan_infla_reg)

Japan_infla_reg<-na.omit(Japan_infla_reg) 


inflation_markup_model_Japan<-lm(Japan_infla_reg~lag(Japan_infla_reg,1)+trend_Japan_hp)
summary(inflation_markup_model_Japan)


B_Japan <- cycle_Japan$coefficients
b1_Japan <- B_Japan[1]
b2_Japan <- B_Japan[2]


Delta1_Japan <- trend_Japan$coefficients
Alphas_Japan <-inflation_markup_model_Japan$coefficients
Alpha1_Japan <- Alphas_Japan[1] + 9.5
Alpha2_Japan <- Alphas_Japan[2] -0.4
Alpha3_Japan <- Alphas_Japan[3]

OGbegint0_Japan <- cycle_Japan_hp[[3]]
OGbegint_moins1_Japan <- cycle_Japan_hp[[2]]

mat_obs_Japan <- matrix(, nrow = 111, ncol = 3)
mat_obs_Japan[,1] <- diff_lgdp_Japan
mat_obs_Japan[,2] <- Japan_infla_reg
mat_obs_Japan[,3] <- lag(Japan_infla_reg,1)
mat_obs_Japan <- na.omit(mat_obs_Japan)

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
                       type = c("xtt1"),
                       interval = c("confidence"),
                       level = 0.95, fun.kf = c("MARSSkfas"))

PIB_POTENTIEL_KF_Japan <- logJapan[2:111] - Japan_KF4$.estimate[1:110]/100
PIB_POTENTIEL_KF_Japants <- ts(PIB_POTENTIEL_KF_Japan, start = c(1994, 2), frequency = 4)

plot.ts( PIB_POTENTIEL_KF_Japants, ylab = "Log GDP",col="red")  


lines(Japants, col = "black")
legend("topleft", legend = c("Kalman filter trend","Log GDP"), lty = 1, 
       fill = c("red", "black"), bty = "n")


plot.ts(PIB_POTENTIEL_KF_Japants, ylab = "", col ="black")  

lines(Japants, col = "red")
lines(trend_Japan_hpts, col = "blue")
legend("topleft", legend = c("PIB_Potentiel Kalman Filter Japan", "Log Japan", "HP trend"), lty = 1, 
       fill = c("black", "red","blue"), bty = "n")

lin.mod_Japan <- lm(Japants ~ time(Japants))
lin.trend_Japan <- lin.mod_Japan$fitted.values  # fitted values pertain to time trend
linear_Japan <- ts(lin.trend_Japan, start = c(1994, 1), frequency = 4)  # create a time series variable for trend
lin.cycle_Japan <- Japants - linear_Japan  # cycle is the difference between the data and linear trend

ts.plot(linear_Japan, Japants, gpars = list(col = c("black", "red")))


Japan_bk <- bkfilter(Japants,pl = 6, pu = 32)

cycle_Japan_bk<-Japan_bk$cycle
trend_Japan_bk<-Japan_bk$trend
Japantss_bk<-Japan_bk$x

ts.plot(Japantss_bk, trend_Japan_bk, gpars = list(col = c("black", "red")))


linear_trend_Japan<-ts(linear_Japan, start = c(1994, 1), frequency = 4)
trend_Japan_bkts<-ts(trend_Japan_bk, start = c(1994, 1), frequency = 4)

plot.ts(PIB_POTENTIEL_KF_Japants, ylab = "Log GDP",col="black")  

lines(Japants, col = "red")
lines(trend_Japan_hpts, col = "blue")
lines(linear_trend_Japan, col = "green")
lines(trend_Japan_bkts, col = "orange")
legend("topleft", legend = c("GDP potential Kalman Filter","Log GDP", "GDP potential HP Filter","GDP potential Linear trend","GDP potential Baxter-King Filter" ), lty = 1, 
       fill = c("black","red", "blue","green","orange"), bty = "n")


observed_Japan<-Japants
predicted_Japan_Kalman<-PIB_POTENTIEL_KF_Japants
predicted_Japan_HP<-trend_Japan_hpts
predicted_Japan_Linear<-linear_trend_Japan
predicted_Japan_Baxter_King<-trend_Japan_bkts



mae_Kalman_Japan_trend<- mae(observed_Japan,predicted_Japan_Kalman)
mae_Kalman_Japan_trend
mae_HP_Japan_trend<-mae(observed_Japan,predicted_Japan_HP)
mae_HP_Japan_trend
mae_Linear_Japan_trend<-mae(observed_Japan,predicted_Japan_Linear)
mae_Linear_Japan_trend


diff_lgdp_Japan_ts<-ts(diff_lgdp_Japan/100, end = c(2022, 1), frequency = 4)
cycle_Japan_bk_ts<-ts(cycle_Japan_bk, end = c(2022, 1), frequency = 4)
cycle_Japan_linear_ts<-ts(lin.cycle_Japan, start = c(1994, 1), frequency = 4)
cycle_Japan_Kalman_a <- Japan_KF4$.estimate[1:110]/100
cycle_Japan_Kalman_ts<-ts(cycle_Japan_Kalman_a, end = c(2021, 4), frequency = 4)


plot.ts(cycle_Japan_linear_ts, ylab = "Output_Gap estimate",col="black")  

lines(cycle_Japan_hpts, col = "blue")
lines(cycle_Japan_Kalman_ts, col = "green")
lines(cycle_Japan_bk_ts, col = "orange")
legend("bottomleft", legend = c("Output Gap estimate linear trend","Output Gap estimate HP filter", "Output Gap estimate Kalman Filter","Output Gap estimate Baxter-King Filter" ), lty = 1, 
       fill = c("black", "blue","green","orange"), bty = "n")

plot.ts(cycle_Japan_Kalman_ts, ylab = "Cyclical component KF filter (log GDP)",col="black")  

legend("topleft", legend = c("Kalman filter cycle"), lty = 1, 
       fill = c("black"), bty = "n")







###United_Kingdom : This is the same code as for France with adjustments for sample size


United_Kingdom<-gdp[[7]]
logUnited_Kingdom<-log(United_Kingdom)

United_Kingdom<-na.omit(United_Kingdom)

United_Kingdomts<-ts(data=logUnited_Kingdom,start=(1975),frequency=4)     

United_Kingdomts<- na.omit(United_Kingdomts) 

plot(United_Kingdomts)


United_Kingdom_hp <- hpfilter(United_Kingdomts, freq = 1600, type = "lambda",drift=FALSE)

plot.ts(United_Kingdomts, ylab = "Log GDP")  


lines(United_Kingdom_hp$trend, col = "red")
legend("topleft", legend = c("Log GDP United_Kingdom", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")

plot.ts(United_Kingdom_hp$cycle, ylab = "Cyclical component HP filter (log GDP)") 
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

trend_United_Kingdom<-lm(diff_lgdp_United_Kingdom~1+offset(diff(cycle_United_Kingdom_hp)))
summary(trend_United_Kingdom)


cycle_United_Kingdom<-lm(cycle_United_Kingdom_hp~0+lag(cycle_United_Kingdom_hp,1)+lag(cycle_United_Kingdom_hp,2))
summary(cycle_United_Kingdom)


trend_United_Kingdom_hp<-trend_United_Kingdom_hp[-180,]
trend_United_Kingdom_hp<-na.omit(trend_United_Kingdom_hp)

United_Kingdom_infla_reg<-United_Kingdom_infla[-1,]
United_Kingdom_infla_reg<-na.omit(United_Kingdom_infla_reg)


inflation_markup_model_United_Kingdom<-lm(United_Kingdom_infla_reg~lag(United_Kingdom_infla_reg,1)+trend_United_Kingdom_hp)
summary(inflation_markup_model_United_Kingdom)


B_United_Kingdom <- cycle_United_Kingdom$coefficients
b1_United_Kingdom <- B_United_Kingdom[1]
b2_United_Kingdom <- B_United_Kingdom[2]


Delta1_United_Kingdom <- trend_United_Kingdom$coefficients
Alphas_United_Kingdom <-inflation_markup_model_United_Kingdom$coefficients
Alpha1_United_Kingdom <- Alphas_United_Kingdom[1]-5.1
Alpha2_United_Kingdom <- Alphas_United_Kingdom[2]-0.15
Alpha3_United_Kingdom <- Alphas_United_Kingdom[3]


OGbegint0_United_Kingdom <- cycle_United_Kingdom_hp[[3]]
OGbegint_moins1_United_Kingdom <- cycle_United_Kingdom_hp[[2]]


mat_obs_United_Kingdom <- matrix(, nrow = 179, ncol = 3)
mat_obs_United_Kingdom[,1] <- diff_lgdp_United_Kingdom
mat_obs_United_Kingdom[,2] <- United_Kingdom_infla_reg
mat_obs_United_Kingdom[,3] <- lag(United_Kingdom_infla_reg,1)
mat_obs_United_Kingdom <- na.omit(mat_obs_United_Kingdom)


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
                      type = c("xtt1"),
                      interval = c("confidence"),
                      level = 0.95, fun.kf = c("MARSSkfas"))

PIB_POTENTIEL_KF_United_Kingdom <- logUnited_Kingdom[2:179] - United_Kingdom_KF4$.estimate[1:178]/100
PIB_POTENTIEL_KF_United_Kingdomts <- ts(PIB_POTENTIEL_KF_United_Kingdom, start = c(1975, 1), frequency = 4)


plot.ts( PIB_POTENTIEL_KF_United_Kingdomts, ylab = "Log GDP",col="red")  


lines(United_Kingdomts, col = "black")
legend("topleft", legend = c("Kalman filter trend","Log GDP"), lty = 1, 
       fill = c("red", "black"), bty = "n")


plot.ts(PIB_POTENTIEL_KF_United_Kingdomts, ylab = "", col = "black")  

lines(United_Kingdomts, col = "red")
lines(trend_United_Kingdom_hpts, col = "blue")
legend("topleft", legend = c("PIB_Potentiel Kalman Filter United_Kingdom", "Log United_Kingdom", "HP trend"), lty = 1, 
       fill = c("black", "red","blue"), bty = "n")

lin.mod_United_Kingdom <- lm(United_Kingdomts ~ time(United_Kingdomts))
lin.trend_United_Kingdom <- lin.mod_United_Kingdom$fitted.values  # fitted values pertain to time trend
linear_United_Kingdom <- ts(lin.trend_United_Kingdom, start = c(1975, 1), frequency = 4)  # create a time series variable for trend
lin.cycle_United_Kingdom <- United_Kingdomts - linear_United_Kingdom  # cycle is the difference between the data and linear trend

ts.plot(linear_United_Kingdom, United_Kingdomts, gpars = list(col = c("black", "red")))

United_Kingdom_bk <- bkfilter(United_Kingdomts,pl = 6, pu = 32)

cycle_United_Kingdom_bk<-United_Kingdom_bk$cycle
trend_United_Kingdom_bk<-United_Kingdom_bk$trend
United_Kingdomtss_bk<-United_Kingdom_bk$x

ts.plot(United_Kingdomtss_bk, trend_United_Kingdom_bk, gpars = list(col = c("black", "red")))

linear_trend_United_Kingdom<-ts(linear_United_Kingdom, start = c(1975, 1), frequency = 4)
trend_United_Kingdom_bkts<-ts(trend_United_Kingdom_bk, start = c(1975, 1), frequency = 4)

plot.ts(PIB_POTENTIEL_KF_United_Kingdomts, ylab = "Log GDP",col="black")  


lines(United_Kingdomts, col = "red")
lines(trend_United_Kingdom_hpts, col = "blue")
lines(linear_trend_United_Kingdom, col = "green")
lines(trend_United_Kingdom_bkts, col = "orange")
legend("topleft", legend = c("GDP potential Kalman Filter","Log GDP", "GDP potential HP Filter","GDP potential Linear trend","GDP potential Baxter-King Filter" ), lty = 1, 
       fill = c("black","red", "blue","green","orange"), bty = "n")



observed_United_Kingdom<-United_Kingdomts
predicted_United_Kingdom_Kalman<-PIB_POTENTIEL_KF_United_Kingdomts
predicted_United_Kingdom_HP<-trend_United_Kingdom_hpts
predicted_United_Kingdom_Linear<-linear_trend_United_Kingdom
predicted_United_Kingdom_Baxter_King<-trend_United_Kingdom_bkts



mae_Kalman_United_Kingdom_trend<- mae(observed_United_Kingdom,predicted_United_Kingdom_Kalman)
mae_Kalman_United_Kingdom_trend
mae_HP_United_Kingdom_trend<-mae(observed_United_Kingdom,predicted_United_Kingdom_HP)
mae_HP_United_Kingdom_trend
mae_Linear_United_Kingdom_trend<-mae(observed_United_Kingdom,predicted_United_Kingdom_Linear)
mae_Linear_United_Kingdom_trend







diff_lgdp_United_Kingdom_ts<-ts(diff_lgdp_United_Kingdom/100, end = c(2020, 1), frequency = 4)
cycle_United_Kingdom_bk_ts<-ts(cycle_United_Kingdom_bk, end = c(2020, 1), frequency = 4)
cycle_United_Kingdom_linear_ts<-ts(lin.cycle_United_Kingdom, start = c(1975, 1), frequency = 4)
cycle_United_Kingdom_Kalman_a <- United_Kingdom_KF4$.estimate[1:178]/100
cycle_United_Kingdom_Kalman_ts<-ts(cycle_United_Kingdom_Kalman_a, end = c(2019, 4), frequency = 4)


plot.ts(cycle_United_Kingdom_linear_ts, ylab = "Output_Gap estimate",col="black")  

lines(cycle_United_Kingdom_hpts, col = "blue")
lines(cycle_United_Kingdom_Kalman_ts, col = "green")
lines(cycle_United_Kingdom_bk_ts, col = "orange")
legend("topright", legend = c("Output Gap estimate linear trend","Output Gap estimate HP filter", "Output Gap estimate Kalman Filter","Output Gap estimate Baxter-King Filter" ), lty = 1, 
       fill = c("black", "blue","green","orange"), bty = "n")



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


plot.ts(cycle_United_Kingdom_Kalman_ts, ylab = "Cyclical component KF filter (log GDP)",col="black")  

legend("topleft", legend = c("Kalman filter cycle"), lty = 1, 
       fill = c("black"), bty = "n")







###United_States : This is the same code as for France with adjustments for sample size

United_States<-gdp[[8]]
logUnited_States<-log(United_States)

United_States<-na.omit(United_States)

United_Statests<-ts(data=logUnited_States,start=(1975),frequency=4)     

United_Statests<- na.omit(United_Statests) 
plot(United_Statests)

United_States_hp <- hpfilter(United_Statests, freq = 1600, type = "lambda",drift=FALSE)

plot.ts(United_Statests, ylab = "Log GDP")  

lines(United_States_hp$trend, col = "red")
legend("topleft", legend = c("Log GDP United_States", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n")

plot.ts(United_States_hp$cycle, ylab = "Cyclical component HP filter (log GDP)") 
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


trend_United_States<-lm(diff_lgdp_United_States~1+offset(diff(cycle_United_States_hp)))
summary(trend_United_States)


cycle_United_States<-lm(cycle_United_States_hp~0+lag(cycle_United_States_hp,1)+lag(cycle_United_States_hp,2))
summary(cycle_United_States)


trend_United_States_hp<-trend_United_States_hp[-188,]
trend_United_States_hp<-na.omit(trend_United_States_hp)

United_States_infla_reg<-United_States_infla[-1,]
United_States_infla_reg<-na.omit(United_States_infla_reg)

United_States_infla_reg<-na.omit(United_States_infla_reg) 


inflation_markup_model_United_States<-lm(United_States_infla_reg~lag(United_States_infla_reg,1)+trend_United_States_hp)
summary(inflation_markup_model_United_States)


B_United_States <- cycle_United_States$coefficients
b1_United_States <- B_United_States[1] 
b2_United_States <- B_United_States[2]

Delta1_United_States <- trend_United_States$coefficients 
Alphas_United_States <-inflation_markup_model_United_States$coefficients
Alpha1_United_States <- Alphas_United_States[1]
Alpha2_United_States <- Alphas_United_States[2]
Alpha3_United_States <- Alphas_United_States[3]

OGbegint0_United_States <- cycle_United_States_hp[[3]]
OGbegint_moins1_United_States <- cycle_United_States_hp[[2]]

mat_obs_United_States <- matrix(, nrow = 187, ncol = 3)
mat_obs_United_States[,1] <- diff_lgdp_United_States
mat_obs_United_States[,2] <- United_States_infla_reg
mat_obs_United_States[,3] <- lag(United_States_infla_reg,1)
mat_obs_United_States <- na.omit(mat_obs_United_States)


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
                      type = c("xtt1"),
                      interval = c("confidence"),
                      level = 0.95, fun.kf = c("MARSSkfas"))


PIB_POTENTIEL_KF_United_States <- logUnited_States[2:187] - United_States_KF4$.estimate[1:186]/100
PIB_POTENTIEL_KF_United_Statests <- ts(PIB_POTENTIEL_KF_United_States, start = c(1975, 2), frequency = 4)


plot.ts( PIB_POTENTIEL_KF_United_Statests, ylab = "Log GDP",col="red")  

lines(United_Statests, col = "black")
legend("topleft", legend = c("Kalman filter trend","Log GDP"), lty = 1, 
       fill = c("red", "black"), bty = "n")


plot.ts(PIB_POTENTIEL_KF_United_Statests, ylab = "black",col="black")  

lines(United_Statests, col = "red")
lines(trend_United_States_hpts, col = "blue")
legend("topleft", legend = c("PIB_Potentiel Kalman Filter United_States", "Log United_States", "HP trend"),lty = 1, 
       fill = c("black", "red","blue"), bty = "n")



lin.mod_United_States <- lm(United_Statests ~ time(United_Statests))
lin.trend_United_States <- lin.mod_United_States$fitted.values  # fitted values pertain to time trend
linear_United_States <- ts(lin.trend_United_States, start = c(1975, 1), frequency = 4)  # create a time series variable for trend
lin.cycle_United_States <- United_Statests - linear_United_States  # cycle is the difference between the data and linear trend

ts.plot(linear_United_States, United_Statests, gpars = list(col = c("black", "red")))


United_States_bk <- bkfilter(United_Statests,pl = 6, pu = 32)

cycle_United_States_bk<-United_States_bk$cycle
trend_United_States_bk<-United_States_bk$trend
United_Statestss_bk<-United_States_bk$x

ts.plot(United_Statestss_bk, trend_United_States_bk, gpars = list(col = c("black", "red")))

linear_trend_United_States<-ts(linear_United_States, start = c(1975, 1), frequency = 4)
trend_United_States_bkts<-ts(trend_United_States_bk, start = c(1975, 1), frequency = 4)


plot.ts(PIB_POTENTIEL_KF_United_Statests, ylab = "Log GDP",col="black")  

lines(United_Statests, col = "red")
lines(trend_United_States_hpts, col = "blue")
lines(linear_trend_United_States, col = "green")
lines(trend_United_States_bkts, col = "orange")
legend("topleft", legend = c("GDP potential Kalman Filter","Log GDP", "GDP potential HP Filter","GDP potential Linear trend","GDP potential Baxter-King Filter" ), lty = 1, 
       fill = c("black","red", "blue","green","orange"), bty = "n")




observed_United_States<-United_Statests
predicted_United_States_Kalman<-PIB_POTENTIEL_KF_United_Statests
predicted_United_States_HP<-trend_United_States_hpts
predicted_United_States_Linear<-linear_trend_United_States
predicted_United_States_Baxter_King<-trend_United_States_bkts



mae_Kalman_United_States_trend<- mae(observed_United_States,predicted_United_States_Kalman)
mae_Kalman_United_States_trend
mae_HP_United_States_trend<-mae(observed_United_States,predicted_United_States_HP)
mae_HP_United_States_trend
mae_Linear_United_States_trend<-mae(observed_United_States,predicted_United_States_Linear)
mae_Linear_United_States_trend



diff_lgdp_United_States_ts<-ts(diff_lgdp_United_States/100, end = c(2022, 1), frequency = 4)
cycle_United_States_bk_ts<-ts(cycle_United_States_bk, end = c(2022, 1), frequency = 4)
cycle_United_States_linear_ts<-ts(lin.cycle_United_States, start = c(1975, 1), frequency = 4)
cycle_United_States_Kalman_a <- United_States_KF4$.estimate[1:186]/100
cycle_United_States_Kalman_ts<-ts(cycle_United_States_Kalman_a*10, end = c(2021, 4), frequency = 4)


plot.ts(cycle_United_States_linear_ts, ylab = "Output_Gap estimate",col="black")  

lines(cycle_United_States_hpts, col = "blue")
lines(cycle_United_States_Kalman_ts, col = "green")
lines(cycle_United_States_bk_ts, col = "orange")
legend("bottomleft", legend = c("Output Gap estimate linear trend","Output Gap estimate HP filter", "Output Gap estimate Kalman Filter","Output Gap estimate Baxter-King Filter" ), lty = 1, 
       fill = c("black", "blue","green","orange"), bty = "n")



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


plot.ts(cycle_United_States_Kalman_ts, ylab = "Cyclical component KF filter (log GDP)",col="black")  

legend("topleft", legend = c("Kalman filter cycle"), lty = 1, 
       fill = c("black"), bty = "n")




