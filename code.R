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

France_hp<- hpfilter(Francets, freq=100,type="frequency",drift=TRUE)
plot(France_hp)





Suede<-dsin[[7]]
Suedets<-ts(data=Suede,start=(1970),end=(2021),frequency=4)
Suede_hp<- hpfilter(Suedets, freq=100,type="frequency",drift=TRUE)

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