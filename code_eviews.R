#Tentative de replication du code eviews


library(readxl)
library(tseries)
library(mFilter)



rm(list = ls())
graphics.off()
MySheet <- read_excel("D:/2021-22/ENSAE_papiers/Cours/S2/2.1 . Applied Macroeconometrics/applied_macroeconometrics/filtering/pottest.xlsx")

gdp<-MySheet[[3]]
infl<-MySheet[[5]]

lgdp<-log(gdp)
dlgdp<-100*diff(lgdp)


lgdp_hp<- hpfilter(lgdp, freq=1600,type="frequency",drift=FALSE)    #Vérifier s'il faut un drift ou pas
lgdpt_hp<-lgdp_hp$trend


lgdpc_hp= lgdp - lgdpt_hp
