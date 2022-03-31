#install.packages("readxl")
install.packages("mfilter")
library(RCurl)
library(mFilter)

urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied_pib_volume.csv'
dsin<-read.csv2(urlfile, header=TRUE)
myts<-ts(data=dsin,start=(1970),end=(2022),frequency=4)
France<-dsin[[6]]


Francets<-ts(data=France,start=(1970),end=(2021),frequency=4)


France_hp<- hpfilter(Francets, freq=1600,type="frequency",drift=TRUE)

