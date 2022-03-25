#install.packages("readxl")
install.packages("mfilter")
library(RCurl)
library(mFilter)

urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied.csv'
dsin<-read.csv2(urlfile, header=TRUE)
myts<-ts(data=dsin,start=(1970),end=(2022),frequency=4)
France<-dsin[[5]]


Francets<-ts(data=France,start=(1970),end=(2022),frequency=4)


France_hp<- hpfilter(Francets, freq=1600,type="frequency",drift=TRUE)

