install.packages("readxl")
library(RCurl)
urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/Data_applied.csv'
dsin<-read.csv2(urlfile, header=TRUE)