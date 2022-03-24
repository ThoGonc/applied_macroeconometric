install.packages("readxl")
library(RCurl)
urlfile<-'https://raw.githubusercontent.com/ThoGonc/applied_macroeconometric/main/TEST_2.csv'
dsin<-read.csv(urlfile)