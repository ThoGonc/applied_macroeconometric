#Tentative de replication du code eviews


library(readxl)
library(tseries)



rm(list = ls())
graphics.off()
MySheet <- read_excel("D:/2021-22/ENSAE_papiers/Cours/S2/2.1 . Applied Macroeconometrics/applied_macroeconometrics/filtering/pottest.xlsx")
