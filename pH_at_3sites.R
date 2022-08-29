library(readr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
library(dplyr)
library('xts') # use to convert dataframe to time series
library(zoo)
library(lmtest)
library(forecast)
library(aTSA)# test stationarity of the time series
library(fUnitRoots)
library(imputeTS)

source("https://gist.githubusercontent.com/ellisp/4002241def4e2b360189e58c3f461b4a/raw/9ab547bff18f73e783aaf30a7e4851c9a2f95b80/dualplot.R")

lol_na<-read.csv('C:/Users/Ryan_/Documents/R/pH_timeseries/lol_incomplete.csv')
lol1 <- na_kalman(lol_na, model = "auto.arima")
lol1["tide_height_na"][lol1["tide_height_na"] == "0"] <- NA
lol<-na_kalman(lol1, model = "StructTS")

ph_all<-read.csv('C:/Users/Ryan_/Documents/R/pH_timeseries/ph_across_sites.csv')
Alegria <- ph_all[ph_all$site=="Alegria",c(1,3,7,9,10)]
Bob <- ph_all[ph_all$site=="Bodega Bay",c(1,3,7,9,10)]
lol<-read.csv('C:/Users/Ryan_/Documents/R/pH_timeseries/lol.csv')

write.csv(Bob, 'C:/Users/Ryan_/Documents/R/pH_timeseries/bob.csv')
write.csv(Alegria, 'C:/Users/Ryan_/Documents/R/pH_timeseries/Alegria.csv')

y=strptime("2021-06-18 1:15:00","%Y-%m-%d %H:%M:%S")+900*1:11154 
total_ph=ts(lol_ph[,2],y, frequency = 96)
total_ph1=ts(lol_ph[,2],y, frequency = 1)
plot(total_ph)



lol$index<-1:11154

with(lol, dualplot(x1 = index, y1 = p_h, y2 = temp_c, 
                     colgrid = "grey90", ylim2 = c(10, 25)))
with(alg, dualplot(x1 = date_time, y1 = p_h, y2 = temp_c, 
                  colgrid = "grey90", ylim2 = c(10, 25)))
with(ll, dualplot(x1 = date_time, y1 = p_h, y2 = temp_c, 
                  colgrid = "grey90", ylim2 = c(10, 25)))



