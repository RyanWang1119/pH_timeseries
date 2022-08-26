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


ph_<-read.csv('C:/Users/Ryan_/Documents/R/time series/files/ph_across_sites.csv')
total<-read.csv('C:/Users/Ryan_/Documents/R/time series/files/total.csv')
lol_ph<-total[,c(2,4)]
colnames(lol_ph)<-c('time', 'ph')
y=strptime("2021-06-18 1:15:00","%Y-%m-%d %H:%M:%S")+900*1:10643 
total_ph=ts(lol_ph[,2],y, frequency = 96)
total_ph1=ts(lol_ph[,2],y, frequency = 1)
plot(total_ph)

# Summary stats about the distribution of missing values
statsNA(total_ph)
ggplot_na_distribution(total_ph)

# Visualize the missing values in this time series
ggplot_na_intervals(total_ph)

# Visualize the top gap sizes / NAs in a row
ggplot_na_gapsize(total_ph)


# Mean Imputation
lol_ph<-total[,c(2,4)]
lol_ph$ph_mean<-na_mean(total_ph)
plot(na_mean(total_ph, option = "mean"), ylab = "ph", main = "Mean Imputation")

# Median Imputation
lol_ph$ph_median<-na_mean(total_ph, option = 'median')
plot(na_mean(total_ph, option = "median"), ylab = "ph", main = "Median Imputation")

# Mode Imputation
lol_ph$ph_mode<-na_mean(total_ph, option = 'mode')
plot(lol_ph$ph_mode, ylab = "ph", main = "Mode Imputation")
min(total_ph, na.rm = T) # minimum ph is 7.200782

# Imputation by Last Observation Carried Forward
lol_ph$ph_locf<-na_locf(total_ph, option = 'locf')
plot(lol_ph$ph_locf, ylab = "ph", main = "locf Imputation")

# Imputation by Last Observation Carried Forward
lol_ph$ph_nocb<-na_locf(total_ph, option = 'nocb')
plot(lol_ph$ph_nocb, ylab = "ph", main = "nocb Imputation")

# Seasonal Adjustment then LOCF
lol_ph$ph_s_locf<-na_seadec(total_ph, algorithm = "locf")
plot(lol_ph$ph_s_locf, ylab = "ph", main = "nocb Imputation")

# Imputation by na_kalman
lol_ph$imp <- na_kalman(total_ph1, model = "auto.arima")
ggplot_na_imputations(total_ph, lol_ph$imp)
ph<-lol_ph$imp
ph1<-lol_ph$ph_s_locf

plot(ph)

##### 2.2 acf-autocorrelation test #####
acf(ph,lag.max = 960)
pacf(ph, lag.max = 960)
# The autocorrelation has periodic fluctuation and decay slowly. Not stationary.

##### 2.3 adf-Unit root test #####
adf.test(as.numeric(ph))
# 0.525>0.05, fail to reject the null. Unit root exists. The times series is non-stationary.

#Show seasonal effect
plot(decompose(ph1))

decomposed <- stl(ph1, s.window = 'periodic')
seasonal <- decomposed$time.series[,1]
trend   <- decomposed$time.series[,2]
remainder <- decomposed$time.series[,3]

deseasoned_ph <- ph1 - seasonal - trend
deseasoned_ph
plot(deseasoned_ph)
plot(seasonal)
acf(deseasoned_ph, lag.max = 200) # Not stationary
pacf(deseasoned_ph, lag.max = 200)

# Take the first difference
acf(diff(deseasoned_ph), lag.max = 500) # 4 spikes follow. So, q for seasonal part is 4.
acf(diff(deseasoned_ph), lag.max = 100) # 4 spikes at first. So, q for non-seasonal part is 4. 
pacf(diff(deseasoned_ph), lag.max = 200) # Many spikes at the beginning and decay exponentially. So p = 0.


##### 2.5 White noise test #####
Box.test(diff(deseasoned_ph),type = "Ljung-Box",lag=6)
# 2.2e-16 <0.05 , reject the null, the time series is not a white noise.
# There is no correlation in a white noise series. No need to fit a model.

for (i in 1:5) {print(Box.test(diff(deseasoned_ph),lag = 96*i , type = "Ljung-Box"))}
# 2.2e-16 <0.05 , reject the null, the time series is not a white noise.


#### 3.modelling ####
a<-ph[1:500]
ts.plot(a)
auto.arima(ts(a, frequency = 96),D=1)
model_lol0 <- arima(a,order=c(0,0,1),seasonal = list(order=c(1,1,0),period=96))
model_lol1 <- arima(a,order=c(0,0,3),seasonal = list(order=c(0,1,3),period=96))

##### Test the model #####
AIC(model_lol0)
AIC(model_lol1)

tsdiag(model_lol1)
# Standardized residuals plot:
# The residuals have zero mean. If the residuals have a mean other than zero, then the forecasts are biased.

# ACF plot of the residual:
acf(model_lol1$residuals, lag.max = 400)
# The residuals are uncorrelated. If there are correlations between residuals, then there is information left in the residuals which should be used in computing forecasts.

# Ljung-Box test:
# H0: The residuals are independently distributed.
# HA: The residuals are not independently distributed; they exhibit serial correlation.

Box.test(model_lol1$residuals,lag=6)
# p-value = 0.9998. Fail to reject the null, the residual of the model is a white noise.


residual_lol0 <- residuals(model_lol0)
msft_fitted0 <- a - residual_lol0
ts.plot(a)
points(msft_fitted0, type = "l", col = 2, lty = 2)

residual_lol1 <- residuals(model_lol1)
msft_fitted1 <- a - residual_lol1
ts.plot(a)
points(msft_fitted1, type = "l", col = 2, lty = 2)

#### 4.Prediction ####
x_forecast_ph0 <- forecast::forecast(model_lol0,h=4*12,level=c(0.8,0.95))
x_forecast_ph1 <- forecast::forecast(model_lol1,h=4*12,level=c(0.8,0.95))

true_ph_a=(ph[1:548])
plot(x_forecast_ph0)
?par
par(mfrow=c(2,1))
plot(x_forecast_ph1)
ts.plot(true_ph)  

b<-ph[500:1000]
ts.plot(b)
model_lol2 <- arima(b,order=c(0,0,3),seasonal = list(order=c(0,1,3),period=96))

residual_lol2 <- residuals(model_lol2)
msft_fitted2 <- b - residual_lol2
ts.plot(b)
points(msft_fitted2, type = "l", col = 2, lty = 2)
x_forecast_ph2 <- forecast::forecast(model_lol2,h=4*12,level=c(0.8,0.95))

true_ph_b=(ph[500:1048])
plot(x_forecast_ph2)
ts.plot(true_ph_b)  

c<-ph[2000:2500]
ts.plot(c)

model_lol3 <- arima(c,order=c(0,0,3),seasonal = list(order=c(0,1,3),period=96))

residual_lol3 <- residuals(model_lol3)
msft_fitted3 <- c - residual_lol3
ts.plot(c)
points(msft_fitted3, type = "l", col = 2, lty = 2)
x_forecast_ph3 <- forecast::forecast(model_lol3,h=4*12,level=c(0.8,0.95))

true_ph_c<-(ph[1500:2048])
plot(x_forecast_ph3)
ts.plot(true_ph_c)

d<-ph[2500:3000]
ts.plot(d)

model_lol4 <- arima(d,order=c(0,0,3),seasonal = list(order=c(0,1,3),period=96))

residual_lol4 <- residuals(model_lol4)
msft_fitted4 <- d - residual_lol4
ts.plot(d)
points(msft_fitted4, type = "l", col = 2, lty = 2)
x_forecast_ph4 <- forecast::forecast(model_lol4,h=4*12,level=c(0.8,0.95))

true_ph_d<-(ph[2500:3048])
par(mfrow = c(2,1))
plot(x_forecast_ph4)
ts.plot(true_ph_d)  

e<-ph[4500:5000]
ts.plot(e)

model_lol5 <- arima(e,order=c(0,0,3),seasonal = list(order=c(0,1,3),period=96))

residual_lol5 <- residuals(model_lol5)
msft_fitted5 <- e - residual_lol5
ts.plot(e)
points(msft_fitted5, type = "l", col = 2, lty = 2)
x_forecast_ph5 <- forecast::forecast(model_lol5,h=4*12,level=c(0.8,0.95))

true_ph_e<-(ph[4500:5048])
plot(x_forecast_ph5)
ts.plot(true_ph_e)  
