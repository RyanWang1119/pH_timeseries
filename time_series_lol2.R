library('xts') # use to convert dataframe to time series
library(zoo)
library(lmtest)
library(forecast)
library(aTSA)# test stationarity of the time series
library(fUnitRoots)

ph_<-read.csv('C:/Users/Ryan_/Documents/R/time series/files/ph_across_sites.csv')

lol <- ph_[ph_$site=="Lompoc Landing",c(4,7)]
lol
y=strptime("2021-06-14 11:30:00","%Y-%m-%d %H:%M:%S")+900*1:8826 # 15 minutes time interval.
write.csv(lol,"C:/Users/Ryan_/Documents/R/time series/files//lol.csv", row.names = FALSE)

total=ts(lol[,2],y,start=1, end=8826,frequency=1)

# 1 to 500
power=ts(lol[1:500,4],y)
model_lol <- arima(power,order=c(0,1,4),seasonal = list(order=c(0,1,2),period=71))
residual_lol <- residuals(model_lol)
acf(residual_lol, lag.max = 80)

power1=ts(lol[1:200,4],y)
acf(diff(power1),lag.max = 80) # lag is 73

power11=ts(lol[301:500,4],y)
acf(diff(power11),lag.max = 80) # lag is 71

msft_fitted2 <- power - residual_lol
ts.plot(power)
points(msft_fitted2, type = "l", col = 2, lty = 2)

x_forecast1 <- forecast::forecast(model_lol,h=4*12,level=c(0.8,0.95))
true_ph1=ts(lol[1:538,4],y)
par(mfrow=c(2,1))
plot(x_forecast1)
plot(true_ph1)

# 500 to 1000
power3=ts(lol[501:1000,4],y)
acf(diff(power3), lag.max = 80)
total=ts(lol[,4],y)
model_lol_3 <- arima(power3,order=c(0,1,4),seasonal = list(order=c(0,1,2),period=74))
model_lol_33<-auto.arima(ts(power3,frequency=74),D=1)
model_lol_33

residual_lol3 <- residuals(model_lol_3)
residual_lol33 <- residuals(model_lol_33)
acf(residual_lol33, lag.max = 75)

power33=ts(lol[501:700,4],y)
acf(diff(power33),lag.max = 75) # lag is 74

power333=ts(lol[700:1000,4],y)
acf(diff(power333),lag.max = 80) # lag is 72

msft_fitted3 <- power3 - residual_lol33
ts.plot(power3)
points(msft_fitted3, type = "l", col = 2, lty = 2)

lines(fitted(model_lol_33),col="blue")

x_forecast3 <- forecast::forecast(model_lol_33,h=4*12,level=c(0.8,0.95))
true_ph3=ts(lol[501:1050,4],y)
par(mfrow=c(2,1))
plot(x_forecast3)
plot(true_ph3)

plot(forecast(auto.arima(ts(power3,frequency=74),D=1),h=48))

# 1000 to 1500
# no change in period
power4=ts(lol[1001:1500,4],y)
model_lol_4 <- arima(power4,order=c(0,1,4),seasonal = list(order=c(0,1,2),period=71))
model_lol_44<-auto.arima(ts(power4,frequency=71),D=1)
model_lol_44

residual_lol4 <- residuals(model_lol_4)
acf(residual_lol4,lag.max = 80)
msft_fitted4 <- power4 - residual_lol4
ts.plot(power4)
points(msft_fitted4, type = "l", col = 2, lty = 2)

x_forecast4 <- forecast::forecast(model_lol_4,h=4*12,level=c(0.8,0.95))
x_forecast44 <- forecast::forecast(model_lol_44,h=4*12,level=c(0.8,0.95))
true_ph4=ts(lol[1001:1548,4],y)
par(mfrow=c(2,1))
plot(x_forecast4)
plot(true_ph4)

plot(x_forecast44)

# 1500 to 2000
# change in period
power5=ts(lol[1501:2000,4],y)
acf(diff(power5),lag.max = 80)
model_lol_5 <- arima(power5,order=c(0,1,4),seasonal = list(order=c(0,1,2),period=74))
residual_lol5 <- residuals(model_lol_5)
msft_fitted5 <- power5 - residual_lol5

acf(residual_lol5, lag.max = 80)

power55=ts(lol[1501:1700,4],y)
acf(diff(power55),lag.max = 80) # lag is 78

power555=ts(lol[1701:2000,4],y)
acf(diff(power555),lag.max = 80) # lag is 73

ts.plot(power5)
points(msft_fitted5, type = "l", col = 2, lty = 2)

x_forecast5 <- forecast::forecast(model_lol_5,h=4*12,level=c(0.8,0.95))
true_ph5=ts(lol[1501:2048,4],y)
par(mfrow=c(2,1))
plot(x_forecast5)
plot(true_ph5)

# 4000 to 4500

power6=ts(lol[4001:4500,4],y)
model_lol_6 <- arima(power6,order=c(0,1,4),seasonal = list(order=c(0,1,2),period=76))
acf(model_lol_6$residuals,lag.max = 80)

power66=ts(lol[4001:4200,4],y)
acf(diff(power66),lag.max = 80) # lag is 75

power666=ts(lol[4210:4400,4],y)
acf(diff(power666),lag.max = 80) # lag is 77

power6666=ts(lol[4310:4500,4],y)
acf(diff(power6666),lag.max = 80) # lag is 79

residual_lol6 <- residuals(model_lol_6)
msft_fitted6 <- power6 - residual_lol6
ts.plot(power6)
points(msft_fitted6, type = "l", col = 2, lty = 2)

x_forecast6 <- forecast::forecast(model_lol_6,h=4*12,level=c(0.8,0.95))
true_ph6=ts(lol[4001:4548,4],y)
par(mfrow=c(2,1))
plot(x_forecast6)
plot(true_ph6)

# 5300:5900
power9=ts(lol[5000:6000,4],y)
plot(power9)
plot(diff(power9))
acf(diff(power9), lag.max = 80)
model_lol_9 <- arima(power9,order=c(0,1,4),seasonal = list(order=c(0,1,2),period=80))
residual_lol9 <- residuals(model_lol_9)
acf(residual_lol9, lag.max = 85)
msft_fitted9 <- power9 - residual_lol9
ts.plot(power9)
points(msft_fitted9, type = "l", col = 2, lty = 2)
lines(fitted(msft_fitted9),col="blue")

x_forecast9 <- forecast::forecast(model_lol_9,h=4*12,level=c(0.8,0.95))
true_ph9=ts(lol[5000:6048,4],y)
par(mfrow=c(2,1))
plot(x_forecast9)
plot(true_ph9)

# 7000 to 7500
power7=ts(lol[7001:7497,4],y)
model_lol_7 <- arima(power7,order=c(0,1,4),seasonal = list(order=c(0,1,2),period=76))
model_lol_77<-auto.arima(ts(power7,frequency=76),D=1)
acf(model_lol_77$residuals,lag.max = 500)

residual_lol77 <- residuals(model_lol_77)
msft_fitted77 <- power7 - residual_lol77
ts.plot(power7)
points(msft_fitted77, type = "l", col = 2, lty = 2)
lines(fitted(model_lol_77),col="blue")

x_forecast7 <- forecast::forecast(model_lol_7,h=4*12,level=c(0.8,0.95))
x_forecast77 <- forecast::forecast(model_lol_77,h=4*12,level=c(0.8,0.95))
true_ph7=ts(lol[7001:7548,4],y)
par(mfrow=c(2,1))
plot(x_forecast7)
plot(x_forecast77)
plot(true_ph7)

# 8000 to 8500
power8=ts(lol[8000:8500,4],y)
model_lol_8 <- arima(power8,order=c(0,1,4),seasonal = list(order=c(0,1,2),period=34))
model_lol_88<-auto.arima(ts(power8,frequency=34),D=1)

residual_lol8 <- residuals(model_lol_8)
acf(residual_lol8, lag.max = 80)
msft_fitted8 <- power8 - residual_lol8
ts.plot(power8)
points(msft_fitted8, type = "l", col = 2, lty = 2)
lines(fitted(model_lol_8),col="blue")

x_forecast8 <- forecast::forecast(model_lol_8,h=4*12,level=c(0.8,0.95))
x_forecast88<- forecast::forecast(model_lol_88,h=4*12,level=c(0.8,0.95))

true_ph8=ts(lol[8300:8848,4],y)
par(mfrow=c(2,1))
plot(x_forecast8)

model_lol_total<-auto.arima(ts(total,frequency=75),D=1)
model_lol_total1<-auto.arima(ts(total),D=1)
model_lol_total
ts.plot(total)
residual_lol_total <- residuals(model_lol_total1)
residual_lol_total
total

msft_fitted_total <- total - residual_lol_total
msft_fitted_total

points(msft_fitted_total, type = "l", col = 2, lty = 2)
lines(fitted(model_lol_total1),col="blue")

x_forecast_total <- forecast::forecast(model_lol_total,h=4*12,level=c(0.8,0.95))
plot(x_forecast_total)
