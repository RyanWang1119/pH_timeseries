residual_lol_temp1 <- residuals(model_lol_temp1)
msft_fitted_temp1<- power_temp - residual_lol_temp1
ts.plot(power_temp)
points(msft_fitted_temp1, type = "l", col = 2, lty = 2)
x_forecast_temp <- forecast::forecast(model_lol_temp1,h=4*12,level=c(0.8,0.95))
true_temp=ts(lol_temp[1:548,4],y)

par(mfrow=c(2,1))
plot(x_forecast_temp)
plot(true_temp)

# 500 to 1000
power_temp3=ts(lol_temp[501:1000,4],y)
acf(diff(power_temp3), lag.max = 80) # 73

model_lol_temp3 <- arima(power_temp3,order=c(0,1,5),seasonal = list(order=c(0,1,3),period=73))
residual_lol_temp3 <- residuals(model_lol_temp3)
acf(residual_lol_temp3, lag.max = 80)

power_temp33=ts(lol_temp[501:700,4],y)
acf(diff(power33),lag.max = 80) # lag is 74

power333=ts(lol[700:1000,4],y)
acf(diff(power333),lag.max = 80) # lag is 72

msft_fitted_temp3 <- power_temp3 - residual_lol_temp3
ts.plot(power_temp3)
points(msft_fitted_temp3, type = "l", col = 2, lty = 2)

x_forecast_temp3 <- forecast::forecast(model_lol_temp3,h=4*12,level=c(0.8,0.95))
true_temp3=ts(lol_temp[501:1048,4],y)
par(mfrow=c(2,1))
plot(x_forecast_temp3)
plot(true_temp3)

# 1000 to 1500
power_temp4=ts(lol_temp[1001:1500,4],y)
acf(diff(power_temp4), lag.max = 90) # 85

model_lol_temp4 <- arima(power_temp4,order=c(0,1,5),seasonal = list(order=c(0,1,3),period=85))
residual_lol_temp4 <- residuals(model_lol_temp4)
acf(residual_lol_temp4, lag.max = 90)

power_temp44=ts(lol_temp[1001:1300,4],y)
acf(diff(power_temp44),lag.max = 90) # lag is 79

power_temp444=ts(lol_temp[1300:1500,4],y)
acf(diff(power_temp444),lag.max = 90) # lag is 85

msft_fitted_temp4 <- power_temp4 - residual_lol_temp4
ts.plot(power_temp4)
points(msft_fitted_temp4, type = "l", col = 2, lty = 2)

lines(fitted(model_lol_3),col="blue")

x_forecast_temp4 <- forecast::forecast(model_lol_temp4,h=4*12,level=c(0.8,0.95))
true_temp4=ts(lol_temp[1001:1548,4],y)
par(mfrow=c(2,1))
plot(x_forecast_temp4)
plot(true_temp4)

# 2000 to 2500
power_temp5=ts(lol_temp[2001:2500,4],y)
acf(diff(power_temp5), lag.max = 90) # lag is 77


model_lol_temp5 <- arima(power_temp5,order=c(0,1,5),seasonal = list(order=c(0,1,3),period=76))
residual_lol_temp5 <- residuals(model_lol_temp5)
acf(residual_lol_temp5, lag.max = 80)

power_temp44=ts(lol_temp[1001:1300,4],y)
acf(diff(power_temp44),lag.max = 90) # lag is 79

power_temp444=ts(lol_temp[1300:1500,4],y)
acf(diff(power_temp444),lag.max = 90) # lag is 85

msft_fitted_temp5 <- power_temp5 - residual_lol_temp5
ts.plot(power_temp5)
points(msft_fitted_temp5, type = "l", col = 2, lty = 2)

lines(fitted(model_lol_3),col="blue")

x_forecast_temp5 <- forecast::forecast(model_lol_temp5,h=4*12,level=c(0.8,0.95))
true_temp5=ts(lol_temp[2001:2548,4],y)
par(mfrow=c(2,1))
plot(x_forecast_temp5)
plot(true_temp5)

# 3000 to 3500
power_temp6=ts(lol_temp[3001:3500,4],y)
acf(diff(power_temp6), lag.max = 95) # lag is 77

model_lol_temp6 <- arima(power_temp6,order=c(0,1,5),seasonal = list(order=c(0,1,3),period=92))
residual_lol_temp6 <- residuals(model_lol_temp6)
acf(residual_lol_temp5, lag.max = 80)

power_temp44=ts(lol_temp[1001:1300,4],y)
acf(diff(power_temp44),lag.max = 90) # lag is 79

power_temp444=ts(lol_temp[1300:1500,4],y)
acf(diff(power_temp444),lag.max = 90) # lag is 85

msft_fitted_temp6 <- power_temp6 - residual_lol_temp6
ts.plot(power_temp6)
points(msft_fitted_temp6, type = "l", col = 2, lty = 2)

lines(fitted(model_lol_3),col="blue")

x_forecast_temp6 <- forecast::forecast(model_lol_temp6,h=4*12,level=c(0.8,0.95))
true_temp6=ts(lol_temp[3001:3548,4],y)
par(mfrow=c(2,1))
plot(x_forecast_temp6)
plot(true_temp6)
