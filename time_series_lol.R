library(forecast)

auto.arima(ts(total, frequency = 75),D=1)
model_lol0 <- arima(total,order=c(4,0,0), seasonal = list(order=c(0,1,0),period=75))

residual_lol0 <- residuals(model_lol0)
msft_fitted0 <- total - residual_lol0
msft_fitted0
ts.plot(total)
points(msft_fitted0, type = "l", col = 2, lty = 2)

x_forecast0 <- forecast::forecast(model_lol0,h=48,level=c(0.8,0.95))
x_forecast0
plot(x_forecast0)



