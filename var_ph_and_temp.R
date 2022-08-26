library(zoo)
library(lubridate)
library(tidyverse)
library('xts')
library(forecast)
library(fUnitRoots)
library(vars)
library(lmtest)
library(ggplot2)

#### load data ####
data <- read.csv('C:/Users/Ryan_/Documents/R/time series/files/ph_across_sites.csv')
data <- data[data$site=="Lompoc Landing",c("date_time","p_h","temp_c")]
data1 <- data[1000:2000, ]
x <- zoo(data1[,c(2,3)],mdy_hm(data$date_time), frequency = 1)
plot(x,xlab="date",main="time series plot")
str(data)

### Visualize the data
acf(data1$p_h, lag.max = 1000)
acf(data1$p_h, lag.max = 100)
acf(data1$temp_c, lag.max = 1000)


ggplot(data1)+geom_point(mapping = aes(x=temp_c, y=p_h))


#### Test stationarity ####
adfTest(data1$p_h)
adfTest(data1$temp_c)
# p-value >0.05, The data is not stationary


# Regression
Reg <- lm(p_h ~ temp_c, data =data1)
summary(Reg)


#2
###var model###
# Selecting the lag
lag_select<-VARselect(data1[,c("p_h","temp_c")], lag.max = 90, type = 'const')
lag_select$selection
VARselect(data1[, c("p_h", "temp_c")], lag.max =90)

#lag=3 or lag=4
library(vars)
var_model_1 <- VAR(data1[,2:3],
                   p = 3,
                   type = "const",
                   season = 75,
                   exog = NULL)
summary(var_model_1)


var_model_2 <- VAR(data1[,2:3],
                   p = 4,
                   type = "const",
                   season = 75)
summary(var_model_2)



#3
# Test the model

# Serial correlation
Serial1<-serial.test(var_model_1, lags.pt = 300, type = 'PT.asymptotic')
Serial1

Serial2<-serial.test(var_model_2, lags.pt = 300, type = 'PT.asymptotic')
Serial2
# p-value < 0.05. I fail to reject the null that there is no serial correlation.

# Heteroscedasticity
arch1<-arch.test(var_model_1, multivariate.only = T)
arch1

arch2<-arch.test(var_model_2, multivariate.only = T)
arch2
#  p-value of var_model_2 is 0.03522 < 0.05. Model 2 does not suffer from heteroscedasticity. 

# Test for structural break
stability1<-stability(var_model_1)
plot(stability1)
stability2<-stability(var_model_2)
plot(stability2)


#4
# The Granger causality test
# if the values of temp_c is valuable for forecasting the future values of pH
grangertest(temp_c ~ p_h, data = data1)
# P-value is 0.3455 > 0.05. I fail to reject the null that the ph does not Granger-cause temp.
Granger_temp1<-causality(var_model_1, cause = 'temp_c')
Granger_temp1
Granger_temp2<-causality(var_model_2, cause = 'temp_c')
Granger_temp2

# if the values of temp_c is valuable for forecasting the future values of pH
grangertest(p_h ~ temp_c, data = data1) 
Granger_ph1<-causality(var_model_1, cause = 'p_h')
Granger_ph1
Granger_ph2<-causality(var_model_2, cause = 'p_h')
Granger_ph2
# P-value is 1.467e-09 < 0.05. I reject the null that the temp_c does not Granger-cause p_h.


#5
#Impulse Response Functions#

#IRF: Impulse response function.
temp_irf<-irf(var_model_1, impulse = "p_h", response = "temp_c", n.ahead = 15, ortho = TRUE)
ph_irf<-irf(var_model_1, impulse = "temp_c", response = "p_h", n.ahead = 15, ortho = TRUE)

temp_irf2<-irf(var_model_2, impulse = "p_h", response = "temp_c", n.ahead = 15, ortho = TRUE)
ph_irf2<-irf(var_model_2, impulse = "temp_c", response = "p_h", n.ahead = 15, ortho = TRUE)

plot(temp_irf)
plot(temp_irf2)
# The positive impulse of pH makes the temperature decrease first and increase later.
# But since the confidence interval contains the x-axis. It can be assumed that the positive impulse of pH
# does not impact the temperature.

plot(ph_irf)
plot(ph_irf2)
# The positive impulse of temperature makes the ph increase.

#6
#Forecast Error Variance Decomposition (FEVD)
# The variance decomposition indicates the amount of information each variable contributes to the other variables. 
res1=fevd(var_model_1, n.ahead = 15)
win.graph(width=12,height=8)
plot(res1)

res2=fevd(var_model_2, n.ahead = 15)
win.graph(width=12,height=8)
plot(res2)
# Temperature is mainly affected by itself, but pH is affected by both the temperature and itself.


# Prediction
var1.predict <- predict(var_model_1,n.ahead=48,ci=0.95)
var1.predict

plot(var1.predict)
plot(x_forecast0)
fanchart(var1.predict, names = 'p_h')
fanchart(var1.predict, names = 'temp')

var2.predict <- predict(var_model_2,n.ahead=48,ci=0.95)
var2.predict
plot(var2.predict)

data0 <- data[1000:2048, ]
y <- zoo(data0[,c(2,3)],mdy_hm(data$date_time), frequency = 1)
plot(y,xlab="date",main="time series plot")
plot(data0)
