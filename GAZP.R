#install.packages('forecast', dependencies = TRUE)
#library(ggplot2)

library(stats)
library(graphics)
library(zoo)
library(xts)
library(forecast)
library(tseries)
library(splines)
install.package("earth")
library(earth)
library(caret)

data = read.table("GAZP.csv", header = T, dec = ",", sep = ",")
data = data[ , c(1,2)]
names(data) = c("time", "price")
data$time = c(256:1)
data$price = as.numeric(data$price)
summary(data)
ts = ts(data = data[,2], frequency = 1)
ts = ts(rev(ts), frequency = 1)
plot(ts, col = "red")

X = c(1:256)

#???????????????? ?????????? ???????????????????? ??????????????, ???????????? ???????????????????? ????????????????????????

d = decompose(ts, "multiplicative", filter = NULL)
S = d$seasonal
T = d$trend
E = d$random

lines(T)

Y_s = ts / S
fm = lm(Y_s ~ X)
summary(fm)
lines(fm$fitted.values)
Y_k = predict(fm) * S 
lines(Y_k)

Y_k = T * S
lines(Y_k)



adf.test(ts, alternative = c("stationary"))
#TS is not stationary
adf.test(diff(ts), alternative = c("stationary"))
#TS is stationary on 5% SL

#differentiation of TS
ts_I = diff(ts)
plot(ts_I, col="red")

#application of AR(p) model (adaptive selection of p)
ar = arima(ts_I, order = c(2,0,0), include.mean = F)
pred = forecast(ar, h = 1);pred

#displaying ACF
corr = acf(ts_I, lag.max = 256, type = c("correlation")); plot(corr)
corr = acf(ts_I, lag.max = 256, type = c("partial")); plot(corr)
tsdisplay(ts_I)

#application of MA(q) model
#since the values of ACF interrupt suddenly with lag > 1, it is 
#a prominent indicator of MA(1) process 

ma = arima(ts_I, order = c(0,0,1), include.mean = F)
summary(ma)
pred = forecast(ma, h = 1); pred
plot(pred)

#building arima model
arma = arima(ts_I, order = c(1,0,3), include.mean = F)
summary(arma)
pred = forecast(arma, h = 1); pred
plot(pred)
res = arma$residuals
plot(res, main = "RESIDUALS")
Box.test(res, lag = 1, type = "Box-Pierce")

#building auto arima model
model = auto.arima(ts_I, stationary = T, seasonal = F, ic = "aic")
summary(model)
pred = forecast(model, h = 4); pred
plot(pred)
res = model$residuals
plot(res, main = "residuals")
Box.test(res, lag = 1, type = "Box-Pierce")

#spline smoothing
mod = smooth.spline(ts, keep.data = T, all.knots = F)
mod$spar #smoothing parameter
lines(mod$y)
pred = predict(mod, 257);pred
lines(pred)

#regression spline
df = data.frame(time = c(X, c(257:270)), price = c(ts, rep(NA, 14)))
model = lm(price ~ bs(time, degree = 3, knots = sample(c(1:256), round(0.6* length(X)))), data = df)
lines(X, model$fitted.values)
plot(model$residuals, type = "l")

pred = predict(model, newdata = df); pred
lines(c(X, c(257:270)), pred)

#MARS
mars = earth(x = time, y = price, degree = 2, minspan = 1, maxspan = 10,
             nprune = 10)
y = predict(mars, time)
lines(time$time, y)