##### DATA DEFINITION #####
# Definition of time series with frequency 3
artificial3.ts <- ts(group22_data_1$month22, start=c(1,1),frequency = 3)
# 80% fitting data 20% validation data
artificial3_fit.ts <- window(artificial3.ts, start=c(1,1), end=c(19,2))
artificial3_val.ts <- window(artificial3.ts, start=c(19,3))

##### MODEL FIT #####
artificial3.arima110110<-Arima(artificial3_fit.ts,order = c(1,1,0),seasonal=list(order=c(1,1,0),period=3))
artificial3.arima110110

##### VALIDATION #####
#Forecast of the next 13 points to validate
artificial3.arima110110.fore<-forecast(artificial3.arima110110, h = 13)

# Test the residuals
ggtsdisplay(artificial3.arima110110.fore$residuals)
shapiro.test(artificial3.arima110110.fore$residuals)
Box.test(artificial3.arima110110.fore$residuals,lag=12,type="Ljung-Box")
# Goodness of fit
accuracy(artificial3.arima110110.fore)
# Test accuracy with independent validation data
accuracy(artificial3.arima110110.fore$mean,artificial3_val.ts)
# Run diagnosis on the arima110110 model
par(mar = rep(3, 4))
tsdiag(artificial3.arima110110)

##### FORECAST #####

forecast_sarima<-forecast(artificial3.ts,model = artificial3.arima110110, h = 18)
forecast_sarima$x<- ts(forecast_sarima$x, start=c(1990,1),frequency = 12)
forecast_sarima$mean<- ts(forecast_sarima$mean, start=c(1995,10),frequency = 12)
forecast_sarima$lower<- ts(forecast_sarima$lower, start=c(1995,10),frequency = 12)
forecast_sarima$upper<- ts(forecast_sarima$upper, start=c(1995,10),frequency = 12)
plot.forecast(forecast_sarima,xlab = "Year", main = "Artificial Data Forecast SARIMA (1,1,0)X(1,1,0)_3 ")
plot.forecast(forecast_sarima)


##### AUTO BOX JENKINS #####
artificial3.auto <- auto.arima(artificial3_fit.ts)
artificial3.auto
artificial3.auto.fore <- forecast(artificial3.auto,h = 13)
# Test the residuals
ggtsdisplay(artificial3.auto.fore$residuals)
shapiro.test(artificial3.auto.fore$residuals)
Box.test(artificial3.auto.fore$residuals,lag=12,type="Ljung-Box")
# Validation
accuracy(artificial3.auto.fore$mean,artificial3_val.ts)
