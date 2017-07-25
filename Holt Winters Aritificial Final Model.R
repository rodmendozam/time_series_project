##### DATA DEFINITION #####
# Definition of time series with frequency 3
artificial3.ts <- ts(group22_data_1$month22, start=c(1,1),frequency = 3)
# 80% fitting data 20% validation data
artificial3_fit.ts <- window(artificial3.ts, start=c(1,1), end=c(19,2))
artificial3_val.ts <- window(artificial3.ts, start=c(19,3))

##### HOLT-WINTERS FITTING #####
# Holt-Winters fitting of the model
artificial3.hw <- HoltWinters(artificial3_fit.ts)
# Parameters of the Holt-Winters model
artificial3.hw

##### VALIDATION #####
#Forecast of the next 13 points to validate
artificial3.hw.fore<-forecast.HoltWinters(artificial3.hw, h = 13)
# Test the residuals
ggtsdisplay(artificial3.hw.fore$residuals)
shapiro.test(artificial3.hw.fore$residuals)
Box.test(artificial3.hw.fore$residuals,lag=12,type="Ljung-Box")
# Goodness of fit
accuracy(artificial3.hw.fore)
# Test accuracy with independent validation data
accuracy(artificial3.hw.fore,artificial3_val.ts)

##### FORECAST #####
# Make prediction with trained model for next 18 months.
artificial.hw<-HoltWinters(artificial3.ts,alpha = 0.02, beta = 1.00,gamma = 0.28)
artificial.hw.fore<-forecast.HoltWinters(artificial.hw,h = 18)
# Plot the forecast for the next 18 months
artificial.hw.fore$x<- ts(artificial.hw.fore$x, start=c(1990,1),frequency = 12)
artificial.hw.fore$mean<- ts(artificial.hw.fore$mean, start=c(1995,10),frequency = 12)
plot.forecast(artificial.hw.fore,xlab = "Years",ylab = "month22",main = "Holt-Winters Forecast Artificial Data")

##### AUTOMATIC ES #####
# Model fitting 
artificial3.ets<-ets(artificial3_fit.ts)
artificial3.ets
# Validation of model
artificial3.ets.fore<-forecast(artificial3.ets, h = 13)
# Analysis of residuals
ggtsdisplay(artificial3.ets.fore$residuals)
shapiro.test(artificial3.ets.fore$residuals)
Box.test(artificial3.ets.fore$residuals,lag=12,type="Ljung-Box")
tsdiag(artificial3.ets)


