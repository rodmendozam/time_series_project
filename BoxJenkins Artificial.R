##### DATA DEFINITION #####
# Definition of time series with frequency 3
artificial3.ts <- ts(group22_data_1$month22, start=c(1,1),frequency = 3)
# 80% fitting data 20% validation data
artificial3_fit.ts <- window(artificial3.ts, start=c(1,1), end=c(19,2))
artificial3_val.ts <- window(artificial3.ts, start=c(19,3))
artificial3.arima300<-Arima(artificial3_fit.ts,order = c(3,0,0))
artificial3.arima300
##### VALIDATION #####
#Forecast of the next 13 points to validate
artificial3.arima300.fore<-forecast(artificial3.arima300, h = 13)

# Test the residuals
ggtsdisplay(artificial3.arima300.fore$residuals)
shapiro.test(artificial3.arima300.fore$residuals)
Box.test(artificial3.arima300.fore$residuals,lag=12,type="Ljung-Box")
# Goodness of fit
accuracy(artificial3.arima300.fore)
# Test accuracy with independent validation data
accuracy(artificial3.arima300.fore$mean,artificial3_val.ts)
# Run diagnosis on the arima300 model
par(mar = rep(3, 4))
tsdiag(artificial3.arima300)



#################################################
#------------(210)-------------------------
#################################################

artificial3.arima210<-Arima(artificial3_fit.ts,order = c(2,1,0))
artificial3.arima210
##### VALIDATION #####
#Forecast of the next 13 points to validate
artificial3.arima210.fore<-forecast(artificial3.arima210, h = 13)

# Test the residuals
ggtsdisplay(artificial3.arima210.fore$residuals)
shapiro.test(artificial3.arima210.fore$residuals)
Box.test(artificial3.arima210.fore$residuals,lag=12,type="Ljung-Box")
# Goodness of fit
accuracy(artificial3.arima210.fore)
# Test accuracy with independent validation data
accuracy(artificial3.arima210.fore$mean,artificial3_val.ts)
# Run diagnosis on the arima210 model
par(mar = rep(3, 4))
tsdiag(artificial3.arima210)




#################################################
#------------(110)(110)-------------------------
#################################################

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



#################################################
#------------(110)(011)-------------------------
#################################################

artificial3.arima110011<-Arima(artificial3_fit.ts,order = c(1,1,0),seasonal=list(order=c(0,1,1),period=3))
artificial3.arima110011
##### VALIDATION #####
#Forecast of the next 13 points to validate
artificial3.arima110011.fore<-forecast(artificial3.arima110011, h = 13)

# Test the residuals
ggtsdisplay(artificial3.arima110011.fore$residuals)
shapiro.test(artificial3.arima110011.fore$residuals)
Box.test(artificial3.arima110011.fore$residuals,lag=12,type="Ljung-Box")
# Goodness of fit
accuracy(artificial3.arima110011.fore)
# Test accuracy with independent validation data
accuracy(artificial3.arima110011.fore$mean,artificial3_val.ts)
# Run diagnosis on the arima110011 model
par(mar = rep(3, 4))
tsdiag(artificial3.arima110011)



#################################################
#------------(110)(111)-------------------------
#################################################

artificial3.arima110011<-Arima(artificial3_fit.ts,order = c(1,1,0),seasonal=list(order=c(0,1,1),period=3))
artificial3.arima110011
##### VALIDATION #####
#Forecast of the next 13 points to validate
artificial3.arima110011.fore<-forecast(artificial3.arima110011, h = 13)

# Test the residuals
ggtsdisplay(artificial3.arima110011.fore$residuals)
shapiro.test(artificial3.arima110011.fore$residuals)
Box.test(artificial3.arima110011.fore$residuals,lag=12,type="Ljung-Box")
# Goodness of fit
accuracy(artificial3.arima110011.fore)
# Test accuracy with independent validation data
accuracy(artificial3.arima110011.fore$mean,artificial3_val.ts)
# Run diagnosis on the arima110011 model
par(mar = rep(3, 4))
tsdiag(artificial3.arima110011)


