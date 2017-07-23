artificial12.ts <- ts(group22_data_1$month22, start=c(1990,1),frequency = 12)
artificial12_fit.ts <- window(artificial12.ts, start=c(1990,1), end=c(1994,7))
artificial12_val.ts <- window(artificial12.ts, start=c(1994,8))
artificial3.ts <- ts(group22_data_1$month22, start=c(1,1),frequency = 3)
artificial3_fit.ts <- window(artificial3.ts, start=c(1,1), end=c(19,2))
artificial3_val.ts <- window(artificial3.ts, start=c(19,3))

###########################################
###############MODEL 2#####################
###########################################

# Holt-Winters fitting of the model
artificial12.hw <- HoltWinters(artificial12_fit.ts)
# Parameters of the Holt-Winters model
artificial12.hw

##### VALIDATION #####
#Forecast of the next 13 points to validate
artificial12.hw.fore<-forecast.HoltWinters(artificial12.hw, h = 13)
# Test the residuals
ggtsdisplay(artificial12.hw.fore$residuals)
shapiro.test(artificial12.hw.fore$residuals)
Box.test(artificial12.hw.fore$residuals,lag=12,type="Ljung-Box")
# Goodness of fit
accuracy(artificial12.hw.fore)
# Test accuracy with independent validation data
accuracy(artificial12.hw.fore,artificial12_val.ts)


###########################################
###############MODEL 3#####################
###########################################
#DIFF
artificial3.d1 <- diff(artificial3.ts,differences = 1)
artificial3_fit.d1 <- window(artificial3.d1, start=c(1,2), end=c(19,2))
artificial3_val.d1 <- window(artificial3.d1, start=c(19,3))


# Holt-Winters fitting of the model
artificial3.d1.hw <- HoltWinters(artificial3_fit.d1)
# Parameters of the Holt-Winters model
artificial3.d1.hw

##### VALIDATION #####
#Forecast of the next 13 points to validate
artificial3.d1.hw.fore<-forecast.HoltWinters(artificial3.d1.hw, h = 13)
# Test the residuals
ggtsdisplay(artificial3.d1.hw.fore$residuals)
shapiro.test(artificial3.d1.hw.fore$residuals)
Box.test(artificial3.d1.hw.fore$residuals,lag=12,type="Ljung-Box")
# Goodness of fit
accuracy(artificial3.d1.hw.fore)
# Test accuracy with independent validation data
accuracy(artificial3.d1.hw.fore,artificial3_val.d1)


###########################################
###############MODEL 4#####################
###########################################
#DIFF
artificial12.d1 <- diff(artificial12.ts,differences = 1)
artificial12_fit.d1 <- window(artificial12.d1, start=c(1990,3), end=c(1994,7))
artificial12_val.d1 <- window(artificial12.d1, start=c(1994,8))

# Holt-Winters fitting of the model
artificial12.d1.hw <- HoltWinters(artificial12_fit.d1,beta = FALSE)
# Parameters of the Holt-Winters model
artificial12.d1.hw

##### VALIDATION #####
#Forecast of the next 13 points to validate
artificial12.d1.hw.fore<-forecast.HoltWinters(artificial12.d1.hw, h = 13)
# Test the residuals
ggtsdisplay(artificial12.d1.hw.fore$residuals)
shapiro.test(artificial12.d1.hw.fore$residuals)
Box.test(artificial12.d1.hw.fore$residuals,lag=12,type="Ljung-Box")
# Goodness of fit
accuracy(artificial12.d1.hw.fore)
# Test accuracy with independent validation data
accuracy(artificial12.d1.hw.fore,artificial12_val.d1)



###########################################
###############MODEL 5#####################
###########################################
#DIFF
artificial3.ds <- diff(artificial3.ts,lag = 3,differences = 1)
artificial3.ds
artificial3_fit.d1 <- window(artificial3.d1, start=c(2,1), end=c(19,2))
artificial3_val.d1 <- window(artificial3.d1, start=c(19,3))
artificial3_fit.d1
# Holt-Winters fitting of the model
artificial3.ds.hw <- HoltWinters(artificial3_fit.ds)
# Parameters of the Holt-Winters model
artificial3.ds.hw

##### VALIDATION #####
#Forecast of the next 13 points to validate
artificial3.ds.hw.fore<-forecast.HoltWinters(artificial3.ds.hw, h = 13)
# Test the residuals
ggtsdisplay(artificial3.ds.hw.fore$residuals)
shapiro.test(artificial3.ds.hw.fore$residuals)
Box.test(artificial3.ds.hw.fore$residuals,lag=12,type="Ljung-Box")
# Goodness of fit
accuracy(artificial3.ds.hw.fore)
# Test accuracy with independent validation data
accuracy(artificial3.ds.hw.fore,artificial3_val.ds)



