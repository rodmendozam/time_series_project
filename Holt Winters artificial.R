artificial.ts <- ts(group22_data_1$month22, start=c(1990,1),frequency = 12)
artificial_fit.ts <- window(artificial.ts, start=c(1990,1), end=c(1994,7))
artificial_val.ts <- window(artificial.ts, start=c(1994,8))
artificial3.ts <- ts(group22_data_1$month22, start=c(1,1),frequency = 3)
#################################################
#------------Differences-------------------------
#################################################
artificial12.d1 <-diff(artificial12.ts,differences = 1)
artificial12.sd1d1 <- diff(artificial12.d1,lag = 3,differences = 1)
artificial3.d1 <-diff(artificial3.ts,differences = 1)
artificial3.sd1d1 <- diff(artificial3.d1,lag = 12,differences = 1)
#################################################
#--------------HOLT WINTERS----------------------
#################################################
# 12 sd1d1
artificial12.sd1d1.hw <- HoltWinters(artificial12.sd1d1,beta = FALSE)
artificial12.sd1d1.hw.fore<-forecast.HoltWinters(artificial12.sd1d1.hw, h = 18)
artificial12.sd1d1.hw
plot.forecast(artificial12.sd1d1.hw.fore)
accuracy(artificial12.sd1d1.hw.fore)
ggtsdisplay(artificial12.sd1d1.hw.fore$residuals)
shapiro.test(artificial12.sd1d1.hw.fore$residuals)
Box.test(artificial12.sd1d1.hw.fore$residuals,lag=12,type="Ljung-Box")
# 3 sd1d1
artificial3.sd1d1.hw <- HoltWinters(artificial3.sd1d1,beta = FALSE)
artificial3.sd1d1.hw.fore<-forecast.HoltWinters(artificial3.sd1d1.hw, h = 18)
artificial3.sd1d1.hw
plot.forecast(artificial3.sd1d1.hw.fore)
accuracy(artificial3.sd1d1.hw.fore)
ggtsdisplay(artificial3.sd1d1.hw.fore$residuals)
shapiro.test(artificial3.sd1d1.hw.fore$residuals)
Box.test(artificial3.sd1d1.hw.fore$residuals,lag=12,type="Ljung-Box")
# 3 
artificial3.hw <- HoltWinters(artificial3.ts)
artificial3.hw.fore<-forecast.HoltWinters(artificial3.hw, h = 18)
artificial3.hw
plot.forecast(artificial3.hw.fore)
accuracy(artificial3.hw.fore)
ggtsdisplay(artificial3.hw.fore$residuals)
shapiro.test(artificial3.hw.fore$residuals)
Box.test(artificial3.hw.fore$residuals,lag=12,type="Ljung-Box")
# 12
artificial_fit.hw <- HoltWinters(artificial_fit.ts)
artificial_fit.hw.fore<-forecast.HoltWinters(artificial_fit.hw, h = 14)
artificial_fit.hw

plot.forecast(artificial_fit.hw.fore)
accuracy(artificial_fit.hw.fore)
accuracy(artificial_fit.hw.fore$mean,artificial_val.ts)

artificial.hw<-HoltWinters(artificial.ts,alpha = 0.04, beta = 0.50,gamma = 0.89)
artificial.hw.fore<-forecast.HoltWinters(artificial.hw,h = 18)
plot.forecast(artificial.hw.fore,xlab = "Years",ylab = "month22",main = "Holt-Winters Artificial Data")
artificial.hw.fore
ggtsdisplay(artificial.hw.fore$residuals)
shapiro.test(artificial.hw.fore$residuals)
Box.test(artificial.hw.fore$residuals,lag=12,type="Ljung-Box")




artificial12.hw <- HoltWinters(artificial12.ts)
artificial12.hw.fore<-forecast.HoltWinters(artificial12.hw, h = 18)
artificial12.hw
plot.forecast(artificial12.hw.fore)
accuracy(artificial12.hw.fore)
# 12 d1
artificial12.d1.hw <- HoltWinters(artificial12.d1,beta = FALSE)
artificial12.d1.hw.fore<-forecast.HoltWinters(artificial12.d1.hw, h = 18)
shapiro.test(artificial12.d1.hw.fore$residuals)
Box.test(artificial12.d1.hw.fore$residuals,lag=12,type="Ljung-Box")
artificial12.d1.hw
plot.forecast(artificial12.d1.hw.fore)
accuracy(artificial12.d1.hw.fore)
# 3 d1
artificial3.d1.hw <- HoltWinters(artificial3.d1,beta = FALSE)
artificial3.d1.hw.fore<-forecast.HoltWinters(artificial3.d1.hw, h = 18)
artificial3.d1.hw
plot.forecast(artificial3.d1.hw.fore)
accuracy(artificial3.d1.hw.fore)
ggtsdisplay(artificial3.d1.hw.fore$residuals)
shapiro.test(artificial3.d1.hw.fore$residuals)
Box.test(artificial3.d1.hw.fore$residuals,lag=12,type="Ljung-Box")

