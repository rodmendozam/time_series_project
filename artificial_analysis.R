# We have divided the data sets into three. The fitting and test together with the complete. 
artificial.ts <- ts(group22_data_1$month22,start = c(1990,1),frequency = 3);
artificial_fit.ts <- window(artificial.ts, start=c(1960,1), end=c(1975,3))
artificial_val.ts <- window(artificial.ts, start=c(1976,1))




#AUTOMATIC TESTING
artificial.ets <- ets(artificial_fit.ts)
artificial.ets.fore<-forecast(artificial.ets,h=21)
ggtsdisplay(artificial.ets.fore$residuals)
plot.forecast(artificial.ets.fore)

plot(artificial.ets.fore$mean)
artificial.ets.fore$mean
artificial_val.ts
x<-1:21
artificial.ets.fore$upper
plot(artificial.ets.fore$mean,ylim = c(1400,3900),type = "l",col = c("red"))
par(new = TRUE)
plot(artificial_val.ts,ylim = c(1400,3900),type = "l")
plot(artificial_val.ts)

Box.test(artificial.ets.fore$residuals,lag=20,type="Ljung-Box")
shapiro.test(artificial.ets.fore$residuals)
artificial.ets.fore$mean
accuracy(artificial.ets.fore$mean,artificial_val.ts)


##############################################################
#SES 
artificial.ses<-HoltWinters(artificial_fit.ts,seasonal = "additive") 
artificial.ses.fore <- forecast.HoltWinters(artificial.ses,h=21)
artificial.ses
artificial.ses.fore$residuals
plot(artificial.ses.fore)
tsdisplay(artificial.ses.fore$residuals)
par(mar = rep(2, 4))
Box.test(artificial.ses.fore$residuals,lag=20,type="Ljung-Box")
accuracy(artificial.ses.fore$mean,artificial_val.ts)
