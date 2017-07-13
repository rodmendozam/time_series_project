# We have divided the data sets into three. The fitting and test together with the complete. 
artificial.ts <- ts(group22_data_1$month22,start = c(1960,1),frequency = 3);
plot(artificial.ts)
artificial_fit.ts <- window(artificial.ts, start=c(1960,1), end=c(1963,12))
artificial_val.ts <- window(artificial.ts, start=c(1964,1))
#d1
artificial.d1 <- diff(artificial.ts,differences = 1)
ggtsdisplay(artificial.d1,main = "Fingerprint difference")
#sd1
artificial.sd1 <- diff(artificial.ts,lag = 3,differences = 1)
ggtsdisplay(artificial.sd1,main = "Fingerprint seasonal difference")
#d1sd1
artificial.d1sd1 <- diff(artificial.d1,lag = 3,differences = 1)
ggtsdisplay(artificial.d1sd1,main = "Fingerprint seasonal and normal difference")

##############################################################
#SES 
artificial.ses<-HoltWinters(artificial.ts,seasonal = "additive") 
artificial.ses.fore <- forecast.HoltWinters(artificial.ses,h=5)
artificial.ses
artificial.ses.fore$residuals
plot(artificial.ses.fore)
tsdisplay(artificial.ses.fore$residuals)
par(mar = rep(2, 4))
Box.test(artificial.ses.fore$residuals,lag=20,type="Ljung-Box")
with(artificial.ses,accuracy(fitted,x))

##############################################################
#SES without gamma
artificial.sd1.ses<-HoltWinters(artificial.sd1,gamma = FALSE)
artificial.sd1.ses.fore <- forecast.HoltWinters(artificial.sd1.ses,h=5)
artificial.sd1.ses.fore$residuals
plot(artificial.sd1.ses.fore)
tsdisplay(artificial.sd1.ses.fore$residuals)
par(mar = rep(2, 4))
Box.test(artificial.sd1.ses.fore$residuals,lag=20,type="Ljung-Box")
with(artificial.sd1.ses,accuracy(fitted,x))

##############################################################
#SES without beta
artificial.d1.ses<-HoltWinters(artificial.d1,beta = FALSE)
artificial.d1.ses.fore <- forecast.HoltWinters(artificial.d1.ses,h=5)
artificial.d1.ses.fore$residuals
plot(artificial.d1.ses.fore)
tsdisplay(artificial.d1.ses.fore$residuals)
par(mar = rep(2, 4))
Box.test(artificial.d1.ses.fore$residuals,lag=20,type="Ljung-Box")
with(artificial.d1.ses,accuracy(fitted,x))


##############################################################
#SES without beta and gamma
artificial.d1sd1.ses<-HoltWinters(artificial.d1sd1)
artificial.d1sd1.ses.fore <- forecast.HoltWinters(artificial.d1sd1.ses,h=5)
artificial.d1sd1.ses.fore$residuals
plot(artificial.d1sd1.ses.fore)
tsdisplay(artificial.d1sd1.ses.fore$residuals)
par(mar = rep(2, 4))
Box.test(artificial.d1sd1.ses.fore$residuals,lag=20,type="Ljung-Box")
with(artificial.d1sd1.ses,accuracy(fitted,x))

qqPlot(artificial.ses.fore$residuals)








# Show the forecast
plot.forecast(artificial.ses.fore)
# Plot predictions
plot(artificial.ses)
# Analyze residuals
ggtsdisplay(artificial.ses.fore$residuals)
artificial.ses.fore$residuals
artificial.ses.fore
# Normality
qqnorm(artificial.ses.fore$residuals)
qqline(artificial.ses.fore$residuals)
shapiro.test(artificial.ses.fore$residuals)
artificial.ses.fore$residuals
# Box-Ljung
Box.test(artificial.ses.fore$residuals,lag=20,type="Ljung-Box")

##############################################################
#SES d1
artificial.sd1.ses<-HoltWinters(artificial.d1, beta = 0.56,gamma = 0.60)
plot(artificial.sd1.ses)


##############################################################
#AUTOMATIC
artificial.ets <- ets(artificial.ts)
artificial.ets.fore<-forecast(artificial.ets,h=10)
ggtsdisplay(artificial.ets.fore$residuals)
Box.test(artificial.ets.fore$residuals,lag=20,type="Ljung-Box")
shapiro.test(artificial.ets.fore$residuals)
with(artificial.ets,accuracy(fitted,x))
artificial.ets.fore$residuals