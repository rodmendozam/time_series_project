# Load the data
artificial.ts <- ts(group22_data_1$month22,start = c(1960,1),frequency = 12);

# Fingerprint of the data
ggtsdisplay(artificial.ts)

# Spectra of the data
artificial.spec<-spectrum(artificial.ts, xlab = "Frequency [1/Year]",ylab = "Spectral coefficients",main='Frequency Domain',span = 2)
ggtsdisplay(artificial.spec$spec)

# Decomposition
artificial.deco<-decompose(artificial.ts,type = "additive")
artificial.loess<-stl(artificial.ts,t.window = 12,s.window="periodic")
plot(artificial.loess,main = "Decompostion of artificial time series")


# Exponential smoothing 
artificial.d1<-diff(artificial.ts,differences = 1)
ggtsdisplay(artificial.d1)
artificial.d1.ses<-HoltWinters(artificial.d1, beta = FALSE);
artificial.d1.ses
artificial.d1.ses.fore<-forecast.HoltWinters(artificial.d1.ses,h=18);
plot.forecast(artificial.d1.ses.fore)

# Seasonal differencing SES
artificial.sd1<-diff(artificial.ts,differences = 1)
ggtsdisplay(artificial.sd1)
artificial.sd1.ses<-HoltWinters(artificial.sd1, beta = 0.56,gamma = 0.60)
plot(artificial.sd1.ses)
artificial.sd1.ses

artificial.ses<-HoltWinters(artificial.ts)
plot(artificial.ses)

artificial.sd1.ses.fore<-forecast.HoltWinters(artificial.sd1.ses,h=5);
plot.forecast(artificial.sd1.ses.fore)
artificial.sd1.ses.fore$fitted
artificial.sd1.ses.fore.inv<-diffinv(artificial.sd1, differences=1,xi=artificial.ts[1])
plot(artificial.sd1.ses.fore.inv)
artificial.sd1.ses.fore.inv
plot(artificial.ts)
artificial.sd1.ses.fore.inv

artificial.ses<-HoltWinters(artificial.ts);
artificial.ses
artificial.ses.fore<-forecast.HoltWinters(artificial.ses,h=18);
plot.forecast(artificial.ses.fore)

artificial.ets <- ets(artificial.ts)
artificial.ets
artificial.ets.fore <- forecast(artificial.ets,h=10)
plot.forecast(artificial.ets.fore)


tsdisplay(artificial.ses.fore$residuals)
Box.test(artificial.ses.fore$residuals,lag=20,type="Ljung-Box")







seasonplot(artificial.ts,season.labels=TRUE,year.labels=TRUE,col=rainbow(12))
monthplot(artificial.ts)


