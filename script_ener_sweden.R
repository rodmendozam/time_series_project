  ##### LOAD THE DATA #####
  ener_sweden.ts <- ts(group22_data_2$ener_sweden,start = c(1960,1));
  gdp_sweden.ts <- ts(group22_data_2$gdp_sweden,start = c(1960,1));
  ener_sweden_fit.ts<- window(ener_sweden.ts,start = c(1960,1),end = c(1997,1));
  ener_sweden_val.ts<- window(ener_sweden.ts,start = c(1998,1));
  
  # Remove na values
  ener_sweden.ts <-na.omit(ener_sweden.ts)
  gdp_sweden.ts <-na.omit(gdp_sweden.ts)

##### DIFFERENCES #####

ener_sweden.d1<-diff(ener_sweden.ts,differences = 1)
ener_sweden_fit.d1<-window(ener_sweden.d1,start = c(1960,2),end = c(1997,1))
ener_sweden_val.d1<-window(ener_sweden.d1,start = c(1998,1))
gdp_sweden.d1<- diff(gdp_sweden.ts,differences = 1)
gdp_sweden.d2<- diff(gdp_sweden.ts,differences = 1)
ggtsdisplay(ener_sweden.d1)
ggtsdisplay(gdp_sweden.d1)
ggtsdisplay(gdp_sweden.d2)
spectrum(ener_sweden.d1)
spectrum(gdp_sweden.d1)

##### FINGERPRINT #####

ggtsdisplay(ener_sweden.ts,main = "Energy Sweden",xlab = "Year", ylab = "toe")
ggtsdisplay(gdp_sweden.ts,main = "GDP Sweden", xlab = "Year", ylab = "US dollar")
plot(gdp_sweden.ts)
# Spectral decomposition of the data 
spectrum(ener_sweden.ts,span = 2,ylab = "Spectral coefficients",main = "Frequency Domain: Energy Sweden", xlab ="Frequency[cycle/year]")
spectrum(gdp_sweden.ts,span = 2,ylab = "Spectral coefficients" ,main = "Frequency Domain: GDP Sweden", xlab ="Frequency[cycle/year]")

##### CROSS CORRELATION #####
ccf(ener_sweden.ts,gdp_sweden.ts,na.action=na.omit)
ccf(gdp_sweden.d1,ener_sweden.d1,na.action=na.omit)
##### BOX JENKINS #####

ener_sweden.arima010<-Arima(ener_sweden_fit.ts,order = c(0,1,0))
ener_sweden.arima010

# Validation of model
ener_sweden.arima010.fore<-forecast(ener_sweden.arima010, h = 10)
# Analysis of residuals
ggtsdisplay(ener_sweden.arima010.fore$residuals)
shapiro.test(ener_sweden.arima010.fore$residuals)
Box.test(ener_sweden.arima010.fore$residuals,lag=12,type="Ljung-Box")
tsdiag(ener_sweden.arima010)
accuracy(ener_sweden.arima010.fore$mean,ener_sweden_val.ts)

ener_sweden.arima.fore<-forecast(ener_sweden.ts,model = ener_sweden.arima010,h = 6)
plot.forecast(ener_sweden.arima.fore,xlab = "Year",ylab = "toe", main = "Forecast ARMA Sweden Energy Consumption")
ener_sweden.arima.auto <- auto.arima(ener_sweden_fit.ts)
ener_sweden.arima.auto 
ener_sweden.arima.auto.fore<-forecast(ener_sweden.arima.auto,h = 10)
accuracy(ener_sweden.arima.auto.fore$mean,ener_sweden_val.ts)

#Validation
ggtsdisplay(ener_sweden.arima.auto.fore$residuals)
shapiro.test(ener_sweden.arima.auto.fore$residuals)
Box.test(ener_sweden.arima.auto.fore$residuals,lag=12,type="Ljung-Box")
tsdiag(ener_sweden.arima010)

ener_sweden.arima.fore<-forecast(ener_sweden.ts,model = ener_sweden.arima.auto,h = 6)
plot.forecast(ener_sweden.arima.fore,xlab = "Year",ylab = "toe", main = "Forecast ARMA Sweden Energy Consumption")

##### EXPONENTIAL SMOOTHING #####
ener_sweden_fit.hw<-HoltWinters(ener_sweden_fit.ts,gamma = FALSE)
ener_sweden_fit.hw
ener_sweden_fit.hw.fore<-forecast.HoltWinters(ener_sweden_fit.hw,h = 10)

# Validation
ggtsdisplay(ener_sweden_fit.hw.fore$residuals)
shapiro.test(ener_sweden_fit.hw.fore$residuals)
Box.test(ener_sweden_fit.hw.fore$residuals,lag=12,type="Ljung-Box")
accuracy(ener_sweden_fit.hw.fore$mean,ener_sweden_val.ts)
# Forecast
ener_sweden.hw<-HoltWinters(ener_sweden.ts,alpha = 0.9541859,beta =0.005193973, gamma = FALSE)
ener_sweden.hw.fore<-forecast.HoltWinters(ener_sweden.hw,h = 6)
plot.forecast(ener_sweden.hw.fore,xlab = "Year",ylab = "toe",main = "Holt Forecast Energy Consumption")

# Automatic exponential smoothing
ener_sweden.ets<-ets(ener_sweden_fit.ts)
ener_sweden.ets
ener_sweden.ets.fore<-forecast(ener_sweden.ets,h = 10)
plot.forecast(ener_sweden.ets.fore)
ggtsdisplay(ener_sweden.ets.fore$residuals)
shapiro.test(ener_sweden.ets.fore$residuals)
Box.test(ener_sweden.ets.fore$residuals,lag=12,type="Ljung-Box")
accuracy(ener_sweden.ets.fore$mean,ener_sweden_val.d1)




##### EXPONENTIAL SMOOTHING DIFFERENCE #####
ener_sweden.d1.hw.a<-HoltWinters(ener_sweden_fit.d1,beta = FALSE,gamma = FALSE)
ener_sweden.d1.hw.a
ener_sweden.d1.hw.a.fore<-forecast.HoltWinters(ener_sweden.d1.hw.a,h = 10)
plot.forecast(ener_sweden.d1.hw.a.fore)
ggtsdisplay(ener_sweden.d1.hw.a.fore$residuals)
shapiro.test(ener_sweden.d1.hw.a.fore$residuals)
Box.test(ener_sweden.d1.hw.a.fore$residuals,lag=12,type="Ljung-Box")
accuracy(ener_sweden.d1.hw.a.fore$mean,ener_sweden_val.d1)





# Forecast
ener_sweden.d1.hw<-HoltWinters(ener_sweden.d1,alpha = 0.003106546,beta = FALSE, gamma = FALSE)
ener_sweden.d1.fore<-forecast.HoltWinters(ener_sweden.d1.hw,h = 6)
ener_sweden.d1.fore$x<-diffinv(ener_sweden.d1.fore$x,differences = 1,xi = 2.700534)
ener_sweden.d1.fore$mean<-diffinv(ener_sweden.d1.fore$mean,differences = 1,xi = 5.511750)
ener_sweden.d1.fore$upper<-diffinv(ener_sweden.d1.fore$upper,differences = 1,xi = c(5.511750,5.511750))
ener_sweden.d1.fore$lower<-diffinv(ener_sweden.d1.fore$lower,differences = 1,xi = c(5.511750,5.511750))
plot(ener_sweden.d1.fore$x)
lines(ener_sweden.d1.fore$mean)
