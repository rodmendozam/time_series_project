# We have divided the data sets into three. The fitting and test together with the complete. 
artificial_fit.ts <- ts(group22_data_1, start=c(1960,1), end=c(1963,12))
artificial_val.ts <- ts(group22_data_1, start=c(1964,1))
artificial.ts <- ts(group22_data_1$month22,start = c(1990,1),frequency = 12);

artificial_fit.msts <- msts(group22_data_1$month22, seasonal.periods=c(3,12), start=c(1990,1))
tsdisplay(artificial_fit.msts)
decompose(artificial_fit.msts)
# Spectrum
artificial.spec<-spectrum(artificial.ts, xlab = "Frequency [Cycle/Year]",ylab = "Spectral coefficients",main='Frequency Domain: Artificial Data',span = 2)
# Decomposition
artificial.deco<-decompose(artificial.ts,type = "additive")
plot(artificial.deco)
# TSDISPLAY
ggtsdisplay(artificial.ts,main = "Artificial Data")

# Remove trend
artificial.d1 <- diff(artificial.ts, differences = 1)
ggtsdisplay(artificial.d1)

# Remove trend and seasonality
artificial.d1sd1 <- diff(artificial.d1,lag = 3, differences = 1)
ggtsdisplay(artificial.d1sd1)




# Spectra of the data
artificial.spec<-spectrum(artificial.ts, xlab = "Frequency [1/Year]",ylab = "Spectral coefficients",main='Frequency Domain',span = 2)
ggtsdisplay(artificial.spec$spec)

# ARMA(3,0,0) MODEL
artificial.arma.3.0 <- Arima(artificial.ts,order=c(3,0,0))
artificial.arma.3.0
artificial.arma.3.0.fore <- forecast(artificial.arma.3.0,h=19)
plot.forecast(artificial.arma.3.0.fore)

# ARMA(2,1,0) MODEL
artificial.arma.2.1 <- Arima(artificial.ts,order=c(2,1,0))
artificial.arma.2.1
artificial.arma.2.1.fore <- forecast(artificial.arma.2.1,h=19)
plot.forecast(artificial.arma.2.1.fore)

# ARMA(0,0,12) MODEL
artificial.arma.3.1 <- Arima(artificial.ts,order=c(0,1,12))
artificial.arma.3.1
artificial.arma.3.1.fore <- forecast(artificial.arma.3.1,h=19)
plot.forecast(artificial.arma.3.1.fore)

# SARIMA
artificial.sarima<-Arima(artificial.ts,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=3))
artificial.sarima.fore<- forecast(artificial.sarima,h=12)
plot(artificial.sarima.fore)

# Auto ARIMA 
artificial.arma <- auto.arima(artificial.ts)
artificial.arma
artificial.arma.fore<- forecast(artificial.arma,h=19)
plot.forecast(artificial.arma.fore)
