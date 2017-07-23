dev.off()
# Time series with frequency 3 (time scale gets distorted)
artificial3.ts <- ts(group22_data_1$month22, start=c(1,1),frequency = 3)
ggtsdisplay(artificial3.ts)
artificial3_fit.ts <- window(artificial3.ts, start=c(1,1), end=c(21,2))
artificial3_val.ts <- window(artificial3.ts, start=c(21,3))

#########################################################
#                    DIFF
#########################################################

# ------------------d1-----------------------

artificial3.d1<- diff(artificial3.ts,differences = 1)
artificial3_fit.d1 <- window(artificial3.d1, start=c(1,2), end=c(21,2))
artificial3_val.d1 <- window(artificial3.d1, start=c(21,3))
ggtsdisplay(artificial3.d1)
ggtsdisplay(artificial3_fit.d1)
artificial3.d1.deco<-decompose(artificial3.d1,type = "additive")
plot(artificial3.d1.deco)
# Shapiro for random component
shapiro.test(artificial3.d1.deco$random)
# Correlation from the random component
tsdisplay(artificial3.d1.deco$random)
# Ljung-Box test
Box.test(artificial3.d1.deco$random,lag=12,type="Ljung-Box")

# ------------------sd1d1-----------------------

artificial3.sd1d1<- diff(artificial3.d1,lag = 3,differences = 1)
artificial3_fit.sd1d1<- diff(artificial3_fit.d1,lag = 3,differences = 1)
ggtsdisplay(artificial3.sd1d1)
artificial3.sd1d1.deco<-decompose(artificial3.sd1d1,type = "additive")
plot(artificial3.sd1d1.deco)
# Shapiro for random component
shapiro.test(artificial3.sd1d1.deco$random)
# Correlation from the random component
tsdisplay(artificial3.sd1d1.deco$random)
# Ljung-Box test
Box.test(artificial3.sd1d1.deco$random,lag=12,type="Ljung-Box")

# ------------------sd1-------------------------
artificial3.sd1<- diff(artificial.ts,lag = 3,differences = 1)
artificial3.sd2<- diff(artificial3.sd1,lag = 12,differences = 1)
artificial3.sd3<- diff(artificial3.sd2,lag = 12,differences = 1)
artificial3.sd4<- diff(artificial3.sd2,differences = 1)

artificial3.sd3sd12<- diff(artificial3.sd1,lag = 6,differences = 2)
spectrum(artificial3.sd3sd12,span = 2)
spectrum(artificial3.sd1,span = 2)
spectrum(artificial.ts)
ggtsdisplay(artificial3.ts)
ggtsdisplay(artificial3.sd1)
ggtsdisplay(artificial3.sd2)
spectrum(artificial3.sd2,span = 2)
Box.test(artificial3.sd2,lag=12,type="Ljung-Box")
ggtsdisplay(artificial3.sd3)
spectrum(artificial.ts,span = 2)
spectrum(artificial3.sd3,span = 2)
ggtsdisplay(artificial3.sd4,lag.max = 24)
Box.test(artificial3.sd3,lag = 12, type = "Ljung-Box")

artificial3_fit.sd1<- diff(artificial3_fit.ts,lag = 3,differences = 1)
ggtsdisplay(artificial3.sd1)
artificial3.sd1.deco<-decompose(artificial3.sd1,type = "additive")
plot(artificial3.sd1.deco)
# Shapiro for random component
shapiro.test(artificial3.sd1.deco$random)
# Correlation from the random component
tsdisplay(artificial3.sd1.deco$random)
# Ljung-Box test
Box.test(artificial3.sd1.deco$random,lag=12,type="Ljung-Box")

#########################################################
#                DECOMPOSITION
#########################################################
# Decomposition of the series into its trend, seasonality and random components 
artificial3.deco<-decompose(artificial3.ts,type = "additive")
# Fixing of the time scale
artificial3.deco$seasonal<- ts(artificial3.deco$seasonal, start=c(1990,1),frequency = 12)
artificial3.deco$trend<- ts(artificial3.deco$trend, start=c(1990,1),frequency = 12)
artificial3.deco$random<- ts(artificial3.deco$random, start=c(1990,1),frequency = 12)
artificial3.deco$x<- ts(artificial3.deco$x, start=c(1990,1),frequency = 12)
# Plotting of the components
plot(artificial3.deco,xlab = "Years")
plot(cbind(Observed = artificial3.deco$x, Trend = artificial3.deco$trend, Seasonal = artificial3.deco$seasonal, 
           Random = artificial3.deco$random), main = "Decomposition of Artificial Time Series",xlab = "Years") 
# Shapiro for random component
shapiro.test(artificial3.deco$random)
# Correlation from the random component
tsdisplay(artificial3.deco$random)
# Ljung-Box test
Box.test(artificial3.deco$random,lag=12,type="Ljung-Box")

#-------------------12 Frequency ------------------------

# Decomposition of the series into its trend, seasonality and random components 
artificial.deco<-decompose(artificial.ts,type = "additive")
# Plotting of the components
plot(artificial.deco,xlab = "Years")
plot(cbind(Observed = artificial.deco$x, Trend = artificial.deco$trend, Seasonal = artificial.deco$seasonal, 
           Random = artificial.deco$random), main = "Decomposition of Artificial Time Series",xlab = "Years") 
# Shapiro for random component
shapiro.test(artificial.deco$random)
# Correlation from the random component
tsdisplay(artificial.deco$random)
# Ljung-Box test
Box.test(artificial.deco$random,lag=12,type="Ljung-Box")

#########################################################
#                EXPONENTIAL SMOOTHING
#########################################################
# Exponential smoothing
artificial3.ses<-HoltWinters(artificial3_fit.ts);
plot(artificial3.ses)
artificial3.ses
artificial3.ses.fore<-forecast.HoltWinters(artificial3.ses,h=7);
plot.forecast(artificial3.ses.fore,xlab = "Year",main = "HoltWinters for Artificial Time Series")
accuracy(artificial3.ses.fore,artificial3_val.ts)

#-------------------AUTOMATIC EXPONENTIAL SMOOTHING
artificial3.ets <- ets(artificial3.ts)
plot(artificial3.ets)
artificial3.ets
artificial3.ets.fore<-forecast(artificial3.ets,h=19);
plot.forecast(artificial3.ets.fore)
#Automatic exponential smoothing for the fit data
artificial3_fit.ets <- ets(artificial3_fit.ts)
artificial3_fit.ets

#----------------d1--------------------------------
# Fit the model
artificial3_fit.d1.ses<-HoltWinters(artificial3_fit.d1,beta = FALSE);
artificial3_fit.d1.ses
artificial3_fit.d1.ses.fore<-forecast.HoltWinters(artificial3_fit.d1.ses,h = 7)
accuracy(artificial3_fit.d1.ses.fore$mean,artificial3_val.d1)
#Plot the fitted model
plot(artificial3_fit.d1.ses)

# See the parameters
artificial3_fit.d1.ses
# Create a forecast
artificial3_fit.d1.ses.fore<-forecast.HoltWinters(artificial3_fit.d1.ses,h=7);
HoltWinters(artificial3_val.d1,model =artificial3_fit.d1.ses.fore$model)

# Plot the forecast
plot.forecast(artificial3_fit.d1.ses.fore,xlab = "Year",main = "HoltWinters for Artificial Time Series")

artificial3_fit.d1.ses.fore
diffinv(artificial3_fit.ses.fore$mean)
#--------------------d1sd1 Freq 3-----------------------
artificial3.sd1d1.hw<-HoltWinters(artificial3.sd1d1,beta = FALSE)
artificial3.sd1d1.hw
artificial3.sd1d1.hw.fore<-forecast.HoltWinters(artificial3.sd1d1.hw, h = 18)
plot.forecast(artificial3.sd1d1.hw.fore)
#########################################################
#                SARIMA MODEL
#########################################################

dev.off()
par(mar=c(4,4,4,4))
