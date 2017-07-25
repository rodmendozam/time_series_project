###############################
###### Dynamic Regression #####
###############################

##### DATA DEFINITION #####
# load series by code
load('time_series/group22_data_2.RData')
load('time_series/group22_data_1.RData')

# load ts
ener_sweden.ts <- ts(group22_data_2$ener_sweden, start = c(1960,1))
gdp_sweden.ts <- ts(group22_data_2$gdp_sweden, start = c(1960,1))

##### SUMMARY ##### 
summary(ener_sweden.ts)
summary(gdp_sweden.ts)
length(ener_sweden.ts)
length(gdp_sweden.ts)

##### MISSING VALUES #####
# Remove the last value of the ts
# energ has a NAN at the end, gdp has a value: remove both
ener_sweden.ts <- na.omit(ener_sweden.ts)
gdp_sweden.ts <- gdp_sweden.ts[1: length(gdp_sweden.ts)-1]

##### SPLIT TRAIN VALID #####
split <- floor(length(ener_sweden.ts)*0.8)
y_train <- ener_sweden.ts[1:split]
y_valid <- ener_sweden.ts[(split+1):length(ener_sweden.ts)]
x_train <- gdp_sweden.ts[1:split]
x_valid <- gdp_sweden.ts[(split+1):length(gdp_sweden.ts)]



##### FINGERPRINTS #####
ggtsdisplay(ener_sweden.ts)
ggtsdisplay(gdp_sweden.ts)

##### ASSUMPTIONS #####
# there are two assumptions: variables stationary and predictor variable exogenous
# Stationary, check acf pacf and decide the differencing
ener_sweden.ts.s1 <- diff(log(ener_sweden.ts), differences = 1)
gdp_sweden.ts.s1 <- diff(log(gdp_sweden.ts), differences = 1)
# gdp_sweden.ts.s1.s7 <- diff(gdp_sweden.ts.s1, lag = 7, differences = 1)
ggtsdisplay(ener_sweden.ts.s1)
ggtsdisplay(gdp_sweden.ts.s1)
# ggtsdisplay(gdp_sweden.ts.s1.s7)

##### CORRELATION #####
# relationship between the variables


##### MODEL #####
# Dynamic Regression
ener_sweden.proxy <- Arima(ener_sweden.ts.s1, xreg=gdp_sweden.ts.s1,  order = c(2,0,0))
ener_sweden.proxy
tsdisplay(residuals(ener_sweden.proxy))
par(mar = rep(2, 4))

shapiro.test(residuals(ener_sweden.proxy))

Box.test(residuals(ener_sweden.proxy),lag=12,type="Ljung-Box")
accuracy

#Automatic dynamic regression
ener_sweden.auto <- auto.arima(ener_sweden.ts.s1, xreg=gdp_sweden.ts.s1)
ener_sweden.auto
tsdisplay(residuals(ener_sweden.auto))
shapiro.test(residuals(ener_sweden.auto))
Box.test(residuals(ener_sweden.auto),lag=12,type="Ljung-Box")
names(ener_sweden.auto)
plot(ener_sweden.auto$xreg)
ener_sweden.dyn<-diffinv(ener_sweden.auto$xreg,xi = log(2.700534),differences = 1)
plot(exp(ener_sweden.dyn))
lines(ener_sweden.ts)
plot(ener_sweden.ts)
dev.off()
##### VALIDATION #####
# split x_train, y_train, x_val, y_val...80% and 20%


##### FORECAST #####
# does the assignment ask for this?
# for forecasting we need future values of gdp to insert into the forecast
# maybe use univariant for gdp, predict some values
e <- rep(tail(ener_sweden.ts.s1, n=1), 10) # get 10 values 
f <- forecast(ener_sweden.auto, h=10, xreg = ts(e)) # forecast
f$mean<-exp(diffinv(f$mean,differences = 1,xi = log(5.511750)))
f$x<-exp(diffinv(f$x,differences = 1,xi = log(2.700534)))
a<-exp(diffinv(f$upper[,1],differences = 1,xi = log(5.511750)))
a <-a[1:length(a)-1]
length(a)
f$upper[,1]<-a
a<-exp(diffinv(f$upper[,1],differences = 1,xi = log(5.511750)))
a <-a[1:length(a)-1]
length(a)
f$upper[,1]<-a



f$upper[,1]<-exp(diffinv(f$upper[,1],differences = 1,xi = log(5.511750)))
f$upper[,2]<-exp(diffinv(f$upper[,2],differences = 1,xi = log(5.511750)))
f$lower[,1]<-exp(diffinv(f$lower[,1],differences = 1,xi = log(5.511750)))
f$lower[,2]<-exp(diffinv(f$lower[,2],differences = 1,xi = log(5.511750)))
plot(f) # plot


##### #####

##### #####




























