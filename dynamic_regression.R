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

##### DIFF #####
gdp_sweden.d1<-diff(log(gdp_sweden.ts),differences = 1)
ener_sweden.d1<-diff(log(ener_sweden.ts),differences = 1)

##### SPLIT TRAIN VALID #####
split <- floor(length(ener_sweden.d1)*0.8)
ener_sweden.ts.y_train <- ener_sweden.d1[1:split]
ener_sweden.ts.y_valid <- ener_sweden.d1[(split+1):length(ener_sweden.d1)]
gdp_sweden.ts.x_train <- gdp_sweden.d1[1:split]
gdp_sweden.ts.x_valid <- gdp_sweden.d1[(split+1):length(gdp_sweden.d1)]

##### FINGERPRINTS #####
ggtsdisplay(ener_sweden.ts)
ggtsdisplay(gdp_sweden.ts)

# ##### NORMALIZE, DIFF, CHECK ASSUMPTIONS #####
# # there are two assumptions: variables stationary and predictor variable exogenous
# # Stationary, check acf pacf and decide the differencing
# ener_sweden.ts.s1 <- diff(log(ener_sweden.ts.y_train), differences = 1)
# gdp_sweden.ts.s1 <- diff(log(gdp_sweden.ts.x_train), differences = 1)
# ggtsdisplay(ener_sweden.ts.s1)
# ggtsdisplay(gdp_sweden.ts.s1)
# # apply log transformation to y_valid, x_valid also
# ener_sweden.ts.y_valid.log <-log(ener_sweden.ts.y_valid)
# gdp_sweden.ts.x_valid.log <- log(gdp_sweden.ts.x_valid)

##### CORRELATION #####
# relationship between the variables


##### MODEL DYNAMIC REGRESSION #####
ener_sweden.proxy <- Arima(ener_sweden.ts.y_train, xreg=gdp_sweden.ts.x_train,  order = c(2,0,0))
# residuals
tsdisplay(residuals(ener_sweden.proxy))
par(mar = rep(2, 4)) # use in case of plot error
shapiro.test(residuals(ener_sweden.proxy))
Box.test(residuals(ener_sweden.proxy),lag=12,type="Ljung-Box")

##### VALIDATION DYNAMIC REGRESSION #####
ener_sweden.ts.fore <- forecast(ener_sweden.proxy, h = length(gdp_sweden.ts.x_valid), xreg = gdp_sweden.ts.x_valid) 
# Test the residuals
ggtsdisplay(ener_sweden.ts.fore$residuals)
shapiro.test(ener_sweden.ts.fore$residuals)
Box.test(ener_sweden.ts.fore$residuals,lag=12,type="Ljung-Box")
# Goodness of fit
accuracy(ener_sweden.ts.fore$mean, ener_sweden.ts.y_valid)

# Denormalize
x_train <- exp(diffinv(ener_sweden.ts.y_train, differences = 1, xi = log(2.700534)))
x_val <- exp(diffinv(ener_sweden.ts.y_valid, differences = 1, xi = log(5.670417)))
x_pred <- exp(diffinv(ener_sweden.ts.fore$mean, differences = 1, xi = log(5.670417)))
x_train <- x_train[1:length(x_train)-1]
x_val <- x_val[1:length(x_val)-1]
x_pred <- x_pred[1:length(x_pred)-1] 
# c.i. 95
x_pred_upper_95 <- exp(diffinv(ener_sweden.ts.fore$upper[,2], differences = 1, xi = log(5.670417)))
x_pred_lower_95 <- exp(diffinv(ener_sweden.ts.fore$lower[,2], differences = 1, xi = log(5.670417)))
x_pred_upper_95 <- x_pred_upper_95[1:length(x_pred_upper_95)-1]
x_pred_lower_95 <- x_pred_lower_95[1:length(x_pred_lower_95)-1]
# c.i. 80
x_pred_upper_80 <- exp(diffinv(ener_sweden.ts.fore$upper[,1], differences = 1, xi = log(5.670417)))
x_pred_lower_80 <- exp(diffinv(ener_sweden.ts.fore$lower[,1], differences = 1, xi = log(5.670417)))
x_pred_upper_80 <- x_pred_upper_80[1:length(x_pred_upper_80)-1]
x_pred_lower_80 <- x_pred_lower_80[1:length(x_pred_lower_80)-1]


# length(x_train)
# length(ener_sweden.ts.fore$x )
# length(x_val)
# length(ener_sweden.ts.fore$mean )

# plot by modifying ener_sweden.proxy
ener_sweden.ts.fore$x <- x_train
ener_sweden.ts.fore$mean <- x_pred
ener_sweden.ts.fore$upper[,2] <- x_pred_upper_95
ener_sweden.ts.fore$lower[,2] <- x_pred_lower_95
ener_sweden.ts.fore$upper[,1] <- x_pred_upper_80
ener_sweden.ts.fore$lower[,1] <- x_pred_lower_80
plot(ener_sweden.ts.fore)

# plot manually
nb_train <- length(x_train)
nb_val <- length(x_val)
indices <- c(1:(nb_train+nb_val))
x_train_and_nan <- c(x_train, rep(NaN, nb_val))
x_val_and_nan <- c(rep(NaN, nb_train), x_val)
x_pred_and_nan <- c(rep(NaN, nb_train), x_pred)
x_pred_ci_upr_and_nan <- c(rep(NaN, nb_train), x_pred_upper_95) #95% ci with 2, 80% ci with 1
x_pred_ci_lwr_and_nan <- c(rep(NaN, nb_train), x_pred_lower_95)
# plot train, valid and predictions note: run the code as block and not a single line
names <- c('train','valid', 'valid_pred')
colors <- c('#000000', '#000000', '#990000')
plot(indices, x_train_and_nan, type = "l", col = colors[1],
     main = "Validation from Dynamic", ylim = c(2, 17),
     xlab="Year index", ylab="toe")
polygon(c(indices, rev(indices)), c(x_pred_ci_upr_and_nan, rev(x_pred_ci_lwr_and_nan)), col = "#FFE4E4", border = NA)
lines(indices, x_val_and_nan, type = "l", col = colors[2], lwd=1, lty=2)
lines(indices, x_pred_and_nan, type = "l", col = colors[3], lwd=2)
lines(indices, x_pred_ci_upr_and_nan, type = "l", col = colors[3], lwd=1,lty=2)
lines(indices, x_pred_ci_lwr_and_nan, type = "l", col = colors[3], lwd=1,lty=2)
legend('topright', names, lty=c(1,2,1), lwd=2, col=colors, bty='n', cex=.95)





##### MODEL AUTOMATIC DYNAMIC REGRESSION #####
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

##### FORECAST WITH AUTOMATIC DYNAMIC REGRESSION #####
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




























