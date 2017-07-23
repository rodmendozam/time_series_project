# Load the data
ener_sweden.ts <- ts(group22_data_2$ener_sweden,start = c(1960,1));
gdp_sweden.ts <- ts(group22_data_2$gdp_sweden,start = c(1960,1));

# Remove na values
ener_sweden.ts <-na.omit(ener_sweden.ts)
gdp_sweden.ts <-na.omit(gdp_sweden.ts)

###############################################
# DIFF
###############################################

ener_sweden.d1<-diff(ener_sweden.ts,differences = 1)
gdp_sweden.d2<- diff(gdp_sweden.ts,differences = 2)
ggtsdisplay(ener_sweden.d1)
ggtsdisplay(gdp_sweden.d1)
spectrum(ener_sweden.d1)
spectrum(gdp_sweden.d1)
###############################################
# Fingerprint of the data
###############################################
ggtsdisplay(ener_sweden.ts,main = "Energy Sweden",xlab = "Year", ylab = "toe")
ggtsdisplay(gdp_sweden.ts,main = "GDP Sweden", xlab = "Year", ylab = "US dollar")
plot(gdp_sweden.ts)
# Spectral decomposition of the data 
spectrum(ener_sweden.ts,span = 2,ylab = "Spectral coefficients",main = "Frequency Domain: Energy Sweden", xlab ="Frequency[cycle/year]")
spectrum(gdp_sweden.ts,span = 2,ylab = "Spectral coefficients" ,main = "Frequency Domain: GDP Sweden", xlab ="Frequency[cycle/year]")


