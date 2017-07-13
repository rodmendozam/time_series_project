# Load the data
gdp_sweden.ts <- ts(group22_data_2$gdp_sweden,start = c(1960,1));

# Fingerprint of the data
ggtsdisplay(gdp_sweden.ts)

gdp_sweden.spec<-spectrum(gdp_sweden.ts,span = 2)
plot(gdp_sweden.spec$freq,gdp_sweden.spec$spec)
gdp_sweden.spec$spec
gdp_sweden.spec$freq

ggtsdisplay(gdp_sweden.spec$spec)


