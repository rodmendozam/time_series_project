# Load the data
ener_sweden.ts <- ts(group22_data_2$ener_sweden,start = c(1960,1));

ener_sweden.ts <-na.omit(ener_sweden.ts)
# Fingerprint of the data
ggtsdisplay(ener_sweden.ts)

# Spectral decomposition of the data 
spectrum(ener_sweden.ts)



