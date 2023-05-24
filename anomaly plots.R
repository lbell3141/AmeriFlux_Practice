par(mfrow = c(2,3))
plot(grszn_anom$years, grszn_anom$sd_prcp_mon, 
     main = "Precipitation w/ Z-Score, 2008-2014",
     xlab = "Years",
     ylab = "Precip",
     ylim = c(-1.5,1.5),
     type = 'l', lwd = 1)
points(grszn_anom$years, grszn_anom$sd_prcp_mon)
abline(h = 0, lty = 3) 

plot(grszn_anom$years, grszn_anom$sd_ts, 
     main = "Soil Temp w/ Z-Score, 2008-2014",
     xlab = "Years",
     ylab = "Soil Temp",
     ylim = c(-1.5,1.5),
     type = 'l', lwd = 1)
points(grszn_anom$years, grszn_anom$sd_ts)
abline(h = 0, lty = 3)  

plot(grszn_anom$years, grszn_anom$sd_nee_mon, 
     main = "NEE w/ Z-Score, 2008-2014",
     xlab = "Years",
     ylab = "NEE",
     ylim = c(-1.5,1.5),
     type = 'l', lwd = 1)
points(grszn_anom$years, grszn_anom$sd_nee_mon)
abline(h = 0, lty = 3)

plot(grszn_gpp$years, grszn_gpp$sd_an_z, 
     main = "GPP w/ Z-Score, 2008-2014",
     xlab = "Years",
     ylab = "GPP",
     ylim = c(-1.5,1.5),
     type = 'l', lwd = 1)
points(grszn_gpp$years, grszn_gpp$sd_an_z)
abline(h = 0, lty = 3)

plot(dat_grszn$years, dat_grszn$sd_an_z,
     main = "SWC w/ Z-Score, 2008-2014",
     xlab = "Years",
     ylab = "SWC",
     ylim = c(-0.5, 0.5),
     type = 'l', lwd = 1)
points(dat_grszn$years, dat_grszn$sd_an_z)
abline(h = 0, lty = 3)
