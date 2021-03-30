setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/')

library(openxlsx)
library(leaps)
source('Scripts/Auxiliary_Functions.R')
source('../Emulation.R')


###############################################################################
# LOAD OBSERVED AND SIMULATED TEMPERATURES: hourly, for one year (8760 obs)
###############################################################################

file <- "Data/Temperature_Data/Space Temperatures - Originals - For Dario.xlsx"
file <- "Data/Temperature_Data/Actual Space Temperatures - Dario.xlsx"

## DATES AND TIMES
DateTimes <- read.xlsx(file, sheet = 1)[,1]
DateTimes <- as.POSIXct(DateTimes*86400, origin="1899-12-30", tz='Europe/London')

## KITCHEN TEMPERATURES
# Data between 12/01 and 21/01 is not available (indices 264-503)
Kitch.Obs2 <- read.xlsx(file, sheet = 1) 
Kitch.Obs2 <- Kitch.Obs2[,"Kitchen.(A)"]
Kitch.Obs <- Kitch.Obs[,"Actual.(°C)"]

## MASTER TEMPERATURES
# Data between 12/01 and 21/01 is not available (indices 264-503)
Master.Obs <- read.xlsx(file, sheet = 2) 
Master.Obs <- Master.Obs[,"Actual"]
rm(file)

## LOAD SIMULATED TEMPERATURES
# Kitch.Sim & Master.Sim: 8760 x 1000
load('RData/Simulated-Temperatures.RData')


################################################################
##                 CODE TO GENERATE AND SAVE PLOTS
################################################################


## PLOT: OBSERVED TIME SERIES
pdf("Observed/Obs_Kitch.pdf", width = 10, height = 6)
plot(DateTimes, Kitch.Obs2, ty='l', col='blue', xaxt='n', ann = F)
axis.POSIXct(side=1, x=DateTimes, format = "%d %b")       # format of x tick-labels
title(main = 'Observed Kitchen Temperature', line = 1)    # title
mtext(side = 1, text = "2016", line=2.5, cex=1.1)                  # x label
mtext(side = 2, text = "Temperature  [\u00B0C]", line = 2.5, cex=1.1) # y label
graphics.off()


## PLOT: SIMULATED ON TOP OF OBSERVED
times <- 1:length(DateTimes)
col_obs <- rgb(0,0,0, 0.3)
col_sim <- "red"
# Plots for n random simulations
n <- 5
for (i in 1:n){
  ind <- ceiling(1000*runif(1))
  filename <- paste("Simulated/Sim_Kitch_", as.character(ind), "_noylab.pdf", sep = "")
  filename <- paste("Simulated/Sim_Kitch_", as.character(ind), ".pdf", sep = "")
#  pdf(filename, width = 8, height = 6)
  plot(DateTimes[times], Kitch.Sim[times,ind], ty='l', col=col_sim,
       ylim = c(11,28), ann = F, cex.axis=1.4)
  mtext(side = 2, text = "Temperature  [\u00B0C]", line = 2.5, cex=1.4) # y label
  lines(DateTimes[times], Kitch.Obs[times], ty='l', col=col_obs)
  legend('topright', legend = c("Simulated", "Observed"), cex=1.4, col = c(col_sim, col_obs), lty = 1)
 # dev.off()
}


## ZOOMING IN
zoom <- 740:850
#ind <- ceiling(1000*runif(1))
filename <- paste("Zoom_", as.character(ind), ".pdf", sep = "")

pdf(filename, width = 10, height = 6)
plot(DateTimes[zoom], Kitch.Sim[zoom,ind], ty='l', col = 'red', xaxt='n', ann = F, cex.axis=1.2, lwd = 1.5)
axis.POSIXct(side=1, x=DateTimes[zoom], format = "%d %b", cex.axis=1.2)       # format of x tick-labels
title(main = 'Simulated Kitchen Temperature', line = 1.3, cex.main=1.6)       # title
mtext(side = 2, text = "Temperature  [\u00B0C]", line = 2.5, cex=1.2) # y label
graphics.off()








