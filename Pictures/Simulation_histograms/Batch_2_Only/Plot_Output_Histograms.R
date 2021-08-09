#############################################################################
#
# For both gas and electricity, and for each of the 12 months, this script 
# produces histograms of the 1000 simulated consumptions. A vertical line is
# added to denote the value of the observed consumption.
#
# This script and the pictures produced (in pdf) can be found at:
# 'UQ_Energy_Building/Pictures/Simulation_Histograms/Batch_2_Only'
#
#############################################################################


# SET FOLDER
setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/Pictures/')

## COLOUR OF HISTOGRAM BARS AND VERTICAL LINE (OBSERVED DATA)
col_hist <- "#00A600FF"
col_line <- "#4C00FFFF" # blue
col_line <- "#FF0000FF" # red-orange
col_line <- "#FF5500FF" # orange


#######################################################################
## ACTUAL PLOTS, GAS AND ELECTRICITY (histogram plus vertical bar)

## GAS ##

# Load gas data
load('../RCode/RData/Inputs/Simulated_and_Observed_Gas.RData')

## Number of bins for each plot (by month)
#              J  F  M  A  M  J  J  A  S  O  N  D
nbin.gas <-  c(15,16,15,15,14, 8, 9,15,15,15,15,15)

for (month in 1:12){
  x <- Gas.Sim[, month]
  obs <- Gas.Obs[month]
  
  # FIND SUITABLE RANGE INCLUDING BOTH SIMULATION OUTPUTS AND OBSERVED DATA
  rg <- range(c(x, obs)) 
  fac <- 1.1
  # The following lines widen the interval rg of a factor fac, around its centre
  shift <- (fac-1)*( (rg[1]+rg[2])/2 )
  rg <- fac*rg - shift
  
  # Title string of plot
  th <- paste(month.names[month], 'Gas consumption')
  
  file.name <- paste('Simulation_histograms/Batch_2_Only/Gas_Runs/', 
                     month.names[month], "_Gas.pdf", sep = "")
  pdf(file.name)
  par(cex=1.6, lwd=1.5)
  
  # Plot the histogram of 1000 simulated consuptions
  hist(x, main=th, breaks=nbin.gas[month], xlim=rg, col=col_hist, 
       xlab="Gas consumption [kWh]")
  
  # Add vertical line corresponding to observation
  abline(v=obs, col=col_line, lwd=6.5, lty=2)
  
  dev.off()
}


## ELECTRICITY ##

# Load gas data
load('../RCode/RData/Inputs/Simulated_and_Observed_Elec.RData')

## Number of bins for each plot (by month)
#               J  F  M  A  M   J   J    A  S  O  N  D
nbin.elec <-  c(15,16,15,15,14, NA, NA, NA,15,15,15,15)

# The code for the plots of June, July and August is done separately, since each 
# of these shows no variation at all in the simulated electricity consumption

# Non-summer months
for (month in c(1:5, 9:12)){
  x <- Elec.Sim[, month]
  obs <- Elec.Obs[month]
  
  # FIND SUITABLE RANGE INCLUDING BOTH SIMULATION OUTPUTS AND OBSERVED DATA
  rg <- range(c(x, obs)) 
  fac <- 1.1
  # The following lines widen the interval rg of a factor fac, around its centre
  shift <- (fac-1)*( (rg[1]+rg[2])/2 )
  rg <- fac*rg - shift
  
  # Title string of plot
  th <- paste(month.names[month], 'Elec consumption')
  
  file.name <- paste('Simulation_histograms/Batch_2_Only/Elec_Runs/', 
                     month.names[month], "_Elec.pdf", sep = "")
  pdf(file.name)
  par(cex=1.6, lwd=1.5)
  
  # Plot the histogram of 1000 simulated consuptions
  hist(x, main=th, breaks=nbin.elec[month], xlim=rg, col=col_hist, 
       xlab="Electricity consumption [kWh]")
  
  # Add vertical line corresponding to observation
  abline(v=obs, col=col_line, lwd=6.5, lty=2)
  
  dev.off()
}


# The 3 summer months
# Non-summer months
for (month in 6:8){
  sim <- Elec.Sim[1, month]
  obs <- Elec.Obs[month]
  
  # FIND SUITABLE RANGE INCLUDING BOTH SIMULATION OUTPUTS AND OBSERVED DATA
  rg <- range(c(sim, obs)) 
  fac <- 1.2
  # The following lines widen the interval rg of a factor fac, around its centre
  shift <- (fac-1)*( (rg[1]+rg[2])/2 )
  rg <- fac*rg - shift
  
  # Title string of plot
  th <- paste(month.names[month], 'Elec consumption')
  
  file.name <- paste('Simulation_histograms/Batch_2_Only/Elec_Runs/', 
                     month.names[month], "_Elec.pdf", sep = "")
  pdf(file.name)
  par(cex=1.6, lwd=1.5)
  
  # Plot the histogram of 1000 simulated consuptions
  hist(2*max(sim, obs), main=th, xlim=rg,
       xlab="Electricity consumption [kWh]")
  
  # Add vertical line corresponding to simulated value
  abline(v=sim, col=col_hist, lwd=6.5, lty=2)
  
  # Add vertical line corresponding to observation
  abline(v=obs, col=col_line, lwd=6.5, lty=2)
  
  dev.off()
}



# Add legend
#legend("topright", legend=c("Output Ensemble", "Observation"), col=c(col_hist, col_line), 
#      pch=c(15,NA), pt.cex=3, y.intersp=2, x.intersp=0.75, lty=c(0,0,2), inset=0.05, lwd=2.5, cex=0.7)
#legend("topright", legend=c("Output Ensemble", "Observation"), col="black", 
#       pch=c(0,NA), pt.cex=3, y.intersp=2, x.intersp=0.75, lty=0, inset=0.05, lwd=1.5, bty="n", cex=0.7)  


