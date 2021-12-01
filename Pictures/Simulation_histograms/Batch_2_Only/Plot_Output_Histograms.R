#############################################################################
#
# For both gas and electricity, a 3x4 plot with histograms of the 1000 simulated
# consumptions for each month is produced. A vertical line is added at the 
# observed consumption.
# Moreover, 12 plots of the single histograms are produced.
#
# This script and the pictures produced (in png/pdf) can be found at:
# 'UQ_Energy_Building/Pictures/Simulation_Histograms/Batch_2_Only'
#
#############################################################################

#########################################################################
#  SET PRELIMINARY VARIABLES AND FUNCTIONS 
#########################################################################


## Set folder
setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/Pictures/')

## Colour of histogram bars and vertical line (observed data)
col_hist <- "#00A600FF"
col_line <- "#FF5500FF" # orange

## CUSTOMISED FUNCTION PLOTTING HISTOGRAM + VERTICAL LINE
# sim: vector of simulated values (used for histogram)
# obs: one value for the observation (vertical line added at that value)
# fac: factor for rescaling the (min, max) range of c(sim, obs)
# l_wdt: line width for added vertical line.
# If length(sim)=1, then a vertical line rather than a histogram is produced

myplot <- function(sim, obs, fac, l_wdt){
  
  # Find range including simulation outputs and observation, 
  # and rescale it by 'fac' times around its center
  rg <- range(c(sim, obs)) 
  shift <- (fac-1)*( (rg[1]+rg[2])/2 )    
  rg <- fac*rg - shift                
  
  # If only one sim value, plot a vertical line for it. Otherwise histogram
  if (length(sim)==1){
    hist(0, xlim = rg, main="", xlab="", ylab="")  # empty histogram 
    abline(v=sim, col=col_hist, lwd=l_wdt, lty=1)  # add vertical line at simulated value
  }
  else{
    hist(sim, breaks=nbin[month], xlim=rg, col=col_hist, 
         main="", xlab="", ylab="")
  }
  
  # Add vertical line corresponding to observation
  abline(v=obs, col=col_line, lwd= l_wdt, lty=2)
}


#########################################################################
####                            GAS PLOTS
#########################################################################

# Load gas data
load('../RCode/RData/Inputs/Simulated_and_Observed_Gas.RData')

## Number of bins for each plot (by month)
#              J  F  M  A  M  J  J  A  S  O  N  D
nbin <-  c(15,16,15,15,14, 8, 9,15,15,15,15,15)


#################################################
# 3x4 PLOT WITH GAS HISTOGRAMS FOR ALL MONTHS

file.name <- 'Simulation_histograms/Batch_2_Only/Gas_Runs/All_months_gas.png'
ln = 1.3                     # distance between title and plot box

png(file.name, width = 12, height = 4, unit="in", res=288)
par(mfrow=c(2,6), 
    cex.lab = 1.2, cex.axis =1.2, cex.main = 1.4,
    mai=c(0.5, 0.5, 0.3, 0.1), # for each subplot: bott, left, top, right margins
    mgp = c(2.8,1,0)         # mgp[1] = dist label-axis; mgp[2] = dist numbers-axis; 
                             # mgp[3] = dist axis line from plot box. Default: 3,1,0
)

for (month in 1:12){
  sim <- Gas.Sim[, month]
  obs <- Gas.Obs[month]
  fac <- 1.1               # Factor rescaling range of histograms's x-axis

  # Plot
  myplot(sim, obs, fac, 3.5)                            # Histogram + vertical line 
  title( paste(month.name[month], 'Gas'), line = ln)    # Title
  
  # If one of the left-most plots, add y-label
  if (month%%6 == 1){
    title(ylab = "Frequency")
  }
  # If last row, add x-label
  if (month>6.5){
    title(xlab = "Gas consumption [kWh]")
  }
}
dev.off()


##########################################
# PLOTS OF SINGLE HISTOGRAMS (GAS)

for (month in 1:12){
  sim <- Gas.Sim[, month]
  obs <- Gas.Obs[month]
  fac <- 1.1               # Factor rescaling range of histograms's x-axis
  
  file.name <- paste('Simulation_histograms/Batch_2_Only/Gas_Runs/Single_months/', 
                     month.names[month], "_Gas.pdf", sep = "")
  pdf(file.name)
  
  # Plot 
  par(cex=1.6, lwd=1.5)                 # Graphical parameters
  myplot(sim, obs, fac, 6.5)            # Histogram + vertical line 
  th <- paste(month.names[month], 'Gas consumption')
  title(main = th, xlab="Gas consumption [kWh]", ylab = "Frequency")

  dev.off()
}



#########################################################################
##########               ELECTRICITY PLOTS          
########################################################################


# Load electricity data
load('../RCode/RData/Inputs/Simulated_and_Observed_Elec.RData')

## Number of bins for each plot (by month)
#               J  F  M  A  M   J   J    A  S  O  N  D
nbin <-  c(15,16,15,15,14, NA, NA, NA,15,15,15,15)

# Summer months show no variation at all in the simulated electricity consumption,
# so future plots will be different for summer and non_summer months.
summer <- c(6,7,8)  # June, July, August


#################################################
# 6x2 PLOT WITH ELEC HISTOGRAMS FOR ALL MONTHS

file.name <- 'Simulation_histograms/Batch_2_Only/Elec_Runs/All_months_elec.png'

png(file.name, width = 12, height = 4, unit="in", res=288)
par(mfrow=c(2,6),
    cex.lab = 1.2, cex.axis =1.2, cex.main = 1.4,
    mai=c(0.5, 0.5, 0.3, 0.1), # for each subplot: bott, left, top, right margins
    mgp = c(2.8,1,0)         # mgp[1] = dist label-axis; mgp[2] = dist numbers-axis;
                             # mgp[3] = dist axis line from plot box. Default: 3,1,0
)

for (month in 1:12){
  
  # Take just first or all simulated values according to summer/non_summer
  if (month %in% summer) sim <- Elec.Sim[1, month]
  else sim <- Elec.Sim[, month]
  
  obs <- Elec.Obs[month]
  # Factor rescaling range of histograms's x-axis
  fac <- ifelse(month %in% summer, 1.2, 1.1)    # = 1.2 for summer, 1.1 otherwise

  # Plot
  myplot(sim, obs, fac, 3.5)                            # Histogram + vertical line 
  title( paste(month.name[month], 'Elec'), line = ln)    # Title
  
  # If one of the left-most plots, add y-label
  if (month%%6 == 1){
    title(ylab = "Frequency")
  }
  # If last row, add x-label
  if (month>6.5){
    title(xlab = "Elec consumption [kWh]")
  }
}
dev.off()


################################################
# PLOTS OF SINGLE HISTOGRAMS (ELECTRICITY)

for (month in 1:12){
  # Take just first or all simulated values according to summer/non_summer
  if (month %in% summer) sim <- Elec.Sim[1, month]
  else sim <- Elec.Sim[, month]
  
  obs <- Elec.Obs[month]
  # Factor rescaling range of histograms's x-axis
  fac <- ifelse(month %in% summer, 1.2, 1.1)    # = 1.2 for summer, 1.1 otherwise
  
  file.name <- paste('Simulation_histograms/Batch_2_Only/Elec_Runs/Single_months/', 
                     month.names[month], "_Elec.pdf", sep = "")
  pdf(file.name)
  
  # Plot 
  par(cex=1.6, lwd=1.5)                 # Graphical parameters
  myplot(sim, obs, fac, 6.5)            # Histogram + vertical line 
  th <- paste(month.names[month], 'Elec consumption')
  title(main = th, xlab="Electricity consumption [kWh]", ylab = "Frequency")
  
  dev.off()
}



# Add legend
#legend("topright", legend=c("Output Ensemble", "Observation"), col=c(col_hist, col_line), 
#      pch=c(15,NA), pt.cex=3, y.intersp=2, x.intersp=0.75, lty=c(0,0,2), inset=0.05, lwd=2.5, cex=0.7)
#legend("topright", legend=c("Output Ensemble", "Observation"), col="black", 
#       pch=c(0,NA), pt.cex=3, y.intersp=2, x.intersp=0.75, lty=0, inset=0.05, lwd=1.5, bty="n", cex=0.7)  


