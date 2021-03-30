##########################################################################
# # #      CODE TO PLOT AND SAVE HISTOGRAMS OF BATCH 2 OUTPUTS       # # #
##########################################################################


month.names <- c("January", "February", "March", "April", "May", "June", "July", 
                 "August", "September", "October", "November", "December")

load("../../Scripts/CV_results.RData")

Obs_Gas <- matrix(c(3201, 2130, 1687, 1452, 1109, 628, 588, 520, 563, 1010, 1985, 2564), ncol = 12)  
colnames(Obs_Gas) <- month.names                                  # name the column by months

# COLOUR OF HISTOGRAM BARS AND VERTICAL LINE (OBSERVED DATA)
col_hist <- "#00A600FF"
col_line <- rgb(0,1,0)
col_line <- "#4C00FFFF"
col_line <- "#FF5500FF"
#col_line <- "#FF0000FF"

# NUMBER OF BINS FOR EACH PLOT (EACH MONTH)
#          J  F  M  A  M  J  J  A  S  O  N  D
nbin <- c(15,16,15,15,14, 8, 9,15,15,15,15,15)

# ACTUAL PLOTS
for (month in 1:12){
  x <- Outputs_Gas[, month]
  obs <- Obs_Gas[month]
  
  # FIND SUITABLE RANGE INCLUDING BOTH SIMULATION OUTPUTS AND OBSERVED DATA
  rg <- range(c(x, obs)) 
  fac <- 1.1
  # The following lines widen the interval rg of a factor fac, around its centre
  shift <- (fac-1)*( (rg[1]+rg[2])/2 )
  rg <- fac*rg - shift
  
  # Title string of plot
  th <- paste(month.name[month], 'Gas consumption')
  
  file.name <- paste(month.name[month], "_Gas.pdf", sep = "")
  pdf(file.name)
  par(cex=1.6, lwd=1.5)
  
  # Plot the two histograms
  hist(x, main=th, breaks=nbin[month], xlim=rg, col=col_hist, xlab="Simulated Gas consumption [kWh]")
  
  # Add vertical line corresponding to observation
  abline(v=obs, col=col_line, lwd=6.5, lty=2)
  
  # Add legend
  #legend("topright", legend=c("Output Ensemble", "Observation"), col=c(col_hist, col_line), 
   #      pch=c(15,NA), pt.cex=3, y.intersp=2, x.intersp=0.75, lty=c(0,0,2), inset=0.05, lwd=2.5, cex=0.7)
  #legend("topright", legend=c("Output Ensemble", "Observation"), col="black", 
  #       pch=c(0,NA), pt.cex=3, y.intersp=2, x.intersp=0.75, lty=0, inset=0.05, lwd=1.5, bty="n", cex=0.7)  
  dev.off()
}
