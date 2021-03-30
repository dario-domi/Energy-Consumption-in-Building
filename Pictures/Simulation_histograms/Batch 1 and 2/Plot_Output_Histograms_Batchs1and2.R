##########################################################################
# # #  CODE TO PLOT AND SAVE JOINT HISTOGRAMS (OF BATCHES 1 AND 2)   # # #
##########################################################################

month.names <- c("January", "February", "March", "April", "May", "June", "July", 
                 "August", "September", "October", "November", "December")

# OUTPUTS BATCH 1 (1000 x 12)
Temp <- read.csv("Data/results-batch1.csv", skip = 3008, nrow=1001)
Outputs_Gas_1 <- Temp[-1,]             # remove baseline simulation
Outputs_Gas_1[,"File.Name"] <- NULL    # remove first column with file name of runs
rownames(Outputs_Gas_1) <- 1:1000      # name the simulations by numbers
colnames(Outputs_Gas_1) <- month.names # assign month names

# OUTPUTS BATCH 2 (1000 x 12)
Temp <- read.csv("Data/results-batch2.csv", skip = 3008, nrow=1001)
Outputs_Gas_2 <- Temp[-1,]             # remove baseline simulation
Outputs_Gas_2[,"File.Name"] <- NULL    # remove first column with file name of runs
rownames(Outputs_Gas_2) <- 1:1000      # name the simulations by numbers
colnames(Outputs_Gas_2) <- month.names # assign month names

rm(Temp)

# COLOUR OF HISTOGRAM BARS
col1 <- rgb(1,0,0, 0.6)
col2 <- rgb(1,1,0, 0.6)
col3 <- "blue"

for (month in 1:12){
  x1 <- Outputs_Gas_1[, month]
  x2 <- Outputs_Gas_2[, month]
  
  # Store global min and max in 'rg'
  rg <- range(c(x1,x2))
  
  # Title string of plot
  th <- paste(month.name[month], 'Gas consumption')
  
  file.name <- paste(month.name[month], "_Gas.pdf", sep = "")
  pdf(file.name)
  par(cex=1.3)
  
  # Plot the two histograms
  hist(x1, main=th, xlim=rg, breaks=15, col=col1, freq=F, xlab="Simulated Gas consumption [kWh]")
  hist(x2, breaks=15, col=col2, add=T, freq=F)
  
  # Add vertical line corresponding to observation
  obs <- Obs_Gas[month]
  abline(v=obs, col=col3, lwd=3, lty=2)
  
  # Add legend
  legend("topright", legend=c("Batch 1", "Batch 2", "Observation"), col=c(col1, col2, col3), 
         pch=c(15,15,NA), pt.cex=3, y.intersp=2, x.intersp=0.75, lty=c(0,0,2), inset=0.05, lwd=2.5, cex=0.7)
  legend("topright", legend=c("Batch 1", "Batch 2", "Observation"), col="black", 
         pch=c(0,0,NA), pt.cex=3, y.intersp=2, x.intersp=0.75, lty=0, inset=0.05, lwd=1.5, bty="n", cex=0.7)  
  dev.off()
}
