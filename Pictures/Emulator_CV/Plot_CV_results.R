# This script loads the results of cross-validation from the R saved file "CV_results.RData" and plots these

%setwd("Pictures/Emulator_CV/")
load("../../Scripts/CV_results.RData")

month.names <- c("January", "February", "March", "April", "May", "June", "July", 
                 "August", "September", "October", "November", "December")

Obs_Gas <- matrix(c(3201, 2130, 1687, 1452, 1109, 628, 588, 520, 563, 1010, 1985, 2564), ncol = 12)  

col_hist <- "cornflowerblue"
col_hist <- "steelblue1"
col_hist <- "slateblue3"
col_hist <- "chartreuse3"
col_obs <- "#FF5500FF"

#            J    F    M    A    M    J   L   A   S    O    N    D   
bounds <- c(3.5, 4.2,  3 , 4.5,  4 , NA, NA, NA,  3 ,  3 , 2.8, 3.3)
nbin   <- c( 20, 20 , 20 , 20 , 20 , NA, NA, NA, 20 , 20 , 20 , 20)


########################################
# HISTOGRAMS OF STANDARDISED RESIDUALS
########################################

#for (month in c(1:5, 9:12)){
for (month in c(1,4,10)){
    
  # X is a list with cross-validated mean and variance predictions
  X <- Res_CV[[month]]
  # y contains the actual simulator outputs
  y <- Outputs_Gas[, month]
  
  err <- (X$Mean -y)/sqrt(X$Variance)
  th <- paste(month.names[month], "(Emul. Validation)")
  xlabel <- expression(plain("CV Standardised Errors ")*hat(epsilon)[i])
  
  file.name <- paste("Histograms/", month.name[month], "_CV_Errors_Hist.pdf", sep = "")
  pdf(file.name)
  par(cex =1.8, lwd=1.5)
  hist(err, 
       freq = T,
       main = th, 
       col = col_hist, 
       xlab =  xlabel, 
       #ylab = "",
       xlim = c(-bounds[month], bounds[month]),
       breaks = nbin[month])
  dev.off()
}


################################################################
# SCATTER PLOT OF STANDARDISED RESIDUALS AGAINST FITTED VALUES
################################################################

for (month in c(1:5, 9:12)){
  
  X <- Res_CV[[month]]
  y <- Outputs_Gas[, month]
  fitted <- X$Mean
  err <- (fitted-y)/sqrt(X$Variance)
  
  th <- paste(month.names[month])
  xlabel <- expression(plain("Fitted Values ")*hat(y)[i])
  ylabel <- expression(plain("CV Standardised Errors ")*hat(epsilon)[i])
  file.name <- paste("Scatter Plots/", month.name[month], "_CV_Scatter.pdf", sep = "")
  
  pdf(file.name)#, width = 7, height = 5)
  par(cex =1.6, lwd=1.5, mgp = c(2,0.5,0))
  plot(fitted, err, type = "n", 
       xlab = xlabel, 
       ylab = ylabel,       
       ylim = c(-bounds[month], bounds[month]))
  abline(h = 0, col = "darkolivegreen4", lty = 1, lwd = 1.5)
  abline(v = Obs_Gas[month], col = col_obs, lty = 2, lwd = 5.5)
  points(fitted, err, 
       pch = 21,
       bg = col_hist
  )
  title(th, line = 1)
  dev.off()
}












