##############################################################################
#
# Analyse and produce plots of MBE and CV(RMSE) of gas and temperature data
#
##############################################################################

# SET FOLDER AND READ DATA
setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')
library(xlsx)
library(plot3D)
library(oceanmap)

Gas.MBE <- read.xlsx(file = '../Data/MBE_and_RMSE/Gas.xlsx', sheetIndex = 1, colIndex = 2:13)
KitchTemp.MBE <- read.xlsx(file = '../Data/MBE_and_RMSE/Kitchen.xlsx', 
                           sheetName = 'MBE Kitchen (%)',  colIndex = 2:14)
KitchTemp.RMSE <- read.xlsx(file = '../Data/MBE_and_RMSE/Kitchen.xlsx', 
                           sheetName = 'CV(RMSE) Kitchen (%)',  colIndex = 2:14)
MastTemp.MBE <- read.xlsx(file = '../Data/MBE_and_RMSE/Master.xlsx', 
                           sheetName = 'MBE Master (%)',  colIndex = 2:14)
MastTemp.RMSE <- read.xlsx(file = '../Data/MBE_and_RMSE/Master.xlsx', 
                            sheetName = 'CV(RMSE) Master (%)',  colIndex = 2:14)
# For Design Points
load('RData/Inputs/Simulated_and_Observed_Gas.RData')
rm(Gas.Sim)

#######################################################

Gas.ind <- abs(Gas.MBE)<5

for (m in 1:12){
  p <- sum(Gas.ind[,m])
  cat('Pencentage of plausible simulations for ', names(Gas.MBE)[m], ': ', p/10, '%\n', sep='')
}

ind <- apply(Gas.ind[, 2:3], 1, prod)
sum(ind)

# Scatter Plots for different months
month <- 'Jan'
ind <- abs(Gas.MBE[,month])<5
#ind <- 1:1000
c1 <- 1
c2 <- 6
X <- Design.Original

###############################################################

# Plot of MBE
for (m in 1:12){
  month <- names(Gas.MBE)[m]
  file.name <- paste('../Pictures/MBE/MBE_Gas_', formatC(m, width=2, flag="0"), 
                     '.pdf', sep='')
  cmax <- max(abs(Gas.MBE[,m]))
  pdf(file.name, width = 8, height = 8)
  par(mgp=c(3.5,1.5,0))
  scatter2D(X[,c1], X[,c2], colvar = Gas.MBE[,month],
          pch=20, cex=3,  xlab = names(Design)[c1], ylab = names(Design)[c2],
          clim = c(-cmax, cmax), 
          cex.main = 2, cex.axis = 2, cex.lab = 2,
          main = paste('MBE Gas: ', month, sep = ''))
  
  # Add contour to bullet points
  scatter2D(X[,c1], X[,c2], col = rgb(0.1, 0.1, 0.1), pch=21, cex=2.4, 
            cex.main = 2, cex.axis = 2, cex.lab = 2,
            xlab = names(Design)[c1], ylab = names(Design)[c2],
            lwd = 0.7, add = T)
  dev.off()
}

########################################################################
ind <- abs(Gas.MBE[,month])<5

# Plots of selected by abs(MBE)<5
for (m in 1:12){
  month <- names(Gas.MBE)[m]
  ind <- abs(Gas.MBE[,month])<5
  file.name <- paste('../Pictures/MBE/SelectedMBE_Gas_', formatC(m, width=2, flag="0"), 
                     '.pdf', sep='')
  pdf(file.name, width = 8, height = 8)
  par(mgp=c(3.5,1.5,0))
  scatter2D(X[,c1], X[,c2], pch=20, cex=2.5, col=rgb(0.5, 0.5, 0.5),
          xlab = names(Design)[c1], ylab = names(Design)[c2],
          cex.main = 2, cex.axis = 2, cex.lab = 2,
          main = paste('-5% < MBE < 5%,  Gas ', month, sep = ''))
  scatter2D(X[ind,c1], X[ind,c2], pch=21, cex=2.4, col='black', bg = 'red', 
            cex.main = 2, cex.axis = 2, cex.lab = 2,
            lwd = 0.7, add=T)
  dev.off()
}




####################################################################
# Using logarithmic scale
colticks <- c(0.05, 0.1, 0.2, 0.5, 1,2,5,10,20,50)
scatter2D(X[,c1], X[,c2], colvar = log(abs(Gas.MBE[,month])), 
          pch=20, cex=1,  colkey = list(width=0.8, 
                                        at=log(colticks),
                                        labels = as.character(colticks)),
          xlab = names(Design)[c1], ylab = names(Design)[c2],
          clim = log(c(0.02, 60)),
          main = paste('ds', 'wq', sep = ''))