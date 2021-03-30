##################################################################################################
# # #      CODE TO PLOT AND SAVE HISTOGRAMS OF PREDICTED SIMULATED SUMMER CONSUMPTIONS       # # #
##################################################################################################
# 
# setwd("Scripts/")
# 
# month.names <- c("January", "February", "March", "April", "May", "June", "July", 
#                  "August", "September", "October", "November", "December")
# 
# Obs_Gas <- matrix(c(3201, 2130, 1687, 1452, 1109, 628, 588, 520, 563, 1010, 1985, 2564), ncol = 12)  
# colnames(Obs_Gas) <- month.names                                  # name the column by months
# 
# ####################################################################################################
# Data-compatible indices and input parameters
load("../HM_Indices.RData")
N <- 5.e7
ind <- ind.MD0.C4
Test.data <- sobol(N, dim = 8, scrambling = 1, seed = 2341)
Test.data <- 2*Test.data[ind, , drop=F] -1
colnames(Test.data) <- colnames(Inputs)
gc()
Test.data <- Test.data[1:1000000, , drop=F]
gc()

####################################################################################################

n <- dim(Inputs)[1]
N <- dim(Test.data)[1]
# Build a matrix of all 2-way interactions of factors (orthogonal polynomials of order 2)
Interactions <- poly(as.matrix(Inputs), degree=2)
Preds <- matrix(nrow = N, ncol = 3)

for (month in c(6, 7, 8)) {
  y <- Outputs_Gas[, month]

  # Linear Regression
  L <- summary(regsubsets(y~., data=Interactions, method = "exhaustive", nvmax = 8))
  ind <- which.max(L$adjr2)          # index (between 1 and nvmax) of model with max adj-R2
  regr <- L$which[ind,-1]            # logical vector with regressors corresponding to selected model
  fit <- lm(y~ ., data = as.data.frame(Interactions[,regr]))

  # Prediction of LR on the data in Test.data
  Test.regr <- predict(Interactions, newdata = Test.data)
  Test.regr <- as.data.frame(Test.regr[, regr])
  Preds[, month-5] <- predict(fit, newdata = Test.regr)
}

#############################################################################################################
# HISTOGRAMS

# COLOUR OF HISTOGRAM BARS AND VERTICAL LINE (OBSERVED DATA)
col_hist1 <- "#00A600FF"
col_hist2 <- "red"
col_line <- "#FF5500FF"

# These lines store the coordinates of two corners of legend boxes, for the three months
pos.leg <- sapply(month.names[c(6,7,8)], function(x) NULL)
for (i in 1:3){
  pos.leg[[i]] <- sapply(c("XCoord", "YCoord"), function(y) c(0,0)) 
}
pos.leg$June[,1] <- c(485, 615)
pos.leg$June[,2] <- c(0.06, 0.038)
pos.leg$July[,1] <- c(465, 580)
pos.leg$July[,2] <- c(0.06, 0.037)
pos.leg$August[,1] <- c(365, 440)
pos.leg$August[,2] <- c(0.056, 0.034)

# NUMBER OF BINS FOR EACH PLOT (EACH MONTH)
#          J  F  M  A  M  J  J  A  S  O  N  D
nbin <- c(15,16,15,15,14, 8, 9,12,15,15,15,15)

# ACTUAL PLOTS
for (month in 6:8){
  x1 <- Preds[, month-5]
  x2 <- Outputs_Gas[, month]
  obs <- Obs_Gas[month]
  
  # FIND SUITABLE RANGE INCLUDING BOTH SIMULATION OUTPUTS AND OBSERVED DATA
  rg <- range(c(x1, x2, obs)) 
  fac <- 1.1
  # The following lines widen the interval rg of a factor fac, around its centre
  shift <- (fac-1)*( (rg[1]+rg[2])/2 )
  rg <- fac*rg - shift
  rm(fac, shift)
  
  # Title string of plot
  th <- paste(month.name[month], 'HM: 20% Mod.Discr, T=4')
  
  file.name <- paste(month.name[month], "_HM_20MD.pdf", sep = "")
  pdf(file.name)
  par(cex=1.6, lwd=1.5)
  
  # Plot the two histograms
  hist(x1, main=th, breaks= 2.5*nbin[month], xlim=rg, col="#00A600FF", xlab="Simulated Gas consumption [kWh]", freq = F)
  hist(x2, breaks = 3*nbin[month], col = col_hist2, add = T, freq = F)
  hist(x1, breaks=2.5*nbin[month], col="#00A60070", add = T, freq = F)
  
  # Add vertical line corresponding to observation
  abline(v=obs, col=col_line, lwd=3.5, lty=2)
  
  # Add legend
  legend(pos.leg[[month-5]][,1], pos.leg[[month-5]][,2], legend=c("Design Ensemble", "Data-Compatible", "Observation"), 
         col=c(col_hist2, col_hist1, col_line), pch=c(15,15,NA), pt.cex=3, y.intersp=2, x.intersp=0.75, lty=c(0,0,2), 
         inset=0.1, lwd=2.5, cex=0.7, bg='lightblue', box.lty = 0)
  dev.off()
}
  