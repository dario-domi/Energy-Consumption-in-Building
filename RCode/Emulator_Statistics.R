################################################################################
#
# This script explores different properties of the 9 emulators built for the 
# project, eg CIs of the emulated variance
#
################################################################################


source('../../Emulation.R')                            # Function to perform emulation

load('RData/Inputs/Design_Points.RData')               # Design Points
load('RData/Inputs/SplitSet.RData')                    # Training, Evaluation, Test sets
load('RData/Inputs/Simulated_and_Observed_Gas.RData')  # Gas data (observed and simulated)

load('RData/Inputs/Regressors.RData')                  # Regressors used in emulation
source('Auxiliary_Scripts/Emulation_Parameters.R')     # Active Inputs and Correlation lengths for each month

source('Auxiliary_Scripts/Auxiliary_Functions.R')      # Load function Create.my.list
rm(Cross_Val, Extract.Flat.Level, Rescale.Linearly)


###############################################################
###    LINEAR REGRESSION RESIDUAL VARIANCE   

# Build a matrix of all 2-way interactions (orthogonal polynomials of order 2) for training set
Interactions.train <- poly(data.matrix(Design[train,]), degree=2)

month.vector <- c(1:5, 9:12)

Res.Var <- array(0, 12)
R.sqr <- array(0, 12)

for (month in month.vector){
  y.train <- Gas.Sim[train, month]
  regr <- Regressors[[month]]
  fit <- lm(y.train ~ ., data = as.data.frame(Interactions.train[, regr]))
  Res.Var[month] <- var(fit$residuals)
  R.sqr[month] <- summary(fit)$adj.r.squared
}


#############################################################################
###  2.5 AND 97.5 PERCENTILES OF EMULATOR VARIANCES ON TEST POINTS

Test.Set <- Design[test, ]
source("Emulation_Small_TestSet.R")

for (month in month.vector){
  v <- Emul[[month]][,2]
  q1 <- quantile(v, 0.025)
  q2 <- quantile(v, 0.975)
  cat('Quantiles for', month.name[month], ':', round(q1,1), '-', round(q2,1), '\n')
}
