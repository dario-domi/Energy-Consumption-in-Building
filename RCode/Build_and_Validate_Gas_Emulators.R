###############################################################################
#
# Builds linear regressions for the monthly gas consumptions by selecting the 
# "best" regressors among choices of quadratic terms of the 8 inputs
#
###############################################################################

# SET FOLDER AND LOAD SCRIPTS/DATA
setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')

library(leaps)
load('RData/Inputs/Design_Points.RData')               # Design Points
load('RData/Inputs/SplitSet.RData')                    # Training, Evaluation, Test sets
load('RData/Inputs/Simulated_and_Observed_Gas.RData')  # Gas data (observed and simulated)
source('../../Emulation.R')                            # Script to carry out Emulation


###############################################################################
# BUILD EMULATORS AND PREDICT VALUES ON VALIDATION AND TEST SETS

load('RData/Inputs/Regressors.RData')               # Regressors
source('Auxiliary_Scripts/Emulation_Parameters.R')  # Active Inputs and Correlation lengths for each month

Interactions.train <- poly(data.matrix(Design[train,]), degree=2) # orthogonal polys computed on 700 points only
Emul.Eval <- sapply(month.names, function(x) NULL)          # will contain emulated predictions on evaluation set
Emul.Test <- sapply(month.names, function(x) NULL)          # will contain emulated predictions on test set

month.vector <- c(1:5, 9:12)

source('Auxiliary_Scripts/Emulation_Parameters.R')

for (month in month.vector){
  y.train <- Gas.Sim[train, month]
  y.eval <- Gas.Sim[eval, month]
  y.test <- Gas.Sim[test, month]
  
  # Linear Regression
  regr <- Regressors[[month]]                       # logical vector with regressors to be used
  fit <- lm(y.train ~ ., data = as.data.frame(Interactions.train[, regr]))
  beta <- fit$coefficients
  
  # Active inputs and Correlation Lengths
  Active.Inputs <- Act_inputs[[month]]              # Active Inputs
  N_Act <- length(Active.Inputs)                         
  d <- Corr_lengths[month] * replicate(N_Act, 1)    # Correlation lengths
  
  # Training and Validation Variables
  ActInp.Train <- Design[train, Active.Inputs, drop=F]          # Design points used to train the emulator
  ActInp.Eval  <- Design[eval, Active.Inputs, drop=F]           # Evaluation points
  ActInp.Test  <- Design[test, Active.Inputs, drop=F]           # Test points
  Regr.Train   <- cbind(1, Interactions.train[, regr, drop=F])  # Regressors, training: add a column of 1s for intercept
  Regr.Eval    <- predict(Interactions.train, newdata = data.matrix(Design[eval,])) 
  Regr.Eval    <- cbind(1, Regr.Eval[, regr, drop=F])           # Regressors, evaluation set
  Regr.Test    <- predict(Interactions.train, newdata = data.matrix(Design[test,])) 
  Regr.Test    <- cbind(1, Regr.Test[, regr, drop=F])           # Regressors, evaluation set
  
  # Prior variances (explained and residual) used in the emulator
  sigma2.tot <- var(fit$residuals)                       # Prior cumulative variance of emulator+nugget
  nugget <- 0.05                                         # Fraction of residual variability not explained by regressors
  sig2 <- (1-nugget)*sigma2.tot                          # Variance of 2nd-order process
  nu2 <- nugget*sigma2.tot                               # Variance of nugget term (Gaussian noise)
  
  Emul.Eval[[month]] <- BL.Emul(ActInp.Train, ActInp.Eval, y.train, 
                                      Regress.Design = Regr.Train, 
                                      Regress.Test = Regr.Eval, 
                                      beta = beta,
                                      sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)
  
  Emul.Test[[month]] <- BL.Emul(ActInp.Train, ActInp.Test, y.train, 
                                Regress.Design = Regr.Train, 
                                Regress.Test = Regr.Test, 
                                beta = beta,
                                sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)
}
