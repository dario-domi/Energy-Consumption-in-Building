################################################################################
# This script works as Emulation_TestSet.R (ie, evaluates gas emulators at a 
# given set of inputs), but is meant to be used for relatively small number of 
# inputs, say up to a few (tens of) thousands.
#
# Before running this script, the following variables must be globally defined:
# - Test.Set:   a Nx8 matrix/data.frame, with coordinates at which to evaluate 
#               the emulators.
# - train:      a vector subset of 1:1000, with indices used for emul training.
# - Regressors: a 12-long list. Each element is a logical vector (of length 44)
#               with regressors to be used in regression.
#               Can be loaded via load("RData/Inputs/Regressors.RData").
#
# The created variable is called Emul (a list of 12 matrices)
################################################################################

#########################################
# LOAD DATA AND CUSTOMISED FUNCTIONS
#########################################

source('../../Emulation.R')                            # Function to perform emulation

load('RData/Inputs/Design_Points.RData')               # Design Points
load('RData/Inputs/Simulated_and_Observed_Gas.RData')  # Gas data (observed and simulated)
source('Auxiliary_Scripts/Emulation_Parameters.R')     # Active Inputs and Correlation lengths for each month


#################################################
# PERFORM EMULATION ON NEW SET OF TEST POINTS 
#################################################

# Build a matrix of all 2-way interactions (orthogonal polynomials of order 2) for training set
Interactions.train <- poly(data.matrix(Design[train,]), degree=2)

# Full set of regressors associated with test point
All.Regr.Test <- predict(Interactions.train, newdata = data.matrix(Test.Set))  # dim(Test.Set)[1] x 44

# List which will contain emulator predictions
Emul <- sapply(month.names, function(x) NULL)

for (month in c(1:5, 9:12) ){
  
  cat(sprintf("\t %s being computed.\n", month.names[month]))
  
  # Linear Regression
  y.train <- Gas.Sim[train, month]
  regr <- Regressors[[month]]                 # logical vector with regressors to be used in lm
  fit <- lm(y.train ~ ., data = as.data.frame(Interactions.train[, regr]))
  beta <- fit$coefficients
  
  # Active inputs and Correlation Lengths
  Active.Inputs <- Act_inputs[[month]]              # Index of Active Inputs
  N_Act <- length(Active.Inputs)                         
  
  # Compute Active inputs and Regressors for Train and Test Sets
  ActInp.Train <- Design[train, Active.Inputs, drop=F]          # Design points used to train the emulator
  ActInp.Test  <- Test.Set[,    Active.Inputs, drop=F]          # Points at which to evaluate the emulator
  Regr.Train   <- cbind(1, Interactions.train[, regr, drop=F])  # Regressors for training sett
  Regr.Test    <- cbind(1,      All.Regr.Test[, regr, drop=F])
  
  # Prior correlation lengths and variances (explained and residual) used in the emulator
  d <- 0.5 * Corr_lengths[month] * replicate(N_Act, 1)      # Correlation lengths
  sigma2.tot <- var(fit$residuals)                    # Prior cumulative variance of stochastic process
  nugget <- 0.05                                      # Fraction of residual variability
  nu2 <- nugget*sigma2.tot                            # Variance of nugget term (Gaussian noise)
  sig2 <- (1-nugget)*sigma2.tot                       # Variance of stochastic process
  
  # Call the function performing Bayes Linear Emulation, on the selected regressors given observed output y
  Emul[[month]] <- BL.Emul(ActInp.Train, ActInp.Test, y.train, 
                     Regress.Design = Regr.Train, 
                     Regress.Test = Regr.Test, 
                     beta = beta,
                     sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)
  
  invisible(gc())
}



