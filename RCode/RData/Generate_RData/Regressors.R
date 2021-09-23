################################################################################
#
# Selects "best" regressors (higher adjusted R^2) for linear regressions of monthly
# gas consumption. Regressors are chosen among all (mutually orthogonal) linear,
# quadratic and interaction terms of the eight variables in the matrix Design.
# Selected regressors are stored in RData/Inputs/Regressors.RData
#
################################################################################

# SET FOLDER AND LOAD DATA
setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')
library(leaps)                                          # Used to select best linear model
load('RData/Inputs/Design_Points.RData')                # Design points, 1000x8
load('RData/Inputs/SplitSet.RData')                     # Train, Evaluation and Test sets
load('RData/Inputs/Simulated_and_Observed_Gas.RData')   # Gas consumptions


# SELECT REGRESSORS

# Matrix of all 2-way interactions of input variables (orthogonal polynomials of order 2)
Interactions.train <- poly(data.matrix(Design[train,]), degree=2)
Regressors <- sapply(month.names, function(x) NULL)

for (month in c(1:5, 9:12)){
  y.train <- Gas.Sim[train, month]

  # Carry out lm's with different number of covariates, selecting best model of each size
  # (for fixed size, best model is the same regardless of criterion used - adjr2, Cp etc)
  L <- summary(regsubsets(y.train~., data=Interactions.train, method = "exhaustive", nvmax = 10))
  
  ind <- which.max(L$adjr2)                 # index (between 1 and nvmax) of model with max adj-R2
  Regressors[[month]] <- L$which[ind,-1]    # logical vector with regressors corresponding to selected model
  
}

save(Regressors, file = 'RData/Inputs/Regressors.RData')



