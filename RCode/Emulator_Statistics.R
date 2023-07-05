################################################################################
#
# The information below is summarised in Table 3 and Table 4 of the manuscript.
#
# This script lists different properties of the 9 emulators (and underlying
# linear regressions) built for the project: eg
# - Regressors, adjusted R^2 of regression
# - CIs of the emulated variance on test set
#
################################################################################


###############################################################
###   LOAD NECESSARY FUNCTIONS AND DATA

source('../../Emulation.R')   # Function to perform emulation

load('RData/Results_Simulator/Design_Points.RData')               # Design Points
load('RData/Results_Simulator/Simulated_and_Observed_Gas.RData')  # Gas data (observed and simulated)

load('RData/Results_Emulator/SplitSet.RData')              # Training, Evaluation, Test sets
load('RData/Results_Emulator/Regressors.RData')            # Regressors used in emulation
load('RData/Results_Emulator/Emulator_Parameters.RData')   # Active Inputs and Correlation lengths for each month


######################################
#                                    #
#           T A B L E  3             #
#                                    #
######################################


##############################################################
#########       LIST EMULATOR SPECIFICATIONS      

month.names <- c("Jan", "Feb", "Mar", "Apr", "May", "Sep", "Oct", "Nov", "Dec")

# REGRESSORS FOR EACH MONTH
for (m in month.names){
  cat("Regressors for ", m, ":\n", sep = "")
  cat(names(Regressors[[m]][Regressors[[m]]]), sep="\n")
  cat("\n")
}


# ACTIVE PARAMETERS
for (m in month.names){
  s <- paste("V", Act_params[[m]], sep = "", collapse = " ")
  cat("Active parameters for ", m, ": ", s, "\n", sep = "")
}


# CORRELATION LENGTHS
for (m in month.names){
  s <- paste("V", Corr_lengths[m], sep = "", collapse = " ")
  cat("Correlation length for ", m, ": ", Corr_lengths[m], "\n", sep = "")
}


##############################################################
# LINEAR REGRESSION RESIDUAL VARIANCE AND R^2   

# Build a matrix of all 2-way interactions (orthogonal polynomials of order 2) for training set
Interactions_train <- poly(data.matrix(Design[train,]), degree=2)

Res_Var <- sapply(month.names, function(x) 0)
R_sqr <- sapply(month.names, function(x) 0)

# Store all residual variances and R^2
for (month in month.names){
  y.train <- Gas.Sim[train, month]
  regr <- Regressors[[month]]
  fit <- lm(y.train ~ ., data = as.data.frame(Interactions_train[, regr]))
  Res_Var[month] <- var(fit$residuals)
  R_sqr[month] <- summary(fit)$adj.r.squared
}


# LIST RESIDUAL VARIANCES
for (m in month.names){
  cat("Linear regression residual variance for ", m, ": ", 
      round(Res_Var[m],2), "\n", sep = "")
}


# LIST R^2
for (m in month.names){
  cat("Adjusted R2 for ", m, ": ", 
      round(R_sqr[m],4), "\n", sep = "")
}



######################################
#                                    #
#           T A B L E  4             #
#                                    #
######################################



#############################################################################
###  2.5 AND 97.5 PERCENTILES OF EMULATOR VARIANCES ON TEST POINTS

Test.Set <- Design[test, ]
source("Emulation_Small_TestSet.R")

for (month in month.names){
  v <- Emul[[month]][,2]
  q1 <- quantile(v, 0.025)
  q2 <- quantile(v, 0.975)
  cat('Quantiles for', month.name[month], ':', round(q1,1), '-', round(q2,1), '\n')
}
