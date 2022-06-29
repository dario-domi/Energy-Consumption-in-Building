###############################################################################
#
# This script loads the implausibility measures of max daily temperature on
# summer months, and emulates them at a large sequence of inputs, loaded from
# 'RData/Results/Eval.Inputs.RData'.
#
# Predictions are stored in 'RData/Results/Implaus_SummerMax_Emulators.RData',
# and used in 'Main_Temperature_Analysis.R' to carry out history matching.
#
###############################################################################


#####################################################################
## SET FOLDER AND LOAD LIBRARIES

setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode/RData/')
library(openxlsx)
library(leaps)
library(fields)
source('../../../Emulation.R')


#####################################################################
## LOAD VARIABLES AND DATASETS FROM PREVIOUSLY STORED .RData FILES

# Design points and set split
load('Inputs/Design_Points.RData')
load('Inputs/SplitSet.RData')

# Implausibility Measures for daily max in Summer months
load('Inputs/Implausibilities.RData')

# Optimised emulation parameters + Train-cross-test subdivision
load('Results/Implaus_SummerMax_OptPars.RData')


#######################################################################
# LARGE SEQUENCE OF INPUTS AT WHICH EMULATION WILL BE CARRIED OUT

load('RData/Results_Emulation/Eval_Inputs.RData') # loads Eval.points.full
Eval.points <- Eval.points.full[1:1.e5, ]
rm(Eval.points.full)  # clean workspace
invisible(gc())       # release memory


#######################################################################
## EMULATE IMPLAUSIBILITIES AT THE ABOVE INPUTS

# The code below is repeated for each month and room


######################
###  JUNE, MASTER  ###
######################

# Linear regression
subset.vars <- c(1,6)  # subset of variables whose powers will be used to select regressors
Interactions <- poly(as.matrix(Design[,subset.vars]), degree=3)
y.train <- Impl.Mast.Jun[train]
L <- summary(regsubsets(y.train~., data=as.data.frame(Interactions[train,]), 
                        method = "forward", nvmax = 5))
which(regr <- L$which[5,-1])            # logical vector with regressors corresponding to selected model
fit <- lm(y.train~ ., data = as.data.frame(Interactions[train, regr ]))

# Regression coefficients mean and covariance
beta <- fit$coefficients
Cov.beta <- vcov(fit)

# Evaluation points and active inputs
Active.Inputs <- c(1,3,6)
Train.ActInp <- Design[train, Active.Inputs, drop=F]         # design points used to train the emulator
Train.regr <- cbind(1, Interactions[train, regr, drop=F])    # regressors: add a column of 1s for intercept
Eval.ActInp <- Eval.points[, Active.Inputs]
All.regr <- predict(Interactions, newdata = Eval.points[, subset.vars])
Eval.regr  <- cbind(1, All.regr[, regr, drop=F])

sig2 <- sig2.Mast.Jun
nu2 <- nu2.Mast.Jun
d <- d.Mast.Jun

Emul.Mast.Jun <- BL.Emul(Train.ActInp, Eval.ActInp, y.train, 
                         Regress.Design = Train.regr, 
                         Regress.Test = Eval.regr, 
                         beta = beta, Cov.beta = Cov.beta, 
                         sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)


##########################################

#######################
###  JUNE, KITCHEN  ###
#######################

# Linear regression
subset.vars <- c(1,6,8)
Interactions <- poly(as.matrix(Design[,subset.vars]), degree=3)
y.train <- Impl.Kitch.Jun[train]
L <- summary(regsubsets(y.train~., data=as.data.frame(Interactions[train,]), 
                        method = "forward", nvmax = 5))
which(regr <- L$which[5,-1])            # logical vector with regressors corresponding to selected model
fit <- lm(y.train~ ., data = as.data.frame(Interactions[train, regr ]))

# Regression coefficients mean and covariance
beta <- fit$coefficients
Cov.beta <- vcov(fit)

# Evaluation points and active inputs
Active.Inputs <- c(1,6,8)
Train.ActInp <- Design[train, Active.Inputs, drop=F]         # design points used to train the emulator
Train.regr <- cbind(1, Interactions[train, regr, drop=F])    # regressors: add a column of 1s for intercept
Eval.ActInp <- Eval.points[, Active.Inputs]
All.regr <- predict(Interactions, newdata = Eval.points[, subset.vars])
Eval.regr  <- cbind(1, All.regr[, regr, drop=F])

sig2 <- sig2.Kitch.Jun
nu2 <- nu2.Kitch.Jun
d <- d.Kitch.Jun

Emul.Kitch.Jun <- BL.Emul(Train.ActInp, Eval.ActInp, y.train, 
                          Regress.Design = Train.regr, 
                          Regress.Test = Eval.regr, 
                          beta = beta, Cov.beta = Cov.beta, 
                          sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)


##########################################

######################
###  JULY, MASTER  ###
######################

# Linear regression
subset.vars <- c(3,6)
Interactions <- poly(as.matrix(Design[, subset.vars]), degree=4)
y.train <- Impl.Mast.Jul[train]
L <- summary(regsubsets(y.train~., data=as.data.frame(Interactions[train,]), 
                        method = "forward", nvmax = 5))
which(regr <- L$which[5,-1])            # logical vector with regressors corresponding to selected model
fit <- lm(y.train~ ., data = as.data.frame(Interactions[train, regr ]))

# Regression coefficients mean and covariance
beta <- fit$coefficients
Cov.beta <- vcov(fit)

# Evaluation points and active inputs
Active.Inputs <- c(1,3,4,6)
Train.ActInp <- Design[train, Active.Inputs, drop=F]         # design points used to train the emulator
Train.regr <- cbind(1, Interactions[train, regr, drop=F])    # regressors: add a column of 1s for intercept
Eval.ActInp <- Eval.points[, Active.Inputs]
All.regr <- predict(Interactions, newdata = Eval.points[, subset.vars])
Eval.regr  <- cbind(1, All.regr[, regr, drop=F])

sig2 <- sig2.Mast.Jul
nu2 <- nu2.Mast.Jul
d <- d.Mast.Jul

Emul.Mast.Jul <- BL.Emul(Train.ActInp, Eval.ActInp, y.train, 
                         Regress.Design = Train.regr, 
                         Regress.Test = Eval.regr, 
                         beta = beta, Cov.beta = Cov.beta, 
                         sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)


##########################################

#######################
###  JULY, KITCHEN  ###
#######################

# Linear regression
subset.vars <- c(6,8)
Interactions <- poly(as.matrix(Design[,subset.vars]), degree=3)
y.train <- Impl.Kitch.Jul[train]
L <- summary(regsubsets(y.train~., data=as.data.frame(Interactions[train,]), method = "forward", nvmax = 5))
which(regr <- L$which[5,-1])            # logical vector with regressors corresponding to selected model
fit <- lm(y.train~ ., data = as.data.frame(Interactions[train, regr ]))

# Regression coefficients mean and covariance
beta <- fit$coefficients
Cov.beta <- vcov(fit)

# Evaluation points and active inputs
Active.Inputs <- c(1,6,8)
Train.ActInp <- Design[train, Active.Inputs, drop=F]         # design points used to train the emulator
Train.regr <- cbind(1, Interactions[train, regr, drop=F])    # regressors: add a column of 1s for intercept
Eval.ActInp <- Eval.points[, Active.Inputs]
All.regr <- predict(Interactions, newdata = Eval.points[, subset.vars])
Eval.regr  <- cbind(1, All.regr[, regr, drop=F])

sig2 <- sig2.Kitch.Jul
nu2 <- nu2.Kitch.Jul
d <- d.Kitch.Jul

Emul.Kitch.Jul <- BL.Emul(Train.ActInp, Eval.ActInp, y.train, 
                          Regress.Design = Train.regr, 
                          Regress.Test = Eval.regr, 
                          beta = beta, Cov.beta = Cov.beta, 
                          sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)


###############################################################################

########################
###  AUGUST, MASTER  ###
########################

# Linear regression
subset.vars <- c(1,6)
Interactions <- poly(as.matrix(Design[, subset.vars]), degree=3)
y.train <- Impl.Mast.Aug[train]
L <- summary(regsubsets(y.train~., data=as.data.frame(Interactions[train,]), method = "forward", nvmax = 5))
which(regr <- L$which[5,-1])            # logical vector with regressors corresponding to selected model
fit <- lm(y.train~ ., data = as.data.frame(Interactions[train, regr ]))

# Regression coefficients mean and covariance
beta <- fit$coefficients
Cov.beta <- vcov(fit)

# Evaluation points and active inputs
Active.Inputs <- c(1,3,6)
Train.ActInp <- Design[train, Active.Inputs, drop=F]         # design points used to train the emulator
Train.regr <- cbind(1, Interactions[train, regr, drop=F])    # regressors: add a column of 1s for intercept
Eval.ActInp <- Eval.points[, Active.Inputs]
All.regr <- predict(Interactions, newdata = Eval.points[, subset.vars])
Eval.regr  <- cbind(1, All.regr[, regr, drop=F])

sig2 <- sig2.Mast.Aug
nu2 <- nu2.Mast.Aug
d <- d.Mast.Aug

Emul.Mast.Aug <- BL.Emul(Train.ActInp, Eval.ActInp, y.train, 
                           Regress.Design = Train.regr, 
                           Regress.Test = Eval.regr, 
                           beta = beta, Cov.beta = Cov.beta, 
                           sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)


###############################################################################

#########################
###  AUGUST, KITCHEN  ###
#########################

# Linear regression
subset.vars <- c(6,8)
Interactions <- poly(as.matrix(Design[,subset.vars]), degree=3)
y.train <- Impl.Kitch.Aug[train]
L <- summary(regsubsets(y.train~., data=as.data.frame(Interactions[train,]), method = "forward", nvmax = 5))
which(regr <- L$which[5,-1])            # logical vector with regressors corresponding to selected model
fit <- lm(y.train~ ., data = as.data.frame(Interactions[train, regr ]))

# Regression coefficients mean and covariance
beta <- fit$coefficients
Cov.beta <- vcov(fit)

# Evaluation points and active inputs
Active.Inputs <- c(1,6,8)
Train.ActInp <- Design[train, Active.Inputs, drop=F]         # design points used to train the emulator
Train.regr <- cbind(1, Interactions[train, regr, drop=F])    # regressors: add a column of 1s for intercept
Eval.ActInp <- Eval.points[, Active.Inputs]
All.regr <- predict(Interactions, newdata = Eval.points[, subset.vars])
Eval.regr  <- cbind(1, All.regr[, regr, drop=F])

sig2 <- sig2.Kitch.Aug
nu2 <- nu2.Kitch.Aug
d <- d.Kitch.Aug

Emul.Kitch.Aug <- BL.Emul(Train.ActInp, Eval.ActInp, y.train, 
                          Regress.Design = Train.regr, 
                          Regress.Test = Eval.regr, 
                          beta = beta, Cov.beta = Cov.beta, 
                          sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)


###############################################################################

save(Emul.Mast.Jun, Emul.Kitch.Jun,
     Emul.Mast.Jul, Emul.Kitch.Jul,
     Emul.Mast.Aug, Emul.Kitch.Aug,
     file = 'RData/Results/Implaus_SummerMax_Emulators.RData')



