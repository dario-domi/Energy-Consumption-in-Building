# A computer model simulating monthly gas consumption in a private house has been run at N=1000 different
# choices of 8 model parameters. These are, for example, boiler efficiency, floor thickness, wall insulation, etc.
# Observations of actual monthly consumption for the house are also available. 

# This script explores the dataset of simulated gas consumption y, and identifies the main factors (and interactions
# thereof) which explain the variability in y. An emulator of the simulated consumption is then built: 
# the emulator predicts the simulator response at any input and quantifies uncertainty around the prediction.

#####################################################################################################################


setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/')

# LOAD LIBRARIES AND CUSTOM FUNCTIONS
library(openxlsx)    
library(leaps)
library(randtoolbox)
source("Auxiliary_Functions.R")
source("../../Emulation.R")


########################################################################
##  LOAD THE DATA: SIMULATION INPUTS, SIMULATION OUTPUTS, OBSERVATIONS  
########################################################################

month.names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

source('Auxiliary_Scripts/Load_data.R')

## RESCALE INPUTS LINEARLY WITHIN [-1,1]  
Design <- Rescale.Linearly(Design) 


###################################################
##   ANALYSE THE SIMULATED INPUT-OUTPUT RELATION
###################################################

n <- dim(Design)[1]
# Build a matrix of all 2-way interactions of factors (orthogonal polynomials of order 2)
Interactions <- poly(as.matrix(Design), degree=2)

month<-1
y <- Outputs_gas[, month]

## Either of next two commands carries out lm's with different number of covariates, selecting best model of each size (for fixed size, best model is the same regardless of criterion used - adjr2, Cp etc)
#L <- leaps(Interactions, y, method = "adjr2", strictly.compatible = F, nbest = 1)
L <- summary(regsubsets(y~., data=Interactions, method = "exhaustive", nvmax = 10))
ind <- which.max(L$adjr2)          # index (between 1 and nvmax) of model with max adj-R2
regr <- L$which[ind,-1]            # logical vector with regressors corresponding to selected model
#names(regr[regr==T])              # just prints selected variables
fit <- lm(y~ ., data = as.data.frame(Interactions[,regr]))
summary(fit)


##################################################################
# This part is to generate synthetic data ... 
N <- 400
Test.data <- matrix(runif(n = 8*N, min=-1, max=1), nrow = N)
colnames(Test.data) <- colnames(Design)

Test.regr <- predict(Interactions, newdata = Test.data)
Test.regr <- as.data.frame(Test.regr[, regr])

# ... and predict their outputs via linear regression
Preds <- predict(fit, newdata = Test.regr)
hist(Preds)
y[ind]

#####################
# This takes subsets of original big set of test data
load("../RData/HM_Indices.RData")
N <- 5.e7
ind <- ind.MD01.C4
Test.data <- sobol(N, dim = 8, scrambling = 1, seed = 2341)
Test.data <- 2*Test.data[ind, , drop=F] -1
gc()
colnames(Test.data) <- colnames(Design)


###################################################################


#################################################################################
###   VALIDATE AN EMULATOR BUILT ON REGRESSORS AND INTERACTIONS SELECTED ABOVE
#################################################################################

# Mean and Variance of coefficients used in prior mean
beta <- fit$coefficients
Cov.beta <- vcov(fit)

# Variables used in the Gaussian process part
GP_inputs <- c(1,2,3,4,6)
N_var <- length(GP_inputs)

# Build corresponding design points and regressors
Design.points <- Inputs_2[, GP_inputs]                 # design points used to train the emulator
Design.regr <- cbind(1, Interactions[,regr, drop=F])   # regressors: add a column of 1s for intercept

# Values of parameters used in the GP part
d <- 0.35* replicate(N_var, 1)                         # Correlation lengths
sigma2.tot <- var(fit$residuals)                       # Prior cumulative variance of homoschedastic Gaussian process
nugget <- 0.05                                         # Fraction of residual variability not explained by regressors
nu <- nugget*sigma2.tot                                # Variance of nugget term (Gaussian noise)
sig2 <- (1-nugget)*sigma2.tot                          # Variance of Gaussian process

# Leave-one-out Cross validation
CV_dec_d035_10cov_5GP <- Cross_Val(Design.points, Design.regr, y, beta, Cov.beta, d, nu, sig2, 'exp2')

# Plots
hist( (X$Mean -y) , breaks = 40, freq = F)#, xlim = c(-20,20))
hist( sqrt(X$Variance), breaks=30, freq = F)
err <- (X$Mean -y)/sqrt(X$Variance)
hist( err , breaks = 40)

plot(y,err)
for (i in 1:8){
  plot(Inputs_2[,i], err, ylim = c(-4,4))
  Sys.sleep(2)
}


#######################################
# Cross validation on a random sample
#######################################

# Select 950 integer numbers between 1 and n=1000
ind <- sample(n, 900)

Design.points <- Inputs_2[ind, GP_inputs]
Design.regr <- cbind(1, Interactions[ind, regr, drop=F])   # regressors: add a column of 1s for intercept

Test.points <- Inputs_2[-ind, GP_inputs]
Test.regr   <- cbind(1, Interactions[-ind, regr, drop=F] )   # add a column of 1 for intercept

y_train <- y[ind]
Res_CV <- emul(Test.points, Test.regr, Design.points, Design.regr, y_train, beta, Cov.beta, 1*d, nu, sig2, 'exp2')

err <- (Res_CV$Mean - y[-ind])/sqrt(Res_CV$Variance)
hist(err)


############################################################################
# How to build covariates actually used in 'fit' for a new piece of data
###########################################################################

form <- formula(fit)[c(1,3)]
test.regr <- model.matrix(form, data=x)
Design.regr <- model.matrix(form, data=Inputs)

d <- rep(0.7, N_var)
nu <- 0
sigma2 <- sigma(fit)^2
