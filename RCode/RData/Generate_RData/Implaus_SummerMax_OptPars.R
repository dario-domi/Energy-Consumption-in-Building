################################################################################
#
# Choose optimal hyperparameters of emulators of summer implausibilities. 
# The function 'Find.Optimal.Parameters' in RCode/Auxiliary_Scripts is used to 
# optimise the hyperparameters (emulator prior variance, correlation lengths, 
# nugget), by maximising joint density of prediction on a validation set.
# The optimised hyperparameters are stored in 
# 'RData/Results/Implaus_SummerMax_OptPars.RData'.
#
# The second part of the script validates the emulators, plots some statistics,
# and compares with the results of linear regression.
#
################################################################################


######################################################################
## FIRST PART: EMULATE SUMMER IMPLAUSIBILITIES OF DAILY MAXIMA
######################################################################

# Example provided for a month. Details of which variables should be used for 
# other months are provided in comments. Also compare with details in script
# 'Implaus_SummerMax_Emulators.R'.


library(leaps)

## Load Implausibility data
setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')
source('../../Emulation.R')
source('Auxiliary_Scripts/Find_Optimal_Params.R')
load('RData/Inputs/Design_Points.RData')
load('RData/Inputs/SplitSet.RData')
load('RData/Inputs/Implausibilities.RData')

Impl <- Impl.Mast.Jul
y.train <- Impl[train]
y.valid <- Impl[valid]
y.test <- Impl[test]


## USE LINEAR REGRESSION TO SELECT REGRESSORS

# Select regressors and active variables to use, by choosing "best" ones
# (maximising adj R^2, or AIC/BIC) among all forth powers and interactions
Interactions <- poly(as.matrix(Design[,]), degree=4)
L <- summary(regsubsets(y.train~., data=as.data.frame(Interactions[train,]), 
                        method = "forward", nvmax = 15))
which(regr <- L$which[5,-1])            # regr: logical vector with selected regressors
fit <- lm(y.train~ ., data = as.data.frame(Interactions[train, regr ]))
summary(fit)


## DEFINE ACTIVE INPUTS AND REGRESSORS FOR THE 3 SUBSETS OF SIMULATIONS

# The following are the active inputs to choose for each month and room:
# June, Master: c(1,3,6)
# June, Kitchen: c(1,6,8)
# July, Master: c(1,3,4,6)
# July, Kitchen: c(1,6,8)
# Aug, Master: c(1,3,6)
# Aug, Kitch: c(1,6,8)

Active.Inputs <- c(1,3,4,6)
Train.ActInp <- Design[train, Active.Inputs, drop=F]       # design points used to train the emulator
Train.regr <- cbind(1, Interactions[train, regr, drop=F])  # regressors: add a column of 1s for intercept
Valid.ActInp <- Design[valid, Active.Inputs, drop=F]
Valid.regr <- cbind(1, Interactions[valid, regr, drop=F])   
Test.ActInp  <- Design[test, Active.Inputs, drop=F]
Test.regr  <- cbind(1, Interactions[test, regr, drop=F])

# MEAN AND COVARIANCE OF REGRESSION COEFFICIENTS
beta <- fit$coefficients
Cov.beta <- vcov(fit)

## FIND THE OPTIMAL PARAMETERS
N_var_GP <- length(Active.Inputs)
# Initial values of d, s2 and nugget for optimisation
d0 <- 0.4*replicate(N_var_GP, 1)    # Correlation lengths
s2.tot0 <- var(fit$residuals)       # Prior cumulative variance of homoschedastic process
nug_frac0 <- 0.05                   # Fraction of variability not explained by regressors
kernel <- 'exp2'

Opt.pars <- Find.Optimal.Parameters(Train.ActInp, Train.regr,
                                    Valid.ActInp, Valid.regr,
                                    y.train, y.valid, 
                                    kernel, d0, s2.tot0, nug_frac0)

sig2.Mast.Jul <- Opt.pars$s2
nu2.Mast.Jul  <- Opt.pars$nu2
d.Mast.Jul    <- Opt.pars$d
val.Mast.Jul  <- Opt.pars$obj


##########################################################################

## SAVE OPTIMISED PARAMETERS AND TRAINING-VALIDATION-TEST SUBDIVISION
save(Design, Design.Original, train, valid, test,
     sig2.Mast.Jul, nu2.Mast.Jul, d.Mast.ul, val.Mast.Jul,
     sig2.Kitch.Jul, nu2.Kitch.Jul, d.Kitch.Jul, val.Kitch.Jul,
     sig2.Mast.Jun, nu2.Mast.Jun, d.Mast.Jun, val.Mast.Jun,
     sig2.Kitch.Jun, nu2.Kitch.Jun, d.Kitch.Jun, val.Kitch.Jun,
     sig2.Mast.Aug, nu2.Mast.Aug, d.Mast.Aug, val.Mast.Aug,
     sig2.Kitch.Aug, nu2.Kitch.Aug, d.Kitch.Aug, val.Kitch.Aug,
     file = "RData/Results/Implaus_SummerMax_OptPars.RData")


###########################################################################



###########################################################################
## SECOND PART: COMPARE PERFORMANCE ON VALIDATION AND TEST SET
###########################################################################

load('RData/Opt_Params_MaxSummerMonths.RData')

sig2 <- sig2.Mast.Jul
nu2 <- nu2.Mast.Jul
d <- d.Mast.Jul

res.test <- BL.Emul(Train.ActInp, Test.ActInp, y.train, 
                    Regress.Design = Train.regr, 
                    Regress.Test = Test.regr, 
                    beta = beta, Cov.beta = Cov.beta, 
                    sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)

res.valid <- BL.Emul(Train.ActInp, Valid.ActInp, y.train, 
                     Regress.Design = Train.regr, 
                     Regress.Test = Valid.regr, 
                     beta = beta, Cov.beta = Cov.beta, 
                     sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)

# Value of objective function on validation and test sets
sum(dnorm(y.valid, mean = res.valid[,1], sd = sqrt(res.valid[,2]), log = T))/length(y.valid)
sum(dnorm(y.test,  mean = res.test[,1],  sd = sqrt(res.test[,2]),  log = T))/length(y.test)

# Histogram of (strandardised) errors and standard deviations
hist( (y.test-res.test[,1])/sqrt(res.test[,2]), breaks = 30)
hist(sqrt(res.test[,2]), breaks = 30)
hist( y.test-res.test[,1]  , breaks = 30)

##############################################################################

############################################################
## COMPARE LINEAR REGRESSION AND EMULATOR PREDICTIONS
############################################################

library(psych)

LRpreds <- predict(fit, as.data.frame(Interactions[test, regr]))
n.std <- 3

# Matrix containing emulator mean predictions and +/- 3 std from it
# (the corrections /sqrt(3)) due to way the error.bars() function plots
data <- rbind(res.test[,1] + n.std*sqrt(res.test[,2])/sqrt(3),
              res.test[,1],
              res.test[,1] - n.std*sqrt(res.test[,2])/sqrt(3))
sorted <- sort(y.test, index.return=T)$ix     # indices in increasing order of y.test 

# Now plot each emulator prediction +/- 3 std for the test set, the actual test
# values, and the linear regression prediction
i <- sorted[51:75]
L <- length(i)
col.em <- rgb(1, 0.4, 0.4)
# Emulator predictions show as cat eyes plot
error.bars(data[,i], alpha = 0.0954659663, eyes=T, col=col.em,
           main = 'Emulator and regression predictions',
           xlab = 'Simulation Index',
           ylab = 'Implausibility Measure'
           )
# Add real y.test values
points(1:L, y.test[i], pch=8, cex=1.3, col='blue')
# Add linear regression predictions
points(1:L, LRpreds[i], pch=23, col = 'black', bg = 'yellow', lwd=1.9, cex = 1.3)
legend('topleft', legend = c('Simulator', 'Emulator', 'Linear regression'), 
       pch = c(8, 22, 23), col = c('blue', 'black', 'black'), 
       pt.bg = c(NA, col.em, 'yellow'),  pt.cex= c(1, 1.5, 1.3), 
       bg = 'aliceblue')

#lightcyan1, slategray1, mintcream


###############################################################################


###############################################################################
# THIS PART RUNS THE EMULATORS WITH OPTIMISED PARAMETERS
###############################################################################

load('RData/Opt_Params_MaxSummerMonths.RData')

Impl <- Impl.Kitch.Aug
y.train <- Impl[train]
y.valid <- Impl[valid]
y.test <- Impl[test]

# Select regressors and active variables to use
Interactions <- poly(as.matrix(Design[,]), degree=4)
L <- summary(regsubsets(y.train~., data=as.data.frame(Interactions[train,]), method = "forward", nvmax = 15))
which(regr <- L$which[5,-1])            # logical vector with regressors corresponding to selected model
fit <- lm(y.train~ ., data = as.data.frame(Interactions[train, regr ]))
summary(fit)


# LOAD ALL TEST INPUTS
load('../RCode/RData/Results/Eval_Inputs.RData') # load 'Eval.points.full'
Eval.points <- Eval.points.full[1:100000, ]

# The following are the active inputs to choose for each month and room:
# June, Master: c(1,3,6)
# June, Kitchen: c(1,6,8)
# July, Master: c(1,3,4,6)
# July, Kitchen: c(1,6,8)
# Aug, Master: c(1,3,6)
# Aug, Kitch: c(1,6,8)

Active.Inputs <- c(1,6,8)
Train.ActInp <- Design[train, Active.Inputs, drop=F]         # design points used to train the emulator
Train.regr <- cbind(1, Interactions[train, regr, drop=F])    # regressors: add a column of 1s for intercept
Test.ActInp  <- Eval.points[, Active.Inputs, drop=F]
Test.regr  <- cbind(1, predict(Interactions, newdata = Eval.points)[, regr])

# Emulation parameters
beta <- fit$coefficients
Cov.beta <- vcov(fit)
sig2 <- sig2.Kitch.Aug
nu2 <- nu2.Kitch.Aug
d <- d.Kitch.Aug

system.time(
res.test <- BL.Emul(Train.ActInp, Test.ActInp, y.train, 
                    Regress.Design = Train.regr, 
                    Regress.Test = Test.regr, 
                    beta = beta, Cov.beta = Cov.beta, 
                    sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)
)

library(plot3D)
scatter2D(Eval.points[,1], Eval.points[,6], colvar = res.test[,1], pch = 20, cex=0.4)
