################################################################################
#
# This script loads summer implausibility values of temperature time series from
# RData/Inputs/Implausibilities.RData (file created through Temp_Implaus.R).
#
# Emulator of the implausibility values are built and validated (for kitchen and
# master), so that suitable parameters can be used for actual emulation in 
# "HM_Summer_Temp_Implausibility.R". 
#
################################################################################


#############################################################
## SET FOLDER, LOAD LIBRARIES & DATA

setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')

library(leaps)
source('../../Emulation.R')

# Two rooms, Master and Kitchen: 1 observed and 1000 simulated time series
load('RData/Inputs/Simulated_and_Observed_Temperatures.RData')

# Load Summer Implausibilities for the 1000 simulations
load('RData/Inputs/Implausibilities.RData')
rm(Impl.Kitch.Jun, Impl.Kitch.Jul, Impl.Kitch.Aug,
   Impl.Mast.Jun,  Impl.Mast.Jul,  Impl.Mast.Aug)


#################################################################
# LINEAR REGRESSION AND EMULATION (KITCHEN)

# TRAINING, EVALUATION AND TEST SETS
load('RData/Inputs/SplitSet.RData')

# OUTPUTS
y.train <- Impl.Kitch.Sum[train]
y.eval <- Impl.Kitch.Sum[eval]
y.test <- Impl.Kitch.Sum[test]

# LINEAR REGRESSION
Interactions <- poly(Design[train,], degree=4)
L <- regsubsets(y.train~., data=Interactions, method = "forward", nvmax = 15)
regr <- summary(L)$which[10,-1]            # logical vector with selected regressors
fit <- lm(y.train~ ., data = as.data.frame(Interactions[, regr]))
summary(fit)

# REGRESSION ON RESIDUALS
res <- fit$residuals
L.res <- regsubsets(res~., data = Interactions, method = "forward", nvmax = 5)
regr.res <- summary(L.res)$which[5,-1]
# choose 1,3,6,8 as active variables

# ACTIVE INPUTS AND REGRESSORS FOR TRAINING, EVALUATION, TEST SETS
Active.Inputs <- c(1,3,6,8)
Train.ActInp <- Design[train, Active.Inputs, drop=F]    # design points used to train the emulator
Eval.ActInp  <- Design[eval,  Active.Inputs, drop=F]
Test.ActInp  <- Design[test,  Active.Inputs, drop=F]
Train.Regr   <- cbind(1, Interactions[, regr, drop=F])  # regressors: add a column of 1s for intercept
temp <- predict(Interactions, newdata = Design[eval,])
Eval.Regr    <- cbind(1, temp[, regr, drop=F])
temp <- predict(Interactions, newdata = Design[test,])
Test.Regr    <- cbind(1, temp[, regr, drop=F])

# EMULATION PARAMETERS
beta <- fit$coefficients
s2.tot <- var(res)
nugget.frac <- 0.05
s2 <- (1-nugget.frac)*s2.tot
nu2 <- nugget.frac*s2.tot
d <- 0.5

Emul.Eval <- BL.Emul(ActInp.Design = Train.ActInp, 
                     ActInp.Test = Eval.ActInp, 
                     y = y.train, 
                     Regress.Design = Train.Regr, 
                     Regress.Test = Eval.Regr, 
                     beta = beta,
                     sigma2 = s2, kernel = 'exp2', d = d, nu2 = nu2)

Emul.Test <- BL.Emul(ActInp.Design = Train.ActInp, 
                     ActInp.Test = Test.ActInp, 
                     y = y.train, 
                     Regress.Design = Train.Regr, 
                     Regress.Test = Test.Regr, 
                     beta = beta,
                     sigma2 = s2, kernel = 'exp2', d = d, nu2 = nu2)


########################################################################
# COMPARE TO KNOWN RESULTS FOR THE EVALUATION/TEST SETS

View(cbind(y.eval, Emul.Eval[,1], sqrt(Emul.Eval[,2]) ))
err <- y.eval - Emul.Eval[,1]  
err.std <- err/sqrt(Emul.Eval[,2])
plot(Emul.Eval[,1], err.std)
NI <- y.eval<9
NI.emul <- Emul.Eval[,1]- 3*sqrt(Emul.Eval[,2]) < 9
sum(NI) == sum(NI & NI.emul) # if true, no point is wrongly classified as implausible

View(cbind(y.test, Emul.Test[,1], sqrt(Emul.Test[,2]) ))
err <- y.test - Emul.Test[,1]  
err.std <- err/sqrt(Emul.Test[,2])
plot(Emul.Test[,1], err.std)
NI <- y.test<9
NI.emul <- Emul.Test[,1]- 3*sqrt(Emul.Test[,2]) < 9
sum(NI) == sum(NI & NI.emul) # if true, no point is wrongly classified as implausible


################################################################################


#################################################################
# LINEAR REGRESSION AND EMULATION (MASTER)

# TRAINING, EVALUATION AND TEST SETS
load('RData/Inputs/SplitSet.RData')

# OUTPUTS
y.train <- Impl.Mast.Sum[train]
y.eval <- Impl.Mast.Sum[eval]
y.test <- Impl.Mast.Sum[test]

# LINEAR REGRESSION
Interactions <- poly(Design[train,], degree=4)
L <- regsubsets(y.train~., data=Interactions, method = "forward", nvmax = 15)
regr <- summary(L)$which[10,-1]            # logical vector with selected regressors
fit <- lm(y.train~ ., data = as.data.frame(Interactions[, regr]))
summary(fit)

# REGRESSION ON RESIDUALS
res <- fit$residuals
L.res <- regsubsets(res~., data = Interactions, method = "forward", nvmax = 8)
regr.res <- summary(L.res)$which[8,-1]
summary(lm(res~., data = as.data.frame(Interactions[, regr.res])))

# choose 1,3,4,6 as active variables

# ACTIVE INPUTS AND REGRESSORS FOR TRAINING, EVALUATION, TEST SETS
Active.Inputs <- c(1,3,4,6)
Train.ActInp <- Design[train, Active.Inputs, drop=F]    # design points used to train the emulator
Eval.ActInp  <- Design[eval,  Active.Inputs, drop=F]
Test.ActInp  <- Design[test,  Active.Inputs, drop=F]
Train.Regr   <- cbind(1, Interactions[, regr, drop=F])  # regressors: add a column of 1s for intercept
temp <- predict(Interactions, newdata = Design[eval,])
Eval.Regr    <- cbind(1, temp[, regr, drop=F])
temp <- predict(Interactions, newdata = Design[test,])
Test.Regr    <- cbind(1, temp[, regr, drop=F])

# EMULATION PARAMETERS
beta <- fit$coefficients
s2.tot <- var(res)
nugget.frac <- 0.05
s2 <- (1-nugget.frac)*s2.tot
nu2 <- nugget.frac*s2.tot
d <- 0.4

Emul.Eval <- BL.Emul(ActInp.Design = Train.ActInp, 
                      ActInp.Test = Eval.ActInp, 
                      y = y.train, 
                      Regress.Design = Train.Regr, 
                      Regress.Test = Eval.Regr, 
                      beta = beta,
                      sigma2 = s2, kernel = 'exp2', d = d, nu2 = nu2)

Emul.Test <- BL.Emul(ActInp.Design = Train.ActInp, 
                     ActInp.Test = Test.ActInp, 
                     y = y.train, 
                     Regress.Design = Train.Regr, 
                     Regress.Test = Test.Regr, 
                     beta = beta,
                     sigma2 = s2, kernel = 'exp2', d = d, nu2 = nu2)


########################################################################
# COMPARE TO KNOWN RESULTS FOR THE EVALUATION/TEST SETS

View(cbind(y.eval, Emul.Eval[,1], sqrt(Emul.Eval[,2]) ))
err <- y.eval - Emul.Eval[,1]  
err.std <- err/sqrt(Emul.Eval[,2])
plot(Emul.Eval[,1], err.std)
NI <- y.eval<9
NI.emul <- Emul.Eval[,1]- 3*sqrt(Emul.Eval[,2]) < 9
sum(NI) == sum(NI & NI.emul) # if true, no point is wrongly classified as implausible

View(cbind(y.test, Emul.Test[,1], sqrt(Emul.Test[,2]) ))
err <- y.test - Emul.Test[,1]  
err.std <- err/sqrt(Emul.Test[,2])
plot(Emul.Test[,1], err.std)
NI <- y.test<9
NI.emul <- Emul.Test[,1]- 3*sqrt(Emul.Test[,2]) < 9
sum(NI) == sum(NI & NI.emul) # if true, no point is wrongly classified as implausible







