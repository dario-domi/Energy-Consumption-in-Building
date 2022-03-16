###############################################################################
#
# Using the parameters validated in "Validate_Summer_Temp_Implaus_Emulators.R",
# this script builds emulators of Master/Kitchen Summer temperature
# implausibilities, and performs history matching. Results are explored in  
# addition to the ones coming from gas history matching.
#
###############################################################################


#############################################################
## SET FOLDER, LOAD LIBRARIES & DATA

setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')

library(leaps)
source('../../Emulation.R')

# Two rooms, Master and Kitchen: 1 observed and 1000 simulated time series
load('RData/Inputs/Simulated_and_Observed_Temperatures.RData')

# Training, validation and test sets
load('RData/Inputs/SplitSet.RData')

# Load Summer Implausibilities for the 1000 simulations
load('RData/Inputs/Implausibilities.RData')
rm(Impl.Kitch.Jun, Impl.Kitch.Jul, Impl.Kitch.Aug,
   Impl.Mast.Jun,  Impl.Mast.Jul,  Impl.Mast.Aug)


#################################################################
# LINEAR REGRESSION AND EMULATION (KITCHEN)

# EVALUATION INPUTS
load('RData/Results_Emulation/Eval_Inputs.RData') # loads Eval.points.full
N <- 1.e6
Eval.points <- Eval.points.full[1:N, ]
rm(Eval.points.full)  # clean workspace
invisible(gc())       # release memory

# OUTPUTS AT DESIGN POINTS
y.train <- Impl.Kitch.Sum[train]

# LINEAR REGRESSION
Interactions <- poly(Design[train,c(1,6,8)], degree=4)
L <- regsubsets(y.train~., data=Interactions, method = "forward", nvmax = 10)
regr <- summary(L)$which[10,-1]            # logical vector with selected regressors
fit <- lm(y.train~ ., data = as.data.frame(Interactions[, regr]))

# ACTIVE INPUTS AND REGRESSORS FOR TRAINING, EVALUATION, TEST SETS
Active.Inputs <- c(1,3,6,8)
Train.ActInp <- Design[train, Active.Inputs, drop=F]           # design points used to train the emulator
Train.Regr   <- cbind(1, Interactions[, regr, drop=F])         # regressors: add a column of 1s for intercept
Eval.ActInp  <- Eval.points[, Active.Inputs, drop=F]           # active inputs for evaluation set
temp <- predict(Interactions, newdata = Eval.points[, c(1,6,8)])
Eval.Regr    <- cbind(1, temp[, regr, drop=F])                 # regressors for evaluation set

# EMULATION PARAMETERS
beta <- fit$coefficients
s2.tot <- var(fit$residuals)
nugget.frac <- 0.05
s2 <- (1-nugget.frac)*s2.tot
nu2 <- nugget.frac*s2.tot
d <- 0.5

system.time(Emul.Kitch <- BL.Emul(ActInp.Design = Train.ActInp, 
                     ActInp.Test = Eval.ActInp, 
                     y = y.train, 
                     Regress.Design = Train.Regr, 
                     Regress.Test = Eval.Regr, 
                     beta = beta,
                     sigma2 = s2, kernel = 'exp2', d = d, nu2 = nu2)
)

kitch.compat <- (Emul.Kitch[,1] - 3*sqrt(Emul.Kitch[,2]))<9


#################################################################
# LINEAR REGRESSION AND EMULATION (MASTER)

# OUTPUTS AT DESIGN POINTS
y.train <- Impl.Mast.Sum[train]

# LINEAR REGRESSION
Interactions <- poly(Design[train, c(1,3,4,6)], degree=4)
L <- regsubsets(y.train~., data=Interactions, method = "forward", nvmax = 10)
regr <- summary(L)$which[10,-1]            # logical vector with selected regressors
fit <- lm(y.train~ ., data = as.data.frame(Interactions[, regr]))

# ACTIVE INPUTS AND REGRESSORS FOR TRAINING, EVALUATION, TEST SETS
Active.Inputs <- c(1,3,4,6)
Train.ActInp <- Design[train, Active.Inputs, drop=F]    # design points used to train the emulator
Train.Regr   <- cbind(1, Interactions[, regr, drop=F])  # regressors: add a column of 1s for intercept

Eval.ActInp  <- Eval.points[, Active.Inputs, drop=F]
temp <- predict(Interactions, newdata = Eval.points[, c(1,3,4,6)])
Eval.Regr    <- cbind(1, temp[, regr, drop=F])

# EMULATION PARAMETERS
beta <- fit$coefficients
s2.tot <- var(fit$residuals)
nugget.frac <- 0.05
s2 <- (1-nugget.frac)*s2.tot
nu2 <- nugget.frac*s2.tot
d <- 0.4

system.time(Emul.Mast <- BL.Emul(ActInp.Design = Train.ActInp, 
                      ActInp.Test = Eval.ActInp, 
                      y = y.train, 
                      Regress.Design = Train.Regr, 
                      Regress.Test = Eval.Regr, 
                      beta = beta,
                      sigma2 = s2, kernel = 'exp2', d = d, nu2 = nu2)
)

mast.compat <- (Emul.Mast[,1] - 3*sqrt(Emul.Mast[,2]))<9

#save(kitch.compat, mast.compat, gas.compat, file = "RData/Results_Emulation/Non-Implausible-Inputs.RData")


################################################################################
# COMPATIBILITY BETWEEN GAS AND TEMPERATURE DATA

load("RData//Results_Emulation/Non-Implausible-Inputs.RData")

cat("Percentage of kitchen non-implausible space: ", 
    format(100*mean(kitch.compat), digits = 2, nsmall = 2), "%", sep = "")

cat("Percentage of master non-implausible space: ", 
    format(100*mean(mast.compat), digits = 2, nsmall = 2), "%", sep = "")

cat("Percentage of kitchen&master non-implausible space: ", 
    format(100*mean(mast.compat & kitch.compat), digits = 2, nsmall = 2), "%", sep = "")

##############

cat("Percentage of master non-implausible space given it being kitchen non-implausible: ", 
    format(100*mean(mast.compat&kitch.compat)/mean(kitch.compat), digits = 2, nsmall = 2), "%", sep = "")

cat("Percentage of kitchen non-implausible space given it being master non-implausible: ", 
    format(100*mean(mast.compat&kitch.compat)/mean(mast.compat), digits = 2, nsmall = 2), "%", sep = "")

##############

gas <- gas.compat[1:length(kitch.compat)]

cat("Percentage of gas non-implausible space: ", 
    format(100*mean(gas), digits = 4, nsmall = 2), "%", sep = "")

cat("Percentage of kitchen&gas non-implausible space: ", 
    format(100*mean(kitch.compat & gas), digits = 4, nsmall = 2), "%", sep = "")

cat("Percentage of kitchen non-implausible space given it being gas non-implausible: ", 
    format(100*mean(gas&kitch.compat)/mean(gas), digits = 2, nsmall = 2), "%", sep = "")

##############

cat("Percentage of gas non-implausible space: ", 
    format(100*mean(gas), digits = 4, nsmall = 2), "%", sep = "")

cat("Percentage of master&gas non-implausible space: ", 
    format(100*mean(mast.compat & gas), digits = 4, nsmall = 2), "%", sep = "")

cat("Percentage of master non-implausible space given it being gas non-implausible: ", 
    format(100*mean(gas&mast.compat)/mean(gas), digits = 2, nsmall = 2), "%", sep = "")


###############################################################################
# SCATTER PLOT OF NON-IMPLAUSIBLE POINTS

c1=6
c2=8

plot(Eval.points[gas,c1], Eval.points[gas,c2], col = 'red',
     xlim = c(-1,1), ylim = c(-1,1), 
     cex=0.7, pch=20,
     xlab = colnames(Design)[c1],
     ylab = colnames(Design)[c2])

x1 <- Eval.points[kitch.compat & mast.compat,c1]
x2 <- Eval.points[kitch.compat & mast.compat,c2]
ind <- sample(length(kitch.compat), 1000)
points(x1[ind], x2[ind], col = 'blue',
     xlim = c(-1,1), ylim = c(-1,1), 
     cex=0.7, pch=20,
     xlab = colnames(Design)[c1],
     ylab = colnames(Design)[c2])

plot(Eval.points[1:10000, 8], Emul.Kitch[1:10000,1])


library("plot3D")
subs <- 1:1e4
scatter2D(Eval.points[subs,8], Emul.Kitch[subs,1], 
          colvar = Emul.Kitch[subs,1], 
          pch=20, cex=0.2)







