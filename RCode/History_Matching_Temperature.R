setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')

library(openxlsx)
library(leaps)
library(fields)
source('../../Emulation.R')


################################################
###### LOAD SEVERAL FUNCTIONS AND VARIABLES
################################################

source('Auxiliary_Scripts/Auxiliary_Functions.R')
rm(Create.my.list, Cross_Val, Extract.Flat.Level)

# Design points
source('Auxiliary_Scripts/Load_data.R')
Design <- Rescale.Linearly(Design)
rm(Outputs_gas, Obs_gas, month.names, Rescale.Linearly)

# Optimised emulation parameters + Train-cross-test sets
load('RData/Opt_Params_MaxSummerMonths.RData')

# Implausibility Measures for Max Summer months
load('RData/Summer_Implausibilities.RData')


##############################################
# PREDICT OVER LOTS OF POINTS

# Eval.points.full <- 2*sobol(N, dim = 8, scrambling = 1, seed = 2341) -1
load('RData/Test_Inputs.RData') # loads Test.points.full
Eval.points.full <- Test.points.full
rm(Test.points.full)

Eval.points <- Eval.points.full[1:1.e6, ]


###############################################################################

######################
### JUNE, MASTER
######################

# Linear regression
subset.vars <- c(1,6)  # subset of variables whose powers will be used to select regressors
Interactions <- poly(as.matrix(Design[,subset.vars]), degree=3)
y.train <- Impl.Mast.Jun[train]
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

sig2 <- sig2.Mast.Jun
nu2 <- nu2.Mast.Jun
d <- d.Mast.Jun

Emul.Mast.Jun <- BL.Emul(Train.ActInp, Eval.ActInp, y.train, 
                         Regress.Design = Train.regr, 
                         Regress.Test = Eval.regr, 
                         beta = beta, Cov.beta = Cov.beta, 
                         sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)


###############################################################################

######################
### JUNE, KITCHEN
######################

# Linear regression
subset.vars <- c(1,6,8)
Interactions <- poly(as.matrix(Design[,subset.vars]), degree=3)
y.train <- Impl.Kitch.Jun[train]
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

sig2 <- sig2.Kitch.Jun
nu2 <- nu2.Kitch.Jun
d <- d.Kitch.Jun

Emul.Kitch.Jun <- BL.Emul(Train.ActInp, Eval.ActInp, y.train, 
                          Regress.Design = Train.regr, 
                          Regress.Test = Eval.regr, 
                          beta = beta, Cov.beta = Cov.beta, 
                          sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)


###############################################################################

######################
### JULY, MASTER
######################

# Linear regression
subset.vars <- c(3,6)
Interactions <- poly(as.matrix(Design[, subset.vars]), degree=4)
y.train <- Impl.Mast.Jul[train]
L <- summary(regsubsets(y.train~., data=as.data.frame(Interactions[train,]), method = "forward", nvmax = 5))
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

system.time(
Emul.Mast.Jul <- BL.Emul(Train.ActInp, Eval.ActInp, y.train, 
                         Regress.Design = Train.regr, 
                         Regress.Test = Eval.regr, 
                         beta = beta, Cov.beta = Cov.beta, 
                         sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)
)


###############################################################################

######################
### JULY, KITCHEN
######################

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

######################
### AUGUST, MASTER
######################

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

system.time(
  Emul.Mast.Aug <- BL.Emul(Train.ActInp, Eval.ActInp, y.train, 
                           Regress.Design = Train.regr, 
                           Regress.Test = Eval.regr, 
                           beta = beta, Cov.beta = Cov.beta, 
                           sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)
)


###############################################################################

######################
### AUGUST, KITCHEN
######################

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
     file = 'RData/Results_Temperature.RData')

################################
## NON-IMPLAUSIBLE REGIONS

ind.M.Jun <- (Emul.Mast.Jun[,1] - 3*sqrt(Emul.Mast.Jun[,2]))<9
ind.K.Jun <- (Emul.Kitch.Jun[,1] - 3*sqrt(Emul.Kitch.Jun[,2]))<9
ind.M.Jul <- (Emul.Mast.Jul[,1] - 3*sqrt(Emul.Mast.Jul[,2]))<9
ind.K.Jul <- (Emul.Kitch.Jul[,1] - 3*sqrt(Emul.Kitch.Jul[,2]))<9
ind.M.Aug <- (Emul.Mast.Aug[,1] - 3*sqrt(Emul.Mast.Aug[,2]))<9
ind.K.Aug <- (Emul.Kitch.Aug[,1] - 3*sqrt(Emul.Kitch.Aug[,2]))<9


ind <- ind.M.Jun & ind.M.Jul & ind.M.Aug & ind.K.Jun & ind.K.Jul & ind.K.Aug

plot(Eval.points[ind,6], Eval.points[ind,8],
     cex =0.2, pch = 20, col='blue',
     xlim = c(-1,1), ylim = c(-1,1)
)
points(Eval.points[ind,8], Eval.points[ind,6],
       cex =0.4, pch = 20, col='blue',
       xlim = c(-1,1), ylim = c(-1,1)
)

library("plot3D")
subs <- 1:1.e5
scatter2D(Eval.points[subs,8], Eval.points[subs,6], colvar = Emul.Kitch.Jul[subs], 
          pch=20, cex=0.2)



