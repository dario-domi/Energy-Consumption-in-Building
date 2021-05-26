load('RData/Summer_Implausibilities.RData')

source('Auxiliary_Scripts/Auxiliary_Functions.R')
rm(Create.my.list, Cross_Val, Extract.Flat.Level)

# Design points
source('Auxiliary_Scripts/Load_data.R')
Design <- Rescale.Linearly(Design)
rm(Outputs_gas, Obs_gas, month.names, Rescale.Linearly)

source('../../Emulation.R')




##### LINEAR REGRESSION
set.seed(5879, kind = "default")
train <- sample(1:1000, 750)
cross <- sample((1:1000)[-train], 150)
test <- (1:1000)[-c(train, cross)]

Impl <- Impl.Mast.Jul
y.train <- Impl[train]
y.cross <- Impl[cross]
y.test <- Impl[test]

# Select regressors and active variables to use
Interactions <- poly(as.matrix(Design[,]), degree=4)
L <- summary(regsubsets(y.train~., data=as.data.frame(Interactions[train,]), method = "forward", nvmax = 15))
which(regr <- L$which[5,-1])            # logical vector with regressors corresponding to selected model
fit <- lm(y.train~ ., data = as.data.frame(Interactions[train, regr ]))
summary(fit)


##### EMULATOR
# June, Master: c(1,3,6)
# June, Kitchen: c(1,6,8)
# July, Master: c(1,3,4,6)
# July, Kitchen: c(1,6,8)
# Aug, Master: c(1,3,6)
# Aug, Kitch: c(1,6,8)
Active.Inputs <- c(1,3,6)
Train.ActInp <- Design[train, Active.Inputs, drop=F]         # design points used to train the emulator
Train.regr <- cbind(1, Interactions[train, regr, drop=F])    # regressors: add a column of 1s for intercept
Cross.ActInp <- Design[cross, Active.Inputs, drop=F]
Cross.regr <- cbind(1, Interactions[cross, regr, drop=F])   
Test.ActInp  <- Design[test, Active.Inputs, drop=F]
Test.regr  <- cbind(1, Interactions[test, regr, drop=F])

# Regression coefficients mean and covariance
beta <- fit$coefficients
Cov.beta <- vcov(fit)

################################################
# FIND THE OPTIMAL PARAMETERS

# Initial values for optimisation
N_var_GP <- length(Active.Inputs)
d0 <- 0.4*replicate(N_var_GP, 1)         # Correlation lengths
s2.tot0 <- var(fit$residuals)       # Prior cumulative variance of homoschedastic Gaussian process
nug_frac0 <- 0.01                          # Fraction of residual variability not explained by regressors
kernel <- 'exp2'

Opt.pars <- Find.Optimal.Parameters(Train.ActInp, Train.regr,
                                    Cross.ActInp, Cross.regr,
                                    y.train, y.cross, 
                                    kernel, d0, s2.tot0, nug_frac0)

sig2.Kitch.Aug <- Opt.pars$s2
nu2.Kitch.Aug <- Opt.pars$nu2
d.Kitch.Aug <- Opt.pars$d
val.Kitch.Aug <- Opt.pars$obj

## Compare performance on cross-validation set and test set
sig2 <- sig2.Mast.Jul
nu2 <- nu2.Mast.Jul
d <- d.Mast.Jul

res.test <- BL.Emul(Train.ActInp, Test.ActInp, y.train, 
                    Regress.Design = Train.regr, 
                    Regress.Test = Test.regr, 
                    beta = beta, Cov.beta = Cov.beta, 
                    sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)

res.cross <- BL.Emul(Train.ActInp, Cross.ActInp, y.train, 
                     Regress.Design = Train.regr, 
                     Regress.Test = Cross.regr, 
                     beta = beta, Cov.beta = Cov.beta, 
                     sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)

# Values of objective function on cross-validation and test sets
sum(dnorm(y.cross, mean = res.cross[,1], sd = sqrt(res.cross[,2]), log = T))/length(y.cross)
sum(dnorm(y.test,  mean = res.test[,1],  sd = sqrt(res.test[,2]),  log = T))/length(y.test)

View(cbind(predict(fit, as.data.frame(Interactions[test, regr])),
           y.test, res.test[,1], sqrt(res.test[,2])))

# Histgram of (strandardised) errors and standard deviations
hist( (y.test-res.test[,1])/sqrt(res.test[,2]), breaks = 30)
hist(sqrt(res.test[,2]), breaks = 30)
hist( y.test-res.test[,1]  , breaks = 30)


## SAVE THE OPTIMISED PARAMETERS AND TRAIN-CROSS-TEST SUBDIVISION
save(train, cross, test,
     sig2.Mast.Jul, nu2.Mast.Jul, d.Mast.Jul, val.Mast.Jul,
     sig2.Kitch.Jul, nu2.Kitch.Jul, d.Kitch.Jul, val.Kitch.Jul,
     sig2.Mast.Jun, nu2.Mast.Jun, d.Mast.Jun, val.Mast.Jun,
     sig2.Kitch.Jun, nu2.Kitch.Jun, d.Kitch.Jun, val.Kitch.Jun,
     sig2.Mast.Aug, nu2.Mast.Aug, d.Mast.Aug, val.Mast.Aug,
     sig2.Kitch.Aug, nu2.Kitch.Aug, d.Kitch.Aug, val.Kitch.Aug,
     file = "RData/Opt_Params_MaxSummerMonths.RData")


###############################################################################


###############################################################################
# THIS PART RUNS LINEAR REGRESSIONS AND EMULATORS WITH OPTIMISED PARAMETERS
###############################################################################

load('RData/Opt_Params_MaxSummerMonths.RData')

Impl <- Impl.Kitch.Aug
y.train <- Impl[train]
y.cross <- Impl[cross]
y.test <- Impl[test]

# Select regressors and active variables to use
Interactions <- poly(as.matrix(Design[,]), degree=4)
L <- summary(regsubsets(y.train~., data=as.data.frame(Interactions[train,]), method = "forward", nvmax = 15))
which(regr <- L$which[5,-1])            # logical vector with regressors corresponding to selected model
fit <- lm(y.train~ ., data = as.data.frame(Interactions[train, regr ]))
summary(fit)

Active.Inputs <- c(1,6,8)
Train.ActInp <- Design[train, Active.Inputs, drop=F]         # design points used to train the emulator
Train.regr <- cbind(1, Interactions[train, regr, drop=F])    # regressors: add a column of 1s for intercept
Test.ActInp  <- Design[test, Active.Inputs, drop=F]
Test.regr  <- cbind(1, Interactions[test, regr, drop=F])

# Emulation parameters
beta <- fit$coefficients
Cov.beta <- vcov(fit)
sig2 <- sig2.Kitch.Aug
nu2 <- nu2.Kitch.Aug
d <- d.Kitch.Aug

res.test <- BL.Emul(Train.ActInp, Test.ActInp, y.train, 
                    Regress.Design = Train.regr, 
                    Regress.Test = Test.regr, 
                    beta = beta, Cov.beta = Cov.beta, 
                    sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)

hist(sqrt(res.test[,2]))



