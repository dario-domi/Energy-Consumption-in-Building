setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/')

library(openxlsx)
library(leaps)
library(fields)
source('Scripts/Auxiliary_Functions.R')
source('Scripts/Find_Optimal_Params.R')
source('../Emulation.R')

###############################################################################
# LOAD OBSERVED AND SIMULATED TEMPERATURES: hourly, for one year (8760 obs)
###############################################################################

file <- "Data/Temperature_Data/Actual Space Temperatures - Dario.xlsx"
Table <- read.xlsx(file)

## DATES AND TIMES
DateTimes <- Table[,1]
DateTimes <- as.POSIXct(DateTimes*86400, origin="1899-12-30", tz='Europe/London')

## KITCHEN TEMPERATURES
Kitch.Obs <- Table[, "Kitchen"]

## MASTER TEMPERATURES
Master.Obs <- Table[, "Master"]
rm(file, Table)

## LOAD SIMULATED TEMPERATURES
# Kitch.Sim & Master.Sim: 8760 x 1000
load('RData/Simulated-Temperatures.RData')

# DESIGN POINTS
source('Scripts/Load_data.R') 
Design <- Rescale.Linearly(Design)


##########  Computing daily maxima

X <- Master.Sim
C <- array(X, dim = c(24, 365, 1000))
Max.Mast.Sim <- apply(C, c(2,3), max)
X <- array(Master.Obs, dim = c(24,365))
Max.Mast.Obs <- apply(X, 2, max)

X <- Kitch.Sim
C <- array(X, dim = c(24, 365, 1000))
Max.Kitch.Sim <- apply(C, c(2,3), max)
X <- array(Kitch.Obs, dim = c(24,365))
Max.Kitch.Obs <- apply(X, 2, max)

rm(C,X)


### IMPLAUSIBILITY ON SIMULATOR RUNS
times <- 152:181 # June
times <- 182:212 # July
times <- 213:243 # August
times.hours <- (times-1)*24+1

V.Obs <- 0.01 * Corr.fun(times.hours, times.hours, d = 12, string = 'exp2')
V.MD <- 0.16 * Corr.fun(times.hours, times.hours, d = 96, string = 'exp2')
V.tot <- V.Obs+V.MD

x <- Max.Mast.Sim[times, ] - Max.Mast.Obs[times] # t x 1000
Impl.Mast.Jul <- diag( t(x)%*%solve(V.tot, x) )/length(times)
Impl <- Impl.Mast.Jul

# July: active GP variables, 1,3,4,6; 

set.seed(5879, kind = "default")
train <- sample(1:1000, 750)
cross <- sample((1:1000)[-train], 150)
test <- (1:1000)[-c(train, cross)]
y.train <- Impl[train]
y.cross <- Impl[cross]
y.test <- Impl[test]

# Select regressors and active variables to use
Interactions <- poly(as.matrix(Design[,6]), degree=4)
L <- summary(regsubsets(y.train~., data=as.data.frame(Interactions[train,]), method = "forward", nvmax = 15))
which(regr <- L$which[4,-1])            # logical vector with regressors corresponding to selected model
fit <- lm(y.train~ ., data = as.data.frame(Interactions[train, regr ]))
summary(fit)

GP_variables <- c(1,3,4,6)
Train.ActInp <- Design[train, GP_variables, drop=F]         # design points used to train the emulator
Train.regr <- cbind(1, Interactions[train, regr, drop=F])   # regressors: add a column of 1s for intercept
Cross.ActInp <- Design[cross, GP_variables, drop=F]
Cross.regr <- cbind(1, Interactions[cross, regr, drop=F])   
Test.ActInp  <- Design[test, GP_variables, drop=F]
Test.regr  <- cbind(1, Interactions[test, regr, drop=F])


# Emulation hyperparameters
beta <- fit$coefficients
Cov.beta <- vcov(fit)

N_var_GP <- length(GP_variables)
d <- 0.4*replicate(N_var_GP, 1)         # Correlation lengths
sigma2.tot <- var(fit$residuals)        # Prior cumulative variance of homoschedastic Gaussian process
nugget_frac <- 0.05                          # Fraction of residual variability not explained by regressors
nu <- nugget_frac*sigma2.tot                 # Variance of nugget term (Gaussian noise)
sig2 <- (1-nugget_frac)*sigma2.tot           # Variance of Gaussian process
kernel <- 'exp2'

# Call the function which carries out  Bayes Linear Emulation, on the selected regressors given observed output y

d0 <- d
sigma2.tot0 <- sigma2.tot
nug_frac0 <- nugget_frac


load('RData/Test_Inputs.RData')
All.regr <- predict(Interactions, newdata = Test.points.full[,6])
Testall.ActInp <- Test.points.full[1:10000, GP_variables]
Testall.regr <- cbind(1, All.regr[1:10000,])
res.eps <- BL.Emul(Train.ActInp, Testall.ActInp, y.train, 
               Regress.Design = Train.regr, 
               Regress.Test = Testall.regr, 
               beta = beta, Cov.beta = Cov.beta, 
               sigma2 = s2.opt.Jul.eps, kernel = 'exp2', d = d.opt.Jul.eps, nu2 = nu2.opt.Jul.eps)


ind <-(res[,1]-3*sqrt(res[,2]))<3
ind.eps <-(res.eps[,1]-3*sqrt(res.eps[,2]))<3

plot(Test.points.full[1:10000,][ind,3],
     Test.points.full[1:10000,][ind,6],
     cex =0.6, pch = 20, col='red',
     xlim = c(-1,1), ylim = c(-1,1)
)




res <- BL.Emul(Train.ActInp, Cross.ActInp, y.train, 
                     Regress.Design = Train.regr, 
                     Regress.Test = Cross.regr, 
                     beta = beta, Cov.beta = Cov.beta, 
                     sigma2 = sig2, kernel = 'exp2', d = d, nu = nu)


sum( dnorm(y.cross, mean = res[,1], sd = sqrt(res[,2]), log = T) )

system.time(
  for (i in 1:10){
    Find.Optimal.Parameters(Train.ActInp, Train.regr,
                        Cross.ActInp, Cross.regr,
                        y.train, y.cross, 
                        kernel, 1, 1, 1)
  }
)

# On test points
sigma2.tot <- exp(opt.par2$par[1])
nugget_frac <- opt.par2$par[2]
sig2 <- sigma2.tot*(1-nugget_frac)
nu <- sigma2.tot*nugget_frac
d <- exp(opt.par2$par[3:6])

res2 <- BL.Emul(Train.ActInp, Test.ActInp, y.train, 
        Regress.Design = Train.regr, 
        Regress.Test = Test.regr, 
        beta = beta, Cov.beta = Cov.beta, 
        sigma2 = sig2, kernel = 'exp2', d = d, nu = nu)


hist(res.old[,2])


y.pred <- predict(fit, as.data.frame(Interactions[cross, ]))



