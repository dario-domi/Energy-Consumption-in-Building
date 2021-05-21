setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/')

library(openxlsx)
library(leaps)
library(fields)
source('Scripts/Auxiliary_Functions.R')
source('../Emulation.R')
source('Scripts/Find_Optimal_Params.R')

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

###############################

### JUNE IMPLAUSIBILITY ON SIMULATOR RUNS
times <- 152:181 # June
times.hours <- (times-1)*24+1

### IMPLAUSIBILITY ON SIMULATOR RUNS
V.Obs <- 0.01 * Corr.fun(times.hours, times.hours, d = 12, string = 'exp2')
V.MD <- 0.16 * Corr.fun(times.hours, times.hours, d = 96, string = 'exp2')
V.tot <- V.Obs+V.MD

x <- Max.Mast.Sim[times, ] - Max.Mast.Obs[times] # t x 1000
Impl.Mast.Jun <- diag( t(x)%*%solve(V.tot, x) )/length(times)
x <- Max.Kitch.Sim[times, ] - Max.Kitch.Obs[times]
Impl.Kitch.Jun <- diag( t(x)%*%solve(V.tot, x) )/length(times)

### JULY IMPLAUSIBILITY ON SIMULATOR RUNS
times <- 182:212 # July
times.hours <- (times-1)*24+1

V.Obs <- 0.01 * Corr.fun(times.hours, times.hours, d = 12, string = 'exp2')
V.MD <- 0.16 * Corr.fun(times.hours, times.hours, d = 96, string = 'exp2')
V.tot <- V.Obs+V.MD

x <- Max.Mast.Sim[times, ] - Max.Mast.Obs[times] # t x 1000
Impl.Mast.Jul <- diag( t(x)%*%solve(V.tot, x) )/length(times)
x <- Max.Kitch.Sim[times, ] - Max.Kitch.Obs[times]
Impl.Kitch.Jul <- diag( t(x)%*%solve(V.tot, x) )/length(times)


### AUGUST IMPLAUSIBILITY ON SIMULATOR RUNS
times <- 213:243 # August
times.hours <- (times-1)*24+1

### IMPLAUSIBILITY ON SIMULATOR RUNS
V.Obs <- 0.01 * Corr.fun(times.hours, times.hours, d = 12, string = 'exp2')
V.MD <- 0.16 * Corr.fun(times.hours, times.hours, d = 96, string = 'exp2')
V.tot <- V.Obs+V.MD

x <- Max.Mast.Sim[times, ] - Max.Mast.Obs[times] # t x 1000
Impl.Mast.Aug <- diag( t(x)%*%solve(V.tot, x) )/length(times)
x <- Max.Kitch.Sim[times, ] - Max.Kitch.Obs[times]
Impl.Kitch.Aug <- diag( t(x)%*%solve(V.tot, x) )/length(times)




### PLOTS OF OBSERVED AND SIMULATED TRAJECTORIES

times <- 152:181 # June
times <- 182:212 # July
times <- 213:243 # August
times.hours <- (times-1)*24+1

Obs <- Max.Kitch.Obs
Sim <- Max.Kitch.Sim
Impl <- Impl.Kitch.Jun

Obs <- Max.Mast.Obs
Sim <- Max.Mast.Sim
Impl <- Impl.Mast.Jun

while (T) {
  i <- sample(1000, 1)
  plot(DateTimes[times.hours], Obs[times], ty='l', col='red', 
       xaxt='n', ylim=c(16,26), lwd=1)
  axis.POSIXct(side=1, x=DateTimes[times.hours], format = "%d %b")       # format of x tick-labels
  imp <- sqrt(Impl[i])
  if (imp < 3)
    lines(DateTimes[times.hours], Sim[times,i], ty='l', col='blue')
  else
    lines(DateTimes[times.hours], Sim[times,i], ty='l', col=rgb(0,0,1,0.6), lty=2)
  legend('topright', legend = c(paste('Impl:', format(imp, digits = 3))) )
  print(i)
  Sys.sleep(1.5)
}

# Specific examples to look at
# i=177, master, june. Day 24 makes impl big
# same for i=166

# Kitch July: criterion seems stricter wrt Master July
## Master june: i=582 and i=270

# i=966 for kitchen (june)
# compare i=710 and i=490 for kitchen
# i=373 for kitchen
# compare i=242 and i=103


# Plot of raw and "uncorrelated" difference
U <- chol(V.tot) # V.tot = t(U)%*%U
t <- length(times)
x <- Max.Kitch.Sim[times, i] - Max.Kitch.Obs[times] # t x 1000
x <- Max.Mast.Sim[times, i] - Max.Mast.Obs[times] # t x 1000
y <- solve(t(U), x) 
sqrt(y%*%y/t)
plot(y, col='red')
abline(h=0, lty=2)
points(x, col='blue')

## Compare NROY space (on 1000 design runs) for June, July, August
Impl <- sqrt(Impl.Mast.Aug)
ind.Aug <- Impl<3
sum(ind.Jun)/length(Impl)
sum(ind.Jul)/length(Impl)
sum(ind.Aug)/length(Impl)
sum( ind.Jul & ind.Aug)/length(Impl)

plot(Design[ind.Jun,6], Design[ind.Jun,3], col='red', pch=20, cex=2,
     xlim = c(-1,1), ylim = c(-1,1))
points(Design[ind.Jul,6], Design[ind.Jul,3], col='blue', pch=21, cex=1.5, lwd=2)
points(Design[ind.Aug,6], Design[ind.Aug,3], col='darkgreen', pch=20, cex=1)


####################################################
### BUILD LINEAR REGRESSION AND EMULATOR MODELS
####################################################

Impl <- Impl.Mast.Jul

##### LINEAR REGRESSION
set.seed(5879, kind = "default")
train <- sample(1:1000, 750)
cross <- sample((1:1000)[-train], 150)
test <- (1:1000)[-c(train, cross)]
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
# July, Master: c(1,3,4,6)
# July, Kitchen: c(1,6,8)
Active.Inputs <- c(1,3,4,6)
Train.ActInp <- Design[train, Active.Inputs, drop=F]         # design points used to train the emulator
Train.regr <- cbind(1, Interactions[train, regr, drop=F])   # regressors: add a column of 1s for intercept
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
s2.tot0 <- var(fit$residuals)        # Prior cumulative variance of homoschedastic Gaussian process
nug_frac0 <- 0.05                          # Fraction of residual variability not explained by regressors
kernel <- 'exp2'

Opt.pars <- Find.Optimal.Parameters(Train.ActInp, Train.regr,
                                    Cross.ActInp, Cross.regr,
                                    y.train, y.cross, 
                                    kernel, d0, s2.tot0, nug_frac0)
sig2.Mast.Jun <- Opt.pars$s2
nu2.Mast.Jun <- Opt.pars$nu2
d.Mast.Jun <- Opt.pars$d

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

sum(dnorm(y.cross, mean = res.cross[,1], sd = sqrt(res.cross[,2]), log = T))/length(y.cross)
sum(dnorm(y.test, mean = res.test[,1], sd = sqrt(res.test[,2]), log = T))/length(y.test)

View(cbind(y.test, res.test[,1], sqrt(res.test[,2])))

hist( (y.test-res.test[,1])/sqrt(res.test[,2]), breaks = 30)
hist(sqrt(res.test[,2]), breaks = 30)
hist( y.test-res.test[,1]  , breaks = 30)

save(sig2.Mast.Jul, nu2.Mast.Jul, d.Mast.Jul,
     sig2.Kitch.Jul, nu2.Kitch.Jul, d.Kitch.Jul,
     sig2.Mast.Jun, nu2.Mast.Jun, d.Mast.Jun,
     file = "RData/Opt_Params_MaxSummerMonths.RData")


##############################################
# PREDICT OVER LOTS OF POINTS

load('RData/Results_First_Million.RData') # loads res1
load('RData/Test_Inputs.RData') # loads Test.points.full

Test.points <- Test.points.full[1:1.e4, ]
All.regr <- predict(Interactions, newdata = Test.points)
Testall.ActInp <- Test.points[, Active.Inputs]
Testall.regr <- cbind(1, All.regr[,regr])

sig2 <- sig2.Kitch.Jun
nu2 <- nu2.Kitch.Jun
d <- d.Kitch.Jun

sig2 <- sig2.Mast.Jul
nu2 <- nu2.Mast.Jul
d <- d.Mast.Jul

Emul.Mast.Jul <- BL.Emul(Train.ActInp, Testall.ActInp, y.train, 
                   Regress.Design = Train.regr, 
                   Regress.Test = Testall.regr, 
                   beta = beta, Cov.beta = Cov.beta, 
                   sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)

res <- Emul.Mast.Jul
res <- Emul.Kitch.Jul
res <- Emul.Kitch.Jun
res <- Emul.Mast.Jun

ind <-(res[,1]-3*sqrt(res[,2]))<9
sum(ind)/length(ind)
sum(indM&indK)/length(ind)


plot(Test.points[ind,8], Test.points[ind,6],
     cex =0.6, pch = 20, col='red',
     xlim = c(-1,1), ylim = c(-1,1)
)
points(Test.points[ind,1], Test.points[ind,6],
     cex =0.4, pch = 20, col='blue',
     xlim = c(-1,1), ylim = c(-1,1)
)



