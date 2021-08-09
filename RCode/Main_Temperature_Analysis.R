################################################################################
#
# The present - heavily commented! - script guides the reader through an ordered 
# sequence of steps, to analyse the original dataset of temperature records, and
# to carry out emulation, history matching, etc on it. For each of two rooms
# (Master and Kitchen), the dataset consists of 1 observed and 1000 simulated 
# temperature time series, taken at hourly rate during a whole year (for a total
# of 24x365=8760 elements in each time series).
#
# The script is divided into several sections, to carry out different tasks. It
# relies on data loaded from previously stored .RData files, containing the 
# results of some code run on the original dataset: optimal emulators 
# hyperparameters, results of emulation on a large sequence of inputs, etc.
#
# Both the .RData files and the code to generate them are contained in the
# 'RData' folder.
#
################################################################################


#############################################################
## SET FOLDER AND LOAD LIBRARIES

setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')

library(openxlsx)
library(leaps)
library(fields)
source('../../Emulation.R')

#############################################################
# LOAD OBSERVED AND SIMULATED TEMPERATURES (and Design points)

# Two rooms, Master and Kitchen: 1 observed and 1000 simulated time series
load('RData/Inputs/Simulated_and_Observed_Temperatures.RData')


###################################################
### PLOTS OF OBSERVED AND SIMULATED TRAJECTORIES

Sim <- Kitch.Sim
Obs <- Kitch.Obs

# Plots of observed and simulated time-series (overlapped)
hours <- 3632:7295 # Jun-Oct
hours <- 2879:6550 # May-Sep
hours <- 1:length(DateTimes)
while (T){
  i <- sample(1000, 1)
  plot(DateTimes[hours], Obs[hours], ty='l', col=rgb(0,0,1, 0.6), 
       xaxt = 'n', ylim=c(13, 28), lwd =1)
  axis.POSIXct(side=1, x=DateTimes[hours], format = "%d %b")       # format of x tick-labels
  lines(DateTimes[hours], Sim[hours,i], ty='l', col=rgb(1,0,0, 0.6))
  legend('topright', legend = i)
  Sys.sleep(1)
}


#################################################
##  COMPUTE DAILY MAXIMA (throughout the year)

X <- Mast.Sim                           # 8760x1000
C <- array(X, dim = c(24, 365, 1000))   # 24x365x1000
Max.Mast.Sim <- apply(C, c(2,3), max)   # 365x1000
X <- array(Mast.Obs, dim = c(24,365))   # 24x365
Max.Mast.Obs <- apply(X, 2, max)        # 365

X <- Kitch.Sim
C <- array(X, dim = c(24, 365, 1000))
Max.Kitch.Sim <- apply(C, c(2,3), max)
X <- array(Kitch.Obs, dim = c(24,365))
Max.Kitch.Obs <- apply(X, 2, max)

rm(C,X)

# AND LOAD ASSOCIATED SUMMER IMPLAUSIBILITIES
load('RData/Inputs/Implausibilities.RData')


########################################################################
### PLOT OBSERVED AND SIMULATED SUMMER DAILY MAX, + IMPLAUSIBILITIES

days <- 152:181 # June
days <- 182:212 # July
days <- 213:243 # August

hours <- (days-1)*24+1

Obs <- Max.Kitch.Obs
Sim <- Max.Kitch.Sim
Impl <- Impl.Kitch.Jun

Obs <- Max.Mast.Obs
Sim <- Max.Mast.Sim
Impl <- Impl.Mast.Jun

while (T) {
  i <- sample(1000, 1)
  plot(DateTimes[hours], Obs[days], ty='l', col='red', 
       xaxt='n', ylim=c(16,26), lwd=1)
  axis.POSIXct(side=1, x=DateTimes[hours], format = "%d %b")       # format of x tick-labels
  imp <- sqrt(Impl[i])
  if (imp < 3)
    lines(DateTimes[hours], Sim[days,i], ty='l', col='blue')
  else
    lines(DateTimes[hours], Sim[days,i], ty='l', col=rgb(0,0,1,0.6), lty=2)
  legend('topright', legend = c(paste('Impl:', format(imp, digits = 3))) )
  print(i)
  Sys.sleep(1.5)
}

## Specific examples to look at
# Master June: compare i=582 and i=270
# Kitch July: criterion seems stricter wrt Master July
# Master June: i=177, i=166: day 24 makes implaus big

# i=966 for kitchen (june)
# compare i=710 and i=490 for kitchen
# i=373 for kitchen
# compare i=242 and i=103


##########################################################################
# PLOT OF "UNCORRELATED" DIFFERENCE BETWEEN SIMULATIONS AND OBSERVATIONS

days <- 152:181 # June
days <- 182:212 # July
days <- 213:243 # August

hours <- (times-1)*24+1

V.Obs <- 0.01 * Corr.fun(hours, hours, d = 12, string = 'exp2')
V.MD <-  0.16 * Corr.fun(hours, hours, d = 96, string = 'exp2')
V.tot <- V.Obs+V.MD

U <- chol(V.tot) # V.tot = t(U)%*%U
t <- length(days)
x <- Max.Kitch.Sim[days, i] - Max.Kitch.Obs[days] # t x 1000
x <- Max.Mast.Sim[days, i] - Max.Mast.Obs[days] # t x 1000
y <- solve(t(U), x) 
sqrt(y%*%y/t)
plot(y, col='red')
abline(h=0, lty=2)
points(x, col='blue')


################################################################################
# COMPARE NOT-RULED-OUT-YET (NROY) SPACE FOR JUN, JUL, AUG, ON 1000 SIMULATIONS

Impl <- sqrt(Impl.Mast.Jun)
ind.Jun <- Impl<3
Impl <- sqrt(Impl.Mast.Jul)
ind.Jul <- Impl<3
Impl <- sqrt(Impl.Mast.Aug)
ind.Aug <- Impl<3
cat('Percentage of non-implausible runs, June: ', 100*sum(ind.Jun)/length(Impl), '%\n')
cat('Percentage of non-implausible runs, July: ', 100*sum(ind.Jul)/length(Impl), '%\n')
cat('Percentage of non-implausible runs, August: ', 100*sum(ind.Aug)/length(Impl), '%\n')
cat('Percentage of non-implausible runs, July AND August: ', 
    100*sum(ind.Jul & ind.Aug)/length(Impl), '%\n')
cat('Non-impl for July ==> Non-impl for August.\n')



plot(Design[ind.Jun,6], Design[ind.Jun,1], col='red', pch=20, cex=1.5,
     xlim = c(-1,1), ylim = c(-1,1))
points(Design[ind.Jul,6], Design[ind.Jul,3], col='blue', pch=21, cex=1.5, lwd=2)
points(Design[ind.Aug,6], Design[ind.Aug,3], col='darkgreen', pch=20, cex=1)


#############################################################################
### HISTORY MATCHING

# The file 'RData/Generate_RData/Implaus_SummerMax_Emulators.R' builds emulators
# of the implausibilities for daily max during summer months, and stores the 
# results in Implaus_SummerMax_Emulators.RData. 
# In this section, we load these and the associated design points, and look 
# at the corresponding non-implausible regions.


# LOAD EVALUATION POINTS AND ASSOCIATED PREDICTIONS
load('RData/Results/Eval_Inputs.RData') # loads Eval.points.full
load('RData/Results/Implaus_SummerMax_Emulators.RData')

N <- dim(Emul.Kitch.Aug)[1]
cat('Number of emulated points:', N)

# Non-implausible inputs, separately for each month and room
ind.M.Jun <- (Emul.Mast.Jun[,1] - 3*sqrt(Emul.Mast.Jun[,2]))<9
ind.K.Jun <- (Emul.Kitch.Jun[,1] - 3*sqrt(Emul.Kitch.Jun[,2]))<9
ind.M.Jul <- (Emul.Mast.Jul[,1] - 3*sqrt(Emul.Mast.Jul[,2]))<9
ind.K.Jul <- (Emul.Kitch.Jul[,1] - 3*sqrt(Emul.Kitch.Jul[,2]))<9
ind.M.Aug <- (Emul.Mast.Aug[,1] - 3*sqrt(Emul.Mast.Aug[,2]))<9
ind.K.Aug <- (Emul.Kitch.Aug[,1] - 3*sqrt(Emul.Kitch.Aug[,2]))<9

# Points whose squared implausibility is (most likely) lower than 9 for all
# three summer months and both rooms
ind.tot <- ind.M.Jun & ind.M.Jul & ind.M.Aug & ind.K.Jun & ind.K.Jul & ind.K.Aug
cat('Number of overall non-implausible points: ', sum(ind), 'out of', length(ind))

# Subselect a random sample if needed
Eval.points <- Eval.points.full[1:N,]
rm(Eval.points.full)
# Random sample of non-implausible indices
ind <- sample(which(ind.tot), 2.e4)

# 2D SCATTER PLOT OF NON-IMPLAUSIBLE REGION
plot(Eval.points[ind,6], Eval.points[ind,8],
     cex =0.2, pch = 20, col='blue',
     xlim = c(-1,1), ylim = c(-1,1)
)

# 2D PLOT OF IMPLAUSIBILITY MEASURE
library("plot3D")
subs <- 1:1.e5
scatter2D(Eval.points[subs,8], Eval.points[subs,6], colvar = Emul.Kitch.Jul[subs], 
          pch=20, cex=0.2)



################################################################################
## ONE EXAMPLE OF BUILDING AN EMULATOR (JULY, MASTER) ON SEVERAL POINTS

# Load design points
load('RData/Inputs/Design_Points.RData')

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


################################################################################
## COMPARE EMULATOR AND LINEAR REGRESSION ON TEST POINTS (example for Kitch.Aug)

Impl <- Impl.Kitch.Aug
y.train <- Impl[train]
y.test <- Impl[test]

## LINEAR REGRESSION TO SELECT REGRESSORS
#  (see RData/Generate_RData/Implaus_SummerMax_OptPars.R for details)
Interactions <- poly(as.matrix(Design[,]), degree=4)
L <- summary(regsubsets(y.train~., data=as.data.frame(Interactions[train,]), 
                        method = "forward", nvmax = 15))
which(regr <- L$which[5,-1])            # regr: logical vector with selected regressors
fit <- lm(y.train~ ., data = as.data.frame(Interactions[train, regr]))
summary(fit)

## DEFINE ACTIVE INPUTS AND REGRESSORS FOR THE TEST SET
# The following are the active inputs to choose for each month and room:
# June, Master: c(1,3,6)
# June, Kitchen: c(1,6,8)
# July, Master: c(1,3,4,6)
# July, Kitchen: c(1,6,8)
# Aug, Master: c(1,3,6)
# Aug, Kitch: c(1,6,8)
Active.Inputs <- c(1,6,8)
Train.ActInp <- Design[train, Active.Inputs, drop=F]       # design points used to train the emulator
Train.regr <- cbind(1, Interactions[train, regr, drop=F])  # regressors: add a column of 1s for intercept
Test.ActInp  <- Design[test, Active.Inputs, drop=F]
Test.regr  <- cbind(1, Interactions[test, regr, drop=F])

# MEAN AND COVARIANCE OF REGRESSION COEFFICIENTS
beta <- fit$coefficients
Cov.beta <- vcov(fit)

# OPTIMISED HYPERPARAMETERS
load('RData/Results/Implaus_SummerMax_OptPars.RData')
sig2 <- sig2.Kitch.Aug
nu2  <- nu2.Kitch.Aug
d    <- d.Kitch.Aug

# EMULATION ON TEST SET
res.test <- BL.Emul(Train.ActInp, Test.ActInp, y.train, 
                    Regress.Design = Train.regr, 
                    Regress.Test = Test.regr, 
                    beta = beta, Cov.beta = Cov.beta, 
                    sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)

# COMPARISON PLOT WITH LINEAR REGRESSION

library(psych)

LRpreds <- predict(fit, as.data.frame(Interactions[test, regr]))
n.std <- 3

# Matrix containing emulator mean predictions and +/- 3 std from it
# (the corrections "/sqrt(3)" due to way the error.bars() function handles data
data <- rbind(res.test[,1] + n.std*sqrt(res.test[,2])/sqrt(3),
              res.test[,1],
              res.test[,1] - n.std*sqrt(res.test[,2])/sqrt(3))
# Sort test indices in increasing order of y.test values 
sorted <- sort(y.test, index.return=T)$ix     

# Now plot each emulator prediction +/- 3 std for the test set, the actual test
# values, and the linear regression prediction
i <- sorted[51:75]
L <- length(i)
col.em <- rgb(1, 0.4, 0.4)
  # 1) Emulator predictions shown as cat eyes plot
error.bars(data[,i], alpha = 0.0954659663, eyes=T, col=col.em,
           main = 'Emulator and regression predictions',
           xlab = 'Simulation Index',
           ylab = 'Implausibility Measure'
)
  # 2) Add real y.test values
points(1:L, y.test[i], pch=8, cex=1.3, col='blue')
  # 3) Add linear regression predictions
points(1:L, LRpreds[i], pch=23, col = 'black', bg = 'yellow', lwd=1.9, cex = 1.3)
legend('topleft', legend = c('Simulator', 'Emulator', 'Linear regression'), 
       pch = c(8, 22, 23), col = c('blue', 'black', 'black'), 
       pt.bg = c(NA, col.em, 'yellow'),  pt.cex= c(1, 1.5, 1.3), 
       bg = 'aliceblue')








