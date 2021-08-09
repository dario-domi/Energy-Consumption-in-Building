###############################################################################
#
# For different months, this script extracts temperature daily maxima (observed 
# and simulated) and computes the implausibility of the 1000 simulations.
#
# Implausibility is computed as:
# (x^T)*(V^(-1))*x/n
# where x is n-dim vector of Observed minus Simulated daily maxima, and V is
# the nxn variance matrix of Model Discrepancy + Meas Error. 
#
# Results are saved in RData/Inputs/Implausibilities.RData.
#
###############################################################################

## SET FOLDER, LOAD PACKAGES/FUNCTIONS
setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')
source('../../Emulation.R')

## LOAD OBSERVED AND SIMULATED TEMPERATURES: hourly, for one year (8760 obs)
load('RData/Inputs/Simulated_and_Observed_Temperatures.RData')
rm(Design, Design.Original)

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

###############################################################################
## COMPUTE AND STORE IMPLAUSIBILITIES OF DIFFERENT MONTHS, ON THE 1000 RUNS

### JUNE IMPLAUSIBILITY ON SIMULATOR RUNS
times <- 152:181 # June
times.hours <- (times-1)*24+1

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

V.Obs <- 0.01 * Corr.fun(times.hours, times.hours, d = 12, string = 'exp2')
V.MD <- 0.16 * Corr.fun(times.hours, times.hours, d = 96, string = 'exp2')
V.tot <- V.Obs+V.MD

x <- Max.Mast.Sim[times, ] - Max.Mast.Obs[times] # t x 1000
Impl.Mast.Aug <- diag( t(x)%*%solve(V.tot, x) )/length(times)
x <- Max.Kitch.Sim[times, ] - Max.Kitch.Obs[times]
Impl.Kitch.Aug <- diag( t(x)%*%solve(V.tot, x) )/length(times)


## STORE THE IMPLAUSIBILITIES

save(Impl.Mast.Jun, Impl.Kitch.Jun,
     Impl.Mast.Jul, Impl.Kitch.Jul,
     Impl.Mast.Aug, Impl.Kitch.Aug,
     file = "RData/Inputs/Implausibilities.RData")


###############################################################################
#
# Code for other months, with implausibility possibly computed on full time series. 
# In this case, numerical problems due to size arise.
#
#################################################################################


### JAN DAILY MAX IMPLAUSIBILITY ON SIMULATOR RUNS
times <- 1:31 # January
times.hours <- (times-1)*24+1

V.Obs <- 0.01 * Corr.fun(times.hours, times.hours, d = 12, string = 'exp2')
V.MD <- 0.16 * Corr.fun(times.hours, times.hours, d = 24, string = 'exp2')
V.tot <- V.Obs+V.MD

x <- Max.Mast.Sim[times, ] - Max.Mast.Obs[times] # t x 1000
Impl.Mast.Jan <- diag( t(x)%*%solve(V.tot, x) )/length(times)
x <- Max.Kitch.Sim[times, ] - Max.Kitch.Obs[times]
Impl.Kitch.Jan <- diag( t(x)%*%solve(V.tot, x) )/length(times)

### JAN IMPLAUSIBILITY ON SIMULATOR RUNS (full data)
times <- 1:31 # January
times.hours <- (times-1)*24+1

V.Obs <- 0.01 * Corr.fun(times.hours, times.hours, d = 12, string = 'exp2')
V.MD <- 0.16 * Corr.fun(times.hours, times.hours, d = 96, string = 'exp2')
V.tot <- V.Obs+V.MD

x <- Max.Mast.Sim[times, ] - Max.Mast.Obs[times] # t x 1000
Impl.Mast.Jan <- diag( t(x)%*%solve(V.tot, x) )/length(times)
x <- Max.Kitch.Sim[times, ] - Max.Kitch.Obs[times]
Impl.Kitch.Jan <- diag( t(x)%*%solve(V.tot, x) )/length(times)



