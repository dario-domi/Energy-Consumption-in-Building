setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')

library(openxlsx)
library(leaps)
library(fields)
source('../../Emulation.R')


################################################
###### LOAD SEVERAL FUNCTIONS AND VARIABLES
################################################

# Time series of simulated and observed temperatures
load('RData/Inputs/Simulated_and_Observed_Temperatures.RData')

# Optimised emulation parameters + Train-cross-test sets
#load('RData/Opt_Params_MaxSummerMonths.RData')

# Implausibility Measures for Max Summer months
load('RData/Inputs/Implausibilities.RData')

# Emulated Implausibilities
load('RData/Results_Temperature.RData')

##########  Computing daily maxima

X <- Mast.Sim
C <- array(X, dim = c(24, 365, 1000))
Max.Mast.Sim <- apply(C, c(2,3), max)
X <- array(Mast.Obs, dim = c(24,365))
Max.Mast.Obs <- apply(X, 2, max)

X <- Kitch.Sim
C <- array(X, dim = c(24, 365, 1000))
Max.Kitch.Sim <- apply(C, c(2,3), max)
X <- array(Kitch.Obs, dim = c(24,365))
Max.Kitch.Obs <- apply(X, 2, max)

rm(C,X)

##############################################################################
## NON-IMPLAUSIBLE REGIONS

load('RData/Test_Inputs.RData') # loads Test.points.full
subs <- 1:1.e6
Eval.points <- Test.points.full[subs,]
rm(Test.points.full)

ind.M.Jun <- (Emul.Mast.Jun[subs,1] - 3*sqrt(Emul.Mast.Jun[subs,2]))<9
ind.K.Jun <- (Emul.Kitch.Jun[subs,1] - 3*sqrt(Emul.Kitch.Jun[subs,2]))<9
ind.M.Jul <- (Emul.Mast.Jul[subs,1] - 3*sqrt(Emul.Mast.Jul[subs,2]))<9
ind.K.Jul <- (Emul.Kitch.Jul[subs,1] - 3*sqrt(Emul.Kitch.Jul[subs,2]))<9
ind.M.Aug <- (Emul.Mast.Aug[subs,1] - 3*sqrt(Emul.Mast.Aug[subs,2]))<9
ind.K.Aug <- (Emul.Kitch.Aug[subs,1] - 3*sqrt(Emul.Kitch.Aug[subs,2]))<9

ind <- ind.M.Jun & ind.M.Jul & ind.M.Aug & ind.K.Jun & ind.K.Jul & ind.K.Aug

c1 <- 8
c2 <- 6
plot(Eval.points[ind,c1], Eval.points[ind,c2],
     cex =0.2, pch = 20, col='blue', xlim = c(-1,1), ylim = c(-1,1) )

compat.Sep <- Y[subs,6]
cat('Non-implausible space: ', 100*sum(compat.Sep)/length(subs), '%', sep = '')


points(Test.points.full[compat.gas,c1], Test.points.full[compat.gas,c2],
       cex =0.2, pch = 20, col='red', xlim = c(-1,1), ylim = c(-1,1) )

points(Eval.points[compat.Sep,c1], Eval.points[compat.Sep,c2],
       cex =0.2, pch = 20, col='red', xlim = c(-1,1), ylim = c(-1,1) )

length(compat.gas)
sum(ind & compat.gas[subs])


library("plot3D")
scatter2D(Eval.points[subs,8], Eval.points[subs,6], colvar = Emul.Kitch.Jul[subs], 
          pch=20, cex=0.2)


###############################################################################
###   JANUARY  ANALYSIS    ##################

### COMPUTE JAN IMPLAUSIBILITY ON SIMULATOR RUNS
times <- 1:31 # January
times.hours <- (times-1)*24+1

V.Obs <- 0.01 * Corr.fun(times.hours, times.hours, d = 12, string = 'exp2')
V.MD <- 0.16 * Corr.fun(times.hours, times.hours, d = 96, string = 'exp2')
V.tot <- V.Obs+V.MD

x <- Max.Mast.Sim[times, ] - Max.Mast.Obs[times] # t x 1000
Impl.Mast.Jan <- diag( t(x)%*%solve(V.tot, x) )/length(times)
x <- Max.Kitch.Sim[times, ] - Max.Kitch.Obs[times]
Impl.Kitch.Jan <- diag( t(x)%*%solve(V.tot, x) )/length(times)

#######################################################################
# PLOTS OF FULL TIMES SERIES WITH DAILY-MAX IMPLAUSIBILITY MEASURE

times <- 1:196     # Jan
Obs <- Mast.Obs[times]
Sim <- Mast.Sim[times,]
Impl <- Impl.Mast.Jan
x.times <- DateTimes[times]

while (T) {
  i <- sample(1000, 1)
  plot(x.times, Obs, ty='l', col='red', xaxt='n', ylim=c(13,22), lwd=1)
  axis.POSIXct(side=1, x= x.times, format = "%d %b")      # format of x tick-labels
  imp <- sqrt(Impl[i])
  if (imp < 3)
    lines(x.times, Sim[,i], ty='l', col='blue')
  else
    lines(x.times, Sim[,i], ty='l', col=rgb(0,0,1,0.6), lty=2)
  legend('topright', legend = c(paste('Impl:', format(imp, digits = 3))) )
  print(i)
  Sys.sleep(1.5)
}

#########################
# Try i=580, i=983, 193 #
#########################


### JAN IMPLAUSIBILITY ON SIMULATOR RUNS
times <- 1:31 # January
times.hours <- (times-1)*24+1

Obs <- Max.Mast.Obs[times]
Sim <- Max.Mast.Sim[times,]
x.times <- DateTimes[times.hours]
Impl <- Impl.Mast.Jan

while (T) {
  i <- sample(1000, 1)
  plot(x.times, Obs, ty='l', col='red', xaxt='n', ylim=c(15,22), lwd=1)
  axis.POSIXct(side=1, x=DateTimes[times], format = "%d %b")      # format of x tick-labels
  imp <- sqrt(Impl[i])
  if (imp < 3)
    lines(x.times, Sim[,i], ty='l', col='blue')
  else
    lines(x.times, Sim[,i], ty='l', col=rgb(0,0,1,0.6), lty=2)
  legend('topright', legend = c(paste('Impl:', format(imp, digits = 3))) )
  print(i)
  Sys.sleep(1.5)
}

# Check directly values of implausibilities
x <- Obs - rep(18.5, 31)
x <- rep(1, 31)

V.Obs <- 0.01 * Corr.fun(times.hours, times.hours, d = 12, string = 'exp2')
V.MD <- 0.16 * Corr.fun(times.hours, times.hours, d = 96, string = 'exp2')
V.tot <- V.Obs+V.MD

sqrt(t(x)%*%solve(V.tot, x)/length(times))


##############################################################################

##############################################################################
## IMPLAUSIBILITY ON FULL TIMES SERIES

times <- 1:196     # Jan
Obs <- Mast.Obs[times]
Sim <- Mast.Sim[times,]

V.Obs <- 0.01 * Corr.fun(times, times, d = 12, string = 'exp2')
V.MD <- 0.16 * Corr.fun(times, times, d = 96, string = 'exp2')
V.tot <- V.Obs+V.MD + 0.001*diag(length(times))

x <- Max.Mast.Sim[times, ] - Max.Mast.Obs[times] # t x 1000
Impl.Mast.Jan <- diag( t(x)%*%solve(V.tot, x) )/length(times)
x <- Max.Kitch.Sim[times, ] - Max.Kitch.Obs[times]
Impl.Kitch.Jan <- diag( t(x)%*%solve(V.tot, x) )/length(times)

val <- eigen(V.tot, symmetric = T, only.values = T)
View(val$values)

image.plot(V.tot)

hist(sqrt(Impl.Kitch.Jan), breaks = 20)


##############################################################################


times= seq(0, 10, 1) # 0,1,2,3,...,10
x <- rep(1, length(times))
V <- Corr.fun(times, times, d = 2, string = 'exp2')
sum(solve(V))/length(x)
t(x)%*%solve(V, x)/length(x)


times= seq(0, 10, 2) # 0,2,4,6,8,10
x <- rep(1, length(times))
V <- Corr.fun(times, times, d = 2, string = 'exp2')
sum(solve(V))
t(x)%*%solve(V, x)/length(x)

times= c(0, 1, 3, 5, 8, 10)
x <- rep(1, length(times))
V <- Corr.fun(times, times, d = 2, string = 'exp2')
sum(solve(V))/length(x)
t(x)%*%solve(V, x)/length(x)


###############################################################################
# IMPLAUSIBILITY AS FUNCTION OF CORRELATION LENGTHS

times <- 182:212 # July
times.hours <- (times-1)*24+1

d.vec <- exp( seq(log(4), log(300), length.out = 100) )
L <- length(d.vec)

Impl.Mast.Jul.Full <- matrix(nrow = 1000, ncol = L)
Impl.Kitch.Jul.Full <- matrix(nrow = 1000, ncol = L)

for (i in 1:L){
  d <- d.vec[i]
  V.Obs <- 0.01 * Corr.fun(times.hours, times.hours, d = 12, string = 'exp2')
  V.MD <- 0.16 * Corr.fun(times.hours, times.hours, d = d, string = 'exp2')
  V.tot <- V.Obs+V.MD
  
  x <- Max.Mast.Sim[times, ] - Max.Mast.Obs[times] # t x 1000
  Impl.Mast.Jul.Full[,i] <- diag( t(x)%*%solve(V.tot, x) )/length(times)
  x <- Max.Kitch.Sim[times, ] - Max.Kitch.Obs[times]
  Impl.Kitch.Jul.Full[,i] <- diag( t(x)%*%solve(V.tot, x) )/length(times)
}

Impl <- Impl.Kitch.Jul.Full
i=487
while (T){
  i <- sample(1000, 1)
  plot(d.vec, Impl[i,], ty='l', col='red')
  Sys.sleep(1.3)
}

  





















