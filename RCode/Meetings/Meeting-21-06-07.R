setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')

library(openxlsx)
library(leaps)
library(fields)
source('../../Emulation.R')

#######################################################################
# PLOTS OF FULL TIMES SERIES WITH DAILY-MAX IMPLAUSIBILITY MEASURE

load('RData/Inputs/Simulated_and_Observed_Temperatures.RData')
load('RData/Inputs/Implausibilities.RData')

times <- 1:196     # Jan
Obs <- Mast.Obs[times]
Sim <- Mast.Sim[times,]
Impl <- Impl.Mast.Jan
x.times <- DateTimes[times]

while (T) {
  i <- sample(1000, 1)
  plot(x.times, Obs, ty='l', col='red', xaxt='n', ylim=c(13,22), lwd=1)
  axis.POSIXct(side=1, x= x.times, format = "%d %b")      # format of x tick-labels
#  imp <- sqrt(Impl[i])
#  if (imp < 3)
    lines(x.times, Sim[,i], ty='l', col='blue')
#  else
  #  lines(x.times, Sim[,i], ty='l', col=rgb(0,0,1,0.6), lty=2)
  legend('topright', legend = c(paste('Impl:', format(imp, digits = 3))) )
  print(i)
  Sys.sleep(1.5)
}

#########################
# Try i=580, i=983, 193 #
#########################


#### PCA on January

times <- 1:743     # Jan
X <- t(Mast.Sim[times,]) # each row is one observation

master.pca <- prcomp(X, center = T)
PC <- master.pca$rotation # each column is a PC
#sd(X%*%(PC.mast[,1]))

M <- master.pca$center
Xbar <- sweep(X, 2, M)  # 1000x743
coefs <- Xbar%*%PC[,1:2] # 1000x2

reconstr <- PC[,1:2]%*%t(coefs) + M # 743x1000

c <- sample(1000,1)
plot(DateTimes[times], PC[,1], ty='l', col='red')

plot(DateTimes[times], X[c,], ty='l', col='red')
lines(DateTimes[times], reconstr[,c], ty='l', col='blue', lty=2)


################################

times <- 1:72

Obs <- Mast.Obs[times]
Sim <- Mast.Sim[times,]
x.times <- DateTimes[times]

while (T) {
  i <- sample(1000, 1)
  plot(x.times, Obs, ty='p', col='red', xaxt='n', ylim=c(13,22), lwd=1)
  axis.POSIXct(side=1, x= x.times, format = "%d %b")      # format of x tick-labels
  lines(x.times, Sim[,i], ty='p', col='blue')
  Sys.sleep(1.5)
}









Interactions <- poly(as.matrix(Design), degree=2)
y <- coefs[,2]
L <- summary(regsubsets(y~., data=Interactions, method = "exhaustive", nvmax = 10))
which(regr <- L$which[1,-1])            # logical vector with regressors corresponding to selected model
fit <- lm(y~ ., data = as.data.frame(Interactions[,regr]))
summary(fit)






plot(DateTimes[times], PC[,1]-0.0369, ty='l')
lines(DateTimes[times], 0.07*PC[,2], ty='l', col='red')
abline(h=0, lty=2, col='red')





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

  





















