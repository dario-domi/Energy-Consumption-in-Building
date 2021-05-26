setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')

library(openxlsx)
library(leaps)
library(fields)
source('Auxiliary_Scripts/Auxiliary_Functions.R')
source('../../Emulation.R')
source('Auxiliary_Scripts/Find_Optimal_Params.R')

###############################################################################
# LOAD OBSERVED AND SIMULATED TEMPERATURES: hourly, for one year (8760 obs)
###############################################################################

file <- "../Data/Temperature_Data/Actual Space Temperatures - Dario.xlsx"
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
source('Auxiliary_Scripts/Load_data.R') 
Design <- Rescale.Linearly(Design)
rm(Outputs_gas, Obs_gas, month.names, Create.my.list)


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


##################################################
##### SAVE IMPLAUSIBILITIES

save(Impl.Mast.Jun, Impl.Kitch.Jun,
     Impl.Mast.Jul, Impl.Kitch.Jul,
     Impl.Mast.Aug, Impl.Kitch.Aug,
     file = "RData/Summer_Implausibilities.RData")


###################################################
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
Impl <- Impl.Mast.Aug

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
Impl <- sqrt(Impl.Mast.Jun)
ind.Jun <- Impl<3
Impl <- sqrt(Impl.Mast.Jul)
ind.Jul <- Impl<3
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

source('Temperature_Implausibility_Emulator.R')

##############################################
## EVALUATE EMULATORS ON SEVERAL POINTS
##############################################









