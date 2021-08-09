setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/')

library(openxlsx)
library(leaps)
source('Scripts/Auxiliary_Functions.R')
source('../Emulation.R')


#########################################
## COMPATIBILITY WITH GAS CONSUMPTION 
#########################################

# Creates variables with inputs and outputs of gas simulator at design points, + observations
source('Scripts/Load_data.R') 
Design <- Rescale.Linearly(Design)

# Load N=1.e6 Test inputs and associated emulated gas predictions
load('RData/Test_Inputs.RData')
load('RData/Results_First_Million.RData') # loads
N <- dim(res1[[1]])[1]

# Build matrix of Implausibility Measures for all observations (rows) and all months of interest (cols)
month.indices <- c(1:5, 9:12)
Global_IM <- matrix(nrow = N, ncol = length(month.indices))
colnames(Global_IM) <- month.names[month.indices]

Mod.Discr <- 0.1
Meas.Err <- 0.05
for (i in month.indices){
  month <- month.names[i]
  X <- res1[[month]]
  z <- Obs_gas[, month]
  IM <- Compute_IM( X[,"Mean"], X[,"Var"], z, Mod.Discr, Meas.Err)
  Global_IM[, month] <- IM
  rm(X, IM)
  gc()
}

# IDENTIFY NON-IMPLAUSIBLE INPUTS
Thresh <- 4
Y <- abs(Global_IM)<Thresh
Z <- as.matrix(rowSums(Y, na.rm = T))
compat.gas <- (Z> dim(Global_IM)[2]-0.1)

cat('Non-implausible space: ', 100*sum(compat.gas)/N, '%', sep = '')


#######################################################
# LOOK AT CORRELATION BETWEEN MONTHLY OUTPUTS
pairs(Outputs_gas[, month.indices])
pairs(Outputs_gas)
View(cor(Outputs_gas[,1], Outputs_gas[,]))


########################################################################
## CONTRIBUTION OF EACH MONTH TO REDUCTION OF NON-IMPLAUSIBLE REGION

# Fraction of further reduced space by introducing any month
retained.frac <- t(sapply(month.names[month.indices], function(i){0}))
for (m in month.names[month.indices]){
  Y1 <- Y[, !colnames(Y) %in% m]
  Z <- as.matrix(rowSums(Y1, na.rm = T))
  retained.frac[,m] <- 100 * sum(compat.gas)/sum(Z>7.9)
}

View(retained.frac)


# Look at values of IM for a given month, when "compatibility" is imposed 
# on all other months
for (m in month.indices){
  Y1 <- Y[, !colnames(Y) %in% month.names[m]]
  Z <- as.matrix(rowSums(Y1, na.rm = T))
  ind.nomonth <- (Z>7.9)
  hist(Global_IM[ind.nomonth, month.names[m]], main = month.names[m])
  Sys.sleep(7)
}
sum(ind.nomonth)

# View predicted emulator values on selected inputs
View(res1[[1]][ind.nomonth,])
hist(res1[[1]][ind.nomonth,"Mean"])


################################################################################



###############################################################################
# LOAD OBSERVED AND SIMULATED TEMPERATURES: hourly, for one year (8760 obs)
###############################################################################

file <- "Data/Temperature_Data/Actual Space Temperatures - Dario.xlsx"
Table <- read.xlsx(file)

## DATES AND TIMES
DateTimes <- Table[,1]
# Convert numbers into actual dates, using as.POSIXct
DateTimes <- as.POSIXct(DateTimes*86400, origin="1899-12-30", tz='Europe/London')

## KITCHEN TEMPERATURES
Kitch.Obs <- Table[, "Kitchen"]

## MASTER TEMPERATURES
Master.Obs <- Table[, "Master"]
rm(file, Table)

## LOAD SIMULATED TEMPERATURES
# Kitch.Sim & Master.Sim: 8760 x 1000
load('RData/Simulated-Temperatures.RData')


################################################################
##                       CODE FOR SOME PLOTS
################################################################

# Observed time series
plot(DateTimes, Kitch.Obs, ty='l', col='blue', main='Observed Kitchen Temperature')
plot(DateTimes, Master.Obs, ty='l', col='blue', main='Observed Kitchen Temperature')

# Zooming
zoom <- 500:700
plot(DateTimes[zoom], Kitch.Obs[zoom], ty='l', col = 'blue', xaxt='n', ylim = c(15,23))
lines(DateTimes[zoom], Kitch.Sim[zoom,2], ty='l', col = 'red')
axis.POSIXct(side=1, x=DateTimes[zoom], format = "%d %b") # read ?strptime for date format


# Plots of observed and simulated time-series (overlapped)
times <- 3632:7295 # Jun-Oct
times <- 2879:6550 # May-Sep
times <- 1:length(DateTimes)
while (T){
  i <- ceiling(1000*runif(1))
  plot(DateTimes[times], Kitch.Obs[times], ty='l', col=rgb(0,0,0, 0.6), ylim=c(13, 28))
  lines(DateTimes[times], Kitch.Sim[times,i], ty='l', col=rgb(1,0,0, 0.6))
  legend('topright', legend = i)
  Sys.sleep(1)
}



#################################################################
##                COMPUTE L2 DIFFERENCE
#################################################################

# Squared L2-norm of difference between observed and simulated temperatures
times <- 2879:6550 # May-Sep
times <- 1:length(DateTimes)
Diff.K <- apply((Kitch.Sim[times,] - Kitch.Obs[times])^2, 2, sum)/length(times)
Diff.M <- apply((Master.Sim[times,] - Master.Obs[times])^2, 2, sum)/length(times)
hist((Diff.M), breaks = 40)


source('Scripts/Load_data.R') 
Design <- Rescale.Linearly(Design)
Interactions.Design <- poly(data.matrix(Design), degree=2)

load('RData/Test_Inputs.RData')
All.regr <- predict(Interactions.Design, newdata = Test.points.full)

# LR with 2nd order terms
y <- Diff.K
L <- summary(regsubsets(y~., data=Interactions.Design, method = "exhaustive", nvmax = 5))
ind <- which.max(L$adjr2)          # index (between 1 and nvmax) of model with max adj-R2
regr <- L$which[ind,-1]            # logical vector with regressors corresponding to selected model
fit <- lm(y~ ., data = as.data.frame(Interactions.Design[,regr]))
summary(fit)

fit.K <- fit
regr.K <- regr

fit.M <- fit
regr.M <- regr

## PREDICTIONS ON TEST SET

preds.l2.K <- predict(fit.K, newdata = as.data.frame(All.regr[, regr.K]))
preds.l2.M <- predict(fit.M, newdata = as.data.frame(All.regr[, regr.M]))

hist(preds.l2.K, breaks=300, xlim = c(0,8))
hist(preds.l2.M, breaks=300, xlim = c(0,8))

compat.l2.M <- (preds.l2.M<0.9)
compat.l2.K <- (preds.l2.K<1.3)
compat.l2 <- compat.l2.K

# Marginals of input coordinates
hist(Test.points.full[compat.l2,6], breaks=100, xlim = c(-1,1), freq = F,
     xlab = colnames(Design)[6])

# 2D marginals for Kitchen
c1=1
c2=6
plot(Test.points.full[compat.l2.K,c1], Test.points.full[compat.l2.K,c2], 
     col = rgb(0,0,1,0.1),
     xlim = c(-1,1), ylim = c(-1,1), cex=0.2,
     xlab = colnames(Design)[c1],
     ylab = colnames(Design)[c2])
legend('topright', legend = c('Kitchen Temp'), col=c('blue'), 
       pch = c(20), cex = 0.9)
# Add same plot for Master room
lines(Test.points.full[compat.l2.M,c1], Test.points.full[compat.l2.M,c2],
      col = rgb(1,0,0, 0.6), cex=0.2)
legend('topright', legend = c('Kitchen Temp', 'Master Temp'), col=c('blue', 'red'), 
       pch = c(20,20), cex = 0.9)


# Add compatibility with monthly gas consumption
points(Test.points.full[compat.gas,c1], Test.points.full[compat.gas,c2], 
     cex=0.4, col=rgb(0,1,0,0.4))
legend('topright', legend = c('Kitchen Temp', 'Master Temp', 'Gas'), col=c('blue', 'red', 'green'), 
       pch = c(20,20, 20), cex = 0.9)


###############################################################################
##   EXPLAIN FLAT LEVEL OF TIME-SERIES OUTPUTS IN TERMS OF INPUTS
###############################################################################


# Max levels of all 1000 simulations 
Flats <- apply(Kitch.Sim, 2, Extract.Flat.Level)

# Run linear regression
fit <- lm(Flats~ heating_setpoint, data = Design)
fit <- lm(Flats~ heating_setpoint * infiltration_ach, data = Design)
summary(fit)

# Input-Output relationship
plot(Design$heating_setpoint, Flats, cex=0.5, pch = 20, col = 'red')

preds.flats <- predict(fit, newdata = as.data.frame(Test.points.full))

# L2 values vs flat levels
ind <- 1:1.e4
plot(preds.l2[ind], preds.flats[ind], cex=0.1)

## CHECK THIS

month<-1
y <- Outputs_gas[, month]
L <- summary(regsubsets(y~., data=Interactions, method = "exhaustive", nvmax = 10))
ind <- which.max(L$adjr2)          # index (between 1 and nvmax) of model with max adj-R2
regr <- L$which[ind,-1]            # logical vector with regressors corresponding to selected model
fit <- lm(y~ ., data = as.data.frame(Interactions[,regr]))
coeff.Jan <- fit$coefficients

C <- rbind(coeff.Jan, coeff.Feb, coeff.Mar, coeff.Apr)

m1=1
m2=2
plot(Outputs_gas[,m1], Outputs_gas[,m2])
lines( c(-1,1)*1.e5, c(-1,1)*1.e5, ty='l', col='red')
