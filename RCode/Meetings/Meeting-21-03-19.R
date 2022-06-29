setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode/')

#library(openxlsx)
#library(leaps)
#source('Scripts/Auxiliary_Functions.R')
source('../../Emulation.R')


#########################################
## COMPATIBILITY WITH GAS CONSUMPTION 
#########################################

# Creates variables with inputs and outputs of gas simulator at design points, + observations
load('RData/Inputs/Design_Points.RData') 

# Load N=1.e6 Test inputs and associated emulated gas predictions
load('RData/Inputs/Simulated_and_Observed_Gas.RData') # to have Gas.Obs
load('RData/Results_Emulation/Eval_Inputs.RData') # loads Eval.points.full
load('RData/Results_Emulation/Gas_Emulation_Results.RData') # loads Emul.Gas
N <- dim(Emul.Gas[[1]])[1]
month.names <- names(Emul.Gas)

# Build matrix of Implausibility Measures for all observations (rows) and all months of interest (cols)
month.indices <- c(1:5, 9:12)
Global_IM <- matrix(nrow = N, ncol = length(month.indices))
colnames(Global_IM) <- month.names[month.indices]

Mod.Discr <- 0.1
Meas.Err <- 0.05
for (i in month.indices){
  month <- month.names[i]
  X <- Emul.Gas[[month]]
  z <- Gas.Obs[, month]
  IM <- Compute_IM( X[,"Mean"], X[,"Var"], z, Mod.Discr, Meas.Err)
  Global_IM[, month] <- IM
  rm(X, IM)
  gc()
}

# IDENTIFY NON-IMPLAUSIBLE INPUTS
Thresh <- 4
Y <- abs(Global_IM)<Thresh
Z <- as.matrix(rowSums(Y))
compat.gas <- (Z> dim(Global_IM)[2]-0.1)

cat('Non-implausible space: ', 100*sum(compat.gas)/N, '%', sep = '')

# Load non-implausible inputs
load('RData/Results_Emulation/Non-Implausible-Inputs.RData')


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
  Z <- as.matrix(rowSums(Y1))
  retained.frac[,m] <- 100 * sum(gas.compat)/sum(Z > dim(Y1)[2] - 0.1)
}

View(retained.frac)


# Look at values of IM for a given month, when "compatibility" is imposed 
# on all other months
for (m in month.indices){
  Y1 <- Y[, !colnames(Y) %in% month.names[m]]
  Z <- as.matrix(rowSums(Y1))
  ind.nomonth <- (Z > dim(Y1)[2] -0.1)
  hist(Global_IM[ind.nomonth, month.names[m]], main = month.names[m])
  Sys.sleep(3)
}
sum(ind.nomonth)

# View predicted emulator values on selected inputs
View(Emul.Gas[[1]][ind.nomonth,])
hist(res1[[1]][ind.nomonth,"Mean"])


# Scatter plot of paired outputs, with observation to match
m1 <- 1
m2 <- 2
ind <- sample(N, 30000)
X <- cbind(Emul.Gas[[m1]][ind, 1], Emul.Gas[[m2]][ind,1])
plot(X[,1], X[,2], pch=20, cex=0.1, asp=1)
points(Gas.Obs[m1], Gas.Obs[m2], pch=4, col = 'red', cex=1.3)


# The function take a numerical vector and a factor, and returns the range
# obtained by expanding the original range of x of a factor fac around its center
expand_range <- function(x, fac){
  rg <- range(x)
  a <- rg[1]
  b <- rg[2]
  d <- (b-a)/2
  A <- a - (fac-1)*d
  B <- b + (fac-1)*d
  return(c(A,B))
}


# Scatter plot of paired outputs only for non-implausible points, plus observation
x <- Emul.Gas[[m1]][gas.compat, 1]
y <- Emul.Gas[[m2]][gas.compat, 1]
xl <- expand_range( c(x,Gas.Obs[m1]), 1.2)
yl <- expand_range( c(y,Gas.Obs[m2]), 1.2)
plot(x, y, pch=20, cex=0.1, asp=1, xlim = xl, ylim = yl)
points(Gas.Obs[m1], Gas.Obs[m2], pch=4, col = 'red', cex=1.3)



################################################################################



###############################################################################
# LOAD OBSERVED AND SIMULATED TEMPERATURES: hourly, for one year (8760 obs)
###############################################################################

file <- "Data/Temperature_Data/Space Temperatures - Originals - For Dario.xlsx"

## DATES AND TIMES
DateTimes <- read.xlsx(file, sheet = 1)[,1]
DateTimes <- as.POSIXct(DateTimes*86400, origin="1899-12-30", tz='Europe/London')

## KITCHEN TEMPERATURES
# Data between 12/01 and 21/01 is not available (indices 264-503)
Kitch.Obs <- read.xlsx(file, sheet = 1) 
Kitch.Obs <- Kitch.Obs[,"Actual.(°C)"]

## MASTER TEMPERATURES
# Data between 12/01 and 21/01 is not available (indices 264-503)
Master.Obs <- read.xlsx(file, sheet = 2) 
Master.Obs <- Master.Obs[,"Actual"]
rm(file)

## LOAD SIMULATED TEMPERATURES
# Kitch.Sim & Master.Sim: 8760 x 1000
load('RData/Simulated-Temperatures.RData')


################################################################
##                       CODE FOR SOME PLOTS
################################################################

# Observed time series
plot(DateTimes, Kitch.Obs, ty='l', col='blue', main='Observed Kitchen Temperature')

# One plot of simulated temperature
i <- 2
plot(DateTimes, Kitch.Sim[,i], ty='l')


# Plots of observed and simulated time-series (overlapped)
times <- 3632:7295 # Jun-Oct
times <- 2879:6550 # May-Sep
times <- 1:length(DateTimes)
while (T){
  i <- ceiling(1000*runif(1))
  plot(DateTimes[times], Kitch.Sim[times,i], ty='l', col='red', 
       ylim = c(12,28))
  lines(DateTimes[times], Kitch.Obs[times], ty='l', col='black')
  legend('topright', legend = i)
  Sys.sleep(0.8)
}

# Zooming
zoom <- 1000:1100
plot(DateTimes[zoom], Kitch.Sim[zoom,i], ty='l', col = 'red', xaxt='n')
axis.POSIXct(side=1, x=DateTimes[zoom], format = "%d %b") # read ?strptime for date format


#################################################################
##                COMPUTE L2 DIFFERENCE
#################################################################

All.regr <- predict(Interactions.Design, newdata = Test.points.full)

# Squared L2-norm of difference between observed and simulated temperatures
times <- 2879:6550 # May-Sep
times <- c(1:2878, 6551:8760)
times <- 1:length(DateTimes)
Diff <- apply((Kitch.Sim[times,] - Kitch.Obs[times])^2, 2, sum, na.rm=TRUE)

# LR with 2nd order terms
y <- Diff
Interactions.Design <- poly(data.matrix(Design), degree=2)
L <- summary(regsubsets(y~., data=Interactions.Design, method = "exhaustive", nvmax = 5))
ind <- which.max(L$adjr2)          # index (between 1 and nvmax) of model with max adj-R2
regr <- L$which[ind,-1]            # logical vector with regressors corresponding to selected model
fit <- lm(y~ ., data = as.data.frame(Interactions.Design[,regr]))
summary(fit)


## PREDICTIONS ON TEST SET

preds.l2 <- predict(fit, newdata = as.data.frame(All.regr[, regr]))
hist(preds.l2, breaks=100)
summary(preds.l2)

compat.l2 <- (preds.l2<53000)
compat.l2 <- (preds.l2<10*1.e3)

# Marginals of input coordinates
hist(Test.points.full[compat.l2,1], breaks=100, xlim = c(-1,1), freq = F)

# 2D marginals (scatter plor)
c1=1
c2=6
plot(Test.points.full[compat.l2,c1], Test.points.full[compat.l2,c2], 
     xlim = c(-1,1), ylim = c(-1,1), cex=0.2,
     xlab = colnames(Design)[c1],
     ylab = colnames(Design)[c2],)

# Add compatibility with monthly gas consumption
points(Test.points.full[compat.gas,c1], Test.points.full[compat.gas,c2], 
     cex=0.4, col='red')
legend('topleft', legend = c('L2', 'Gas'), col=c('black', 'red'), pch = c(20,20))


###############################################################################
##   EXPLAIN FLAT LEVEL OF TIME-SERIES OUTPUTS IN TERMS OF INPUTS
###############################################################################

source('Scripts/Extract_Flat.R')

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
