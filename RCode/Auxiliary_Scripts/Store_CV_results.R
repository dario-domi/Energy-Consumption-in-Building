# This program builds the emulators for the simulated gas consumputions (at different months)
# and produces plots of the cross-validated residuals


# LOAD LIBRARIES AND CUSTOM FUNCTIONS

library(leaps)
library(plot3D)
source("../../../Various_Functions.R")
source("../../../emulation.R")

####################################################################
##  LOAD THE DATA: SIMULATION INPUTS AND OUTPUTS, AND OBSERVATIONS  
####################################################################

month.names <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")

# INPUTS (1000 x 8)
Temp <- read.csv("../../Data/inputs-batch2.csv")
Inputs_2 <- Temp[, -1]             # delete first column with numbers from 1 to 1000
var.names <- colnames(Inputs_2)    # store names of input variables

# OUTPUTS (1000 x 12)
Temp <- read.csv("../../Data/results-batch2.csv", skip = 3008, nrow=1001)
Outputs_Gas_2 <- Temp[-1,]             # remove baseline simulation
Outputs_Gas_2[,"File.Name"] <- NULL    # remove first column with file name of runs
rownames(Outputs_Gas_2) <- 1:1000      # name the simulations by numbers
colnames(Outputs_Gas_2) <- month.names # assign month names
rm(Temp)

# OBSERVATIONS (1 x 12)
Obs_Gas <- matrix(c(3201, 2130, 1687, 1452, 1109, 628, 588, 520, 563, 1010, 1985, 2564), ncol = 12)  
colnames(Obs_Gas) <- month.names                                  # name the column by months


###########################################
##  RESCALE INPUTS LINEARLY WITHIN [-1,1]  
###########################################

# Set the ranges...
N_var<-8
input_ranges<-matrix(NA, nrow=2, ncol=N_var)

input_ranges[1,1]<-17.5     # Heating setpoint [Celcius degrees]
input_ranges[2,1]<-20.5

input_ranges[1,2]<-0.6      # Gas boiler seasonal efficiency
input_ranges[2,2]<-0.75

input_ranges[1,3]<-0.04     # External wall thickness [m]
input_ranges[2,3]<-0.063

input_ranges[1,4]<-0.15     # ~Roof thickness [m]
input_ranges[2,4]<-0.21

input_ranges[1,5]<-0.045    # ~Floor thickness [m] 
input_ranges[2,5]<-0.055

input_ranges[1,6]<-0.2      # Infiltration [ac/h]
input_ranges[2,6]<-0.95 

input_ranges[1,7]<-6.15e-06 # DHW consumption [litre/day]
input_ranges[2,7]<-2.20e-05

input_ranges[1,8]<-1.05     # Cooking [W/m2]
input_ranges[2,8]<-6.3

# ... and rescale the input variables within these
for (i in 1:N_var){
  a <- input_ranges[1,i]
  b <- input_ranges[2,i]
  x <- Inputs_2[,i]
  Inputs_2[,i] <- 2*(x-a)/(b-a) -1
}
rm(input_ranges)


################################################################
# SET GENERAL CONSTANTS WHICH WILL BE USED IN LATER EMULATION
################################################################

N_var <- 8
n <- dim(Inputs_2)[1]

GP_inputs <- res <- sapply(month.names, function(x) NULL)
GP_inputs[[1]] <- c(1,2,3,4,6)
GP_inputs[[2]] <- c(1,2,3,4,6)
GP_inputs[[3]] <- c(1,2,3,4,6,7)
GP_inputs[[4]] <- c(1,2,3,4,6)
GP_inputs[[5]] <- c(1,2,3,6,8)
GP_inputs[[9]] <- c(1,2,3,4,6,8)
GP_inputs[[10]] <- c(1,2,3,4,6,8)
GP_inputs[[11]] <- c(1,2,3,4,6,8)
GP_inputs[[12]] <- c(1,2,3,4,6)

Corr_length <- numeric(length = 12)
Corr_length[1] <- 0.35
Corr_length[2] <- 0.3
Corr_length[3] <- 0.35
Corr_length[4] <- 0.3
Corr_length[5] <- 0.4
Corr_length[9] <- 0.4
Corr_length[10] <- 0.45
Corr_length[11] <- 0.5
Corr_length[12] <- 0.35

############################
#  START CROSS-VALIDATION
############################

# Build a matrix of all 2-way interactions of factors (orthogonal polynomials of order 2) for design points
Interactions <- poly(data.matrix(Inputs_2), degree=2)

# List where results of CV will be stored
res <- sapply(month.names, function(x) NULL)

for (month in c(2:4, 9:12)){
  y <- Outputs_Gas_2[, month]
  ## The next line carries out lm's with different number of covariates, selecting best model of each size
  ## (for fixed size, best model is the same regardless of criterion used - adjr2, Cp etc)
  L <- summary(regsubsets(y~., data=Interactions, method = "exhaustive", nvmax = 10))
  ind <- which.max(L$adjr2)          # index (between 1 and nvmax) of model with max adj-R2
  regr <- L$which[ind,-1]            # logical vector with regressors corresponding to selected model
  fit <- lm(y~ ., data = as.data.frame(Interactions[,regr]))
  summary(fit)
  
  # Mean and Variance of coefficients used in prior mean
  beta <- fit$coefficients
  Cov.beta <- vcov(fit)
  
  # Which of the 8 variables to use to build GP
  GP_variables <- GP_inputs[[month]]
  Design.points <- Inputs_2[, GP_variables, drop=F]       # design points used to train the emulator
  Design.regr <- cbind(1, Interactions[, regr, drop=F])   # regressors: add a column of 1s for intercept
  
  N_var_GP <- length(GP_variables)
  d <- Corr_length[month]* replicate(N_var_GP, 1)        # Correlation lengths
  sigma2.tot <- var(fit$residuals)                       # Prior cumulative variance of homoschedastic Gaussian process
  nugget <- 0.05                                         # Fraction of residual variability not explained by regressors
  nu <- nugget*sigma2.tot                                # Variance of nugget term (Gaussian noise)
  sig2 <- (1-nugget)*sigma2.tot                          # Variance of Gaussian process
  
  # Call the function which carries out emulation, on the selected regressors and on output y
  res[[month]] <- Cross_Val(Design.points, Design.regr, y, beta, Cov.beta, d, nu, sig2, 'exp2')
  
  cat(sprintf("%s completed.\n", month.names[month]))
}

####################################
##   THIS IS SPECIFIC FOR MAY     ##
####################################

n <- dim(Inputs_2)[1]
# Build matrices of all 2- and 3-way interactions of factors (orthogonal polynomials of order 2 or 3)
Interactions2 <- poly(data.matrix(Inputs_2), degree=2)
Interactions3 <- poly(data.matrix(Inputs_2), degree=3)

x <- unname( colnames(Interactions3) )
list_index <- NULL
for (i in 1:length(x)){
  x1 <- as.double( substr(x[i], 1,1) )
  x6 <- as.double( substr(x[i], 11,11) )
  if (x1>0 | x6>0){
    s<-0
    for (j in 1:8){
      s <- s + as.double( substr(x[i], 2*j-1, 2*j-1) )
    }
    if (s>2.5){
      list_index <- c(list_index,i)
    }
  } # end if
} # end for
#X <- cbind(Interactions2, Interactions3[, list_index])

i <- c(78, 83)
X <- cbind(Interactions2, Interactions3[, i])
View(X)

# Linear regression with 3rd order terms
month<-5

y <- Outputs_Gas_2[, month]
L <- summary(regsubsets(y~., data=as.data.frame(X), method = "exhaustive", nvmax = 11))
ind <- which.max(L$adjr2)          # index (between 1 and nvmax) of model with max adj-R2
regr <- L$which[ind,-1]            # logical vector with regressors corresponding to selected model
names(regr[regr==T])
fit <- lm(y~., data = as.data.frame(X[,regr]))

# Mean and Variance of coefficients used in prior mean
beta <- fit$coefficients
Cov.beta <- vcov(fit)

GP_variables <- GP_inputs[[month]]
N_var_GP <- length(GP_variables)
Design.points <- Inputs_2[, GP_variables]      # design points used to train the emulator
Design.regr <- cbind(1, X[,regr, drop=F])   # regressors: add a column of 1s for intercept

d <- Corr_length[month] * replicate(N_var_GP, 1)                          # Correlation lengths
sigma2.tot <- var(fit$residuals)                       # Prior cumulative variance of homoschedastic Gaussian process
nugget <- 0.05                                         # Fraction of residual variability not explained by regressors
nu <- nugget*sigma2.tot                                # Variance of nugget term (Gaussian noise)
sig2 <- (1-nugget)*sigma2.tot                          # Variance of Gaussian process

res[[month]] <- Cross_Val(Design.points, Design.regr, y, beta, Cov.beta, d, nu, sig2, 'exp2')


#####################
## EXAMPLE OF PLOTS
#####################

Y <- res[[month]]

hist( (Y$Mean -y) , breaks = 40, freq = F)#, xlim = c(-20,20))
hist( sqrt(Y$Variance), breaks=30, freq = F)

err <- (Y$Mean -y)/sqrt(Y$Variance)
hist( err , breaks = 40)#, xlim = c(-bound,bound))

plot(Y$Mean, err, ylim = c(-bound,bound))
plot(Y$Mean, Y$Mean -y)
plot(Y$Mean, sqrt(Y$Variance))

for (i in 1:8){
  plot(Inputs_2[,i],  Y$Mean -y)#, ylim = c(-bound, bound) )
  Sys.sleep(2.5)
}


#################################
###  SAVE THE RESULTS ON R FILE
#################################

save(res, Inputs_2, Outputs_Gas_2, file = "CV_results.RData")
