# GENERAL OVERVIEW

# A computer model simulating monthly gas consumption in a private house has been run N=1000 times, for N different
# choices of 8 model parameters. This script emulates the simulator response at any choice of inputs.
# Recall: an emulator predicts the simulator response in the form of a mean prediction and associated uncertainty. 

# The emulator for each month is customised according to which (functions of the) inputs best explain the output.

# Re the choice of inputs, the following possibilities are implemented:
# 1) Inputs taken from a Sobol sequence [-1,1]^8: the emulator predictions for the first 100 Millions of the sequence
#    have been actually stored (see RData/Results_1-30.RData, etc).
# 2) Only inputs between the N1-th and N2-th of the sequence can be selected.
# 3) Trivially, any sequence of points in [-1,1]^8 can be considered as Inputs.

# In all cases, the emulation is carried out in blocks of at most 1million points
# (this is to avoid memory problems, eg when generating all 2-way interactions)

###########################################################################################################

#####################################################
# SET DIRECTORY AND LOAD LIBRARIES/CUSTOM FUNCTIONS
#####################################################

setwd('/Users/Durham/Desktop/Post Doc/Projects/UQ for building energy systems/')
library(leaps)
library(randtoolbox)
library(plot3D)
source("../Emulation.R")
source("Auxiliary_Scripts/Auxiliary_Functions.R")


#####################################################
#  LOAD THE DATA: SIMULATION INPUTS AND OUTPUTS  
#####################################################

month.names <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")

# INPUTS (1000 x 8)
Raw.inp <- read.csv("../Data/inputs-batch2.csv")
Design <- Raw.inp[, -1]          # delete first column with numbers from 1 to 1000

# OUTPUTS (1000 x 12)
Raw.inp <- read.csv("Data/results-batch2.csv", skip = 3008, nrow=1001)
Outputs <- Raw.inp[-1,]             # remove baseline simulation
Outputs[,"File.Name"] <- NULL    # remove first column with file name of runs
rownames(Outputs) <- 1:1000      # name the simulations by numbers
colnames(Outputs) <- month.names # assign month names
rm(Raw.inp)

# RESCALE INPUTS LINEARLY WITHIN [-1,1]
Design <- Rescale.Linearly(Design)


####################################################################
# LOAD SOME EMULATION PARAMETERS AND BUILD NEW SET OF TEST POINTS 
####################################################################

# Define correlation lengths and which GP variables to use: "Corr_lengths" and "GP_inputs"
source("Scripts/Emulation_Parameters.R")

# Emulation will be carried out on the elements from N1 to N2 of Sobol sequence
N1 <- 4800000                   # First and last index of sobol sequence...
N2 <- N1+1000                   # ...on which emulation will be carried out
N <- N2 - N1 + 1                # Total number of test points on which emulation will be carried out

# Generate N random points in the cube [-1,1]^Nvar
N_var <- 8
if (identical(N1,1)) {                                                                   # If N1=1:
  Test.points.global <- 2*sobol(N, dim = N_var, scrambling = 1, seed = 2341) -1          #    take the first N points of the sequence
} else {                                                                                 # Otherwise:
  Test.points.global <- 2*sobol(N1-1, dim = N_var, scrambling = 1, seed = 2341) -1       #    generate the first N1-1 points
  Test.points.global <- 2*sobol(N, dim = N_var, scrambling = 1, seed = 2341, init=F) -1  #    take the ones from N1 to N2
}
gc()       # releases memory
colnames(Test.points.global) <- colnames(Design)

# Alternatively: sample any N points in the cube [-1,1]^8 (uncomment following lines if needed)
#N <- 100
#N_var <- 8
#Test.points.global <- matrix(2*runif(N*N_var)-1, nrow = N)
#colnames(Test.points.global) <- colnames(Design)

# Emulation will be carried out in blocks
Block_size <- 1.e6                 # Maximum number of test points which will be emulated in each block
N_loops <- ceiling(N/Block_size)   # Number of loops subsequently needed

# If variable 'res' already exists, leave it to be able to append to it
#if (!exists("res"))
  res <- Create.my.list(N, month.names, indices = c(1:5, 9:12), val = NA)

# Build a matrix of all 2-way interactions of factors (orthogonal polynomials of order 2) for design points
Interactions.Design <- poly(data.matrix(Design), degree=2)


##########################################################################
# ACTUAL EMULATION: LOOP OVER THE DIFFERENT BLOCKS OF SIZE "Block_size"
##########################################################################

for (block in 1:N_loops){
  
  cat(sprintf("Starting block number %d out of %d:\n", block, N_loops))
  
  ind_start <- (block-1)*Block_size +1   # first index of current block
  ind_end   <- min(block*Block_size, N)  # last index of current block
  index     <- ind_start:ind_end
  
  # Test points in current block
  Test.points.full <- Test.points.global[index, , drop=F]  # Block_size x N_var
  
  # Full set of regressors associated with test points in the block
  Test.regr.full <- predict(Interactions.Design, newdata = Test.points.full)
  
  for (month in c(1:4, 9:12) ){
    
    cat(sprintf("\t %s being computed.\n", month.names[month]))
    
    y <- Outputs[, month]
    ## The next line carries out lm's with different number of covariates, selecting best model of each size
    ## (for fixed size, best model is the same regardless of criterion used - adjr2, Cp etc)
    L <- summary(regsubsets(y~., data=Interactions.Design, method = "exhaustive", nvmax = 10))
    ind <- which.max(L$adjr2)          # index (between 1 and nvmax) of model with max adj-R2
    regr <- L$which[ind,-1]            # logical vector with regressors corresponding to selected model
    fit <- lm(y~ ., data = as.data.frame(Interactions.Design[,regr]))
    
    # Mean and Variance of coefficients used in prior mean
    beta <- fit$coefficients
    Cov.beta <- vcov(fit)
    
    # Which of the 8 variables to use to build GP
    GP_variables <- GP_inputs[[month]]
    Design.points <- Design[, GP_variables, drop=F]         # design points used to train the emulator
    Design.regr <- cbind(1, Interactions.Design[, regr, drop=F])   # regressors: add a column of 1s for intercept
    
    Test.points <- Test.points.full[, GP_variables, drop=F]
    Test.regr <- cbind(1, Test.regr.full[, regr, drop=F])   # add a column of 1 for intercept
    
    N_var_GP <- length(GP_variables)
    d <- Corr_lengths[month]* replicate(N_var_GP, 1)         # Correlation lengths
    sigma2.tot <- var(fit$residuals)                        # Prior cumulative variance of homoschedastic Gaussian process
    nugget <- 0.05                                          # Fraction of residual variability not explained by regressors
    nu2 <- nugget*sigma2.tot                                 # Variance of nugget term (Gaussian noise)
    sig2 <- (1-nugget)*sigma2.tot                           # Variance of Gaussian process
    
    # Call the function which carries out  Bayes Linear Emulation, on the selected regressors given observed output y
    res.block <- BL.Emul(Design.points, Test.points, y, 
                            Regress.Design = Design.regr, 
                            Regress.Test = Test.regr, 
                            beta = beta, Cov.beta = Cov.beta, 
                            sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)

    # Store the results for this block and this month
    res[[month]][index, ] <- res.block
    
    # Release memory (otherwise it won't get released despite using same name for some variables)
    gc()
  }
  
  cat(sprintf("\n"))
  if ( identical(block%%4,0) | (block == N_loops) ){
    cat(sprintf("Saving results for block %d of %d ...\n", block, N_loops))
    #save(res, file = "Results_61-100.RData", version = 2, compress = "xz")
    cat(sprintf("Results saved.\n\n"))
  }
  
}

##########################################
##  THIS REPEATS ALL THE ABOVE, FOR MAY
##########################################

cat(sprintf("\n"))
cat(sprintf("Now Starting May.\n"))

Block_size <- 1.e5                 # Number of test points which will be emulated in each block
N_loops <- ceiling(N/Block_size)   # Number of loops subsequently needed

# Build matrices of all 2- and 3-way interactions of factors (orthogonal polynomials of order 2 or 3)
Interactions2 <- poly(data.matrix(Design), degree=2)
Interactions3 <- poly(data.matrix(Design), degree=3)

# Select all 2-way interactions, plus V1*V6^2 and V6^3
X <- cbind(Interactions2, Interactions3[, c(78, 83)])

month<-5
y <- Outputs[, month]
L <- summary(regsubsets(y~., data=as.data.frame(X), method = "exhaustive", nvmax = 11))
ind <- which.max(L$adjr2)          # index (between 1 and nvmax) of model with max adj-R2
regr <- L$which[ind,-1]            # logical vector with regressors corresponding to selected model
fit <- lm(y~., data = as.data.frame(X[,regr]))

# Mean and Variance of coefficients used in prior mean
beta <- fit$coefficients
Cov.beta <- vcov(fit)

# Which of the 8 variables to use to build GP
GP_variables <- GP_inputs[[month]]
Design.points <- Design[, GP_variables, drop=F]    # design points used to train the emulator
Design.regr <- cbind(1, X[,regr, drop=F])          # regressors: add a column of 1s for intercept

N_var_GP <- length(GP_variables)
d <- Corr_lengths[month] * replicate(N_var_GP, 1)   # Correlation lengths
sigma2.tot <- var(fit$residuals)                   # Prior cumulative variance of homoschedastic Gaussian process
nugget <- 0.05                                     # Fraction of residual variability not explained by regressors
nu2 <- nugget*sigma2.tot                           # Variance of nugget term (Gaussian noise)
sig2 <- (1-nugget)*sigma2.tot                      # Variance of Gaussian process

for (block in 1:N_loops){
  
  cat(sprintf("Starting block number %d out of %d:\n", block, N_loops))
  
  ind_start <-    (block-1)*Block_size +1
  ind_end   <- min(   block*Block_size, N)
  index     <- ind_start:ind_end
  
  Test.points.full <- Test.points.global[index, , drop=F]
  
  # Full set of regressors associated with test points in the block
  Regr2 <- predict(Interactions2, newdata = Test.points.full)
  Regr3 <- predict(Interactions3, newdata = Test.points.full)
  Test.regr.full <- cbind(Regr2, Regr3[, c(78, 83), drop=F])
  
  Test.points <- Test.points.full[, GP_variables, drop=F]
  Test.regr <- cbind(1, Test.regr.full[, regr, drop=F])   # add a column of 1 for intercept
  
  # Call the function which carries out  Bayes Linear Emulation, on the selected regressors given observed output y
  res.block <- BL.Emul(Design.points, Test.points, y, 
                       Regress.Design = Design.regr, 
                       Regress.Test = Test.regr, 
                       beta = beta, Cov.beta = Cov.beta, 
                       sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)
  
  # Store the results for this block and this month
  res[[month]][index, ] <- res.block
  
  # Potentially release memory
  gc()
  
  cat(sprintf("\n"))
  if (identical(block%%50,0) | (block == N_loops) ){
    cat(sprintf("Saving results for block %d of %d...\n", block, N_loops))
  #  save(res, file = "Results_61-100.RData", version = 2, compress = "xz")
    cat(sprintf("Results saved.\n\n"))
  }

}
  
cat(sprintf("May completed.\n"))





