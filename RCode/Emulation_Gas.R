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
# Preliminary: to check how many points 'Emul.Gas' contains results for
#for (i in 1:12){
#  cat("Number of emulated points for ", month.names[i], ": ",  sum(!is.na(Emul.Gas[[i]][,1])), "\n", sep = "")
#}
############################################################################################################


#####################################################
# SET DIRECTORY AND LOAD LIBRARIES/CUSTOM FUNCTIONS
#####################################################

# SET FOLDER AND LOAD NEEDED FUNCTIONS/DATA
setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')
load('RData/Results_Emulation/Gas_Emulation_Results.RData') # Emulation results (Emul.Gas)

source('../../Emulation.R')                            # Function to perform emulation

load('RData/Inputs/Design_Points.RData')               # Design Points
load('RData/Inputs/SplitSet.RData')                    # Training, Evaluation, Test sets
load('RData/Results_Emulation/Eval_Inputs.RData')        # loads Eval.points.full
#Eval.points.full[,1] <- 2*Eval.points.full[,1] - 1
#Eval.points.full[,6] <- 1.2*Eval.points.full[,6]
#Eval.points.full[,8] <- 1.5*Eval.points.full[,8] + 1.5

load('RData/Inputs/Simulated_and_Observed_Gas.RData')  # Gas data (observed and simulated)

load('RData/Inputs/Regressors.RData')                  # Regressors used in emulation
source('Auxiliary_Scripts/Emulation_Parameters.R')     # Active Inputs and Correlation lengths for each month

source('Auxiliary_Scripts/Auxiliary_Functions.R')      # Load function Create.my.list
rm(Cross_Val, Extract.Flat.Level, Rescale.Linearly)


####################################################################
# LOAD SOME EMULATION PARAMETERS AND BUILD NEW SET OF TEST POINTS 
####################################################################

# Emulation will be carried out on the elements from N1 to N2 of Sobol sequence
N1 <- 6e6 + 1         # First and last index of sobol sequence...
N2 <- 1e7             # ...on which emulation will be carried out
N <- N2 - N1 + 1      # Total number of test points on which emulation will be carried out


## INTERACTIVE PART
# Ask the user whether to create a new list to store the emulated values.
# Only allowed answers are Y or N, and the length of the list. Question will be repeated otherwise. 
# If Y, a further confirmation will be asked before proceeding.
answer <- NA
cat("Should a new empty list to store emulation results, 'Emul.Gas', be created? (Y/N)\n", 
    "If 'Y', you'll be able to input its length afterwards.", sep = "")
answer <- readline()
while ( !(answer %in% c("Y", "N")) ){
  cat("Only type 'Y' for yes, 'N' for no.")
  answer <- readline()
}

if (answer=="Y"){
    cat("Warning. You chose Yes: if an object 'Emul.Gas' already exists, it will be overwritten.\n",
      "Press 'Y' to confirm your choice, anything else otherwise.", sep = "")
  answer <- readline()
}

if (answer=="Y"){
  cat("Type the desired length of the list, and press ENTER")
  Lngt <- as.numeric(readline())
  Emul.Gas <- Create.my.list(Lngt, month.names, indices = c(1:5, 9:12), val = NA)
}


# Set of Test inputs
Test.points.global <- Eval.points.full[N1:N2, , drop=F]  # Subselect only inputs of interest
rm(Eval.points.full)
invisible(gc())                                          # release memory

# Build a matrix of all 2-way interactions (orthogonal polynomials of order 2) for training set
Interactions.train <- poly(data.matrix(Design[train,]), degree=2)


##########################################################################
# ACTUAL EMULATION: LOOP OVER THE DIFFERENT BLOCKS OF SIZE "Block_size"
##########################################################################

# The subdivision in blocks allows to:
# 1. Proceed "in parallel" among months (no month completed with a huge number
# of emulated points while others are still to start); and
# 2. Avoid the generation of all 2-way interactions for huge blocks
# (only some of these interactions will be used).  

# Emulation performed in blocks
Block_size <- 1.e5                 # Maximum number of test points which will be emulated in each block, for all months
N_loops <- ceiling(N/Block_size)   # Number of loops subsequently needed

for (block in 1:N_loops){
  
  cat(sprintf("Starting block number %d out of %d:\n", block, N_loops))
  
  ind_start <- (block-1)*Block_size +1   # first index of current block
  ind_end   <- min(block*Block_size, N)  #  last index of current block
  index     <- ind_start:ind_end
  
  # Test points in current block
  Test.points.block <- Test.points.global[index, , drop=F]  # Block_size x N_var
  
  # Full set of regressors associated with test points in the block
  All.Regr.block <- predict(Interactions.train, newdata = Test.points.block)  # Block_size x 44
  
  for (month in c(1:5, 9:12) ){
    
    cat(sprintf("\t %s being computed.\n", month.names[month]))
    
    # Linear Regression
    y.train <- Gas.Sim[train, month]
    regr <- Regressors[[month]]                 # logical vector with regressors to be used in lm
    fit <- lm(y.train ~ ., data = as.data.frame(Interactions.train[, regr]))
    beta <- fit$coefficients
    
    # Active inputs and Correlation Lengths
    Active.Inputs <- Act_inputs[[month]]              # Index of Active Inputs
    N_Act <- length(Active.Inputs)                         
    
    # Compute Active inputs and Regressors for Train and Test Sets
    ActInp.Train <- Design[train, Active.Inputs, drop=F]          # Design points used to train the emulator, 700 x N_Act
    ActInp.Test  <- Test.points.block[, Active.Inputs, drop=F]    # Test points: Block_size x N_Act
    Regr.Train   <- cbind(1, Interactions.train[, regr, drop=F])  # Regressors, training: add a column of 1s for intercept
    Regr.Test    <- cbind(1, All.Regr.block[, regr, drop=F])

    # Prior correlation lengths and variances (explained and residual) used in the emulator
    d <- Corr_lengths[month] * replicate(N_Act, 1)      # Correlation lengths
    sigma2.tot <- var(fit$residuals)                    # Prior cumulative variance of homoschedastic Gaussian process
    nugget <- 0.05                                      # Fraction of residual variability not explained by regressors
    nu2 <- nugget*sigma2.tot                            # Variance of nugget term (Gaussian noise)
    sig2 <- (1-nugget)*sigma2.tot                       # Variance of Gaussian process
    
    # Call the function performing Bayes Linear Emulation, on the selected regressors given observed output y
    res.block <- BL.Emul(ActInp.Train, ActInp.Test, y.train, 
                         Regress.Design = Regr.Train, 
                         Regress.Test = Regr.Test, 
                         beta = beta,
                         sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)

    # Store the results for this block and this month
    Emul.Gas[[month]][N1-1+index, ] <- res.block

    # Release memory (otherwise it won't get released despite using same name for some variables)
    invisible(gc())
  }
  
  cat(sprintf("\n"))
  if ( identical(block%%3,0) | (block == N_loops) ){
    cat(sprintf("Saving results for block %d of %d ...\n", block, N_loops))
    save(Emul.Gas, file = "RData/Results_Emulation/Gas_Emulation_Results.RData")
    cat(sprintf("Results saved.\n\n"))
  }
  
}



