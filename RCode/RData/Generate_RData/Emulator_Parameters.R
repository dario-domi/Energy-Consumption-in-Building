#####################################################################################
#
# This script creates and stores, for each month of interest, which parameters 
# should be used to build the stochastic process part of the corresponding 
# emulator, and which correlation length to use in the prior correlation 
# function of the emulator (same correlation length for all covariates).
#
###############################################################################

# Set folder
setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')

# Names appearing in the variables 'Active_params' and 'Corr_lengths'
month.names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Active Parameters
Act_params <- sapply(month.names, function(x) NULL)
Act_params[[1]] <- c(1,2,3,6,8)
Act_params[[2]] <- c(1,2,3,7,8)
Act_params[[3]] <- c(1,2,3,6,7,8)
Act_params[[4]] <- c(1,2,3,6,8)
Act_params[[5]] <- c(1,2,3,4,6,8)
Act_params[[9]] <- c(1,2,3,6,7,8)
Act_params[[10]] <- c(1,2,3,6,7,8)
Act_params[[11]] <- c(1,2,3,6,7,8)
Act_params[[12]] <- c(1,2,3,6,7,8)

# Correlation lengths
Corr_lengths <- sapply(month.names, function(x) NA)
Corr_lengths[1] <- 0.8 # also 0.8, 0.9
Corr_lengths[2] <- 0.65 # 
Corr_lengths[3] <- 1.3 # 
Corr_lengths[4] <- 1 # 
Corr_lengths[5] <- 1 # 
Corr_lengths[9] <- 1.2 # 
Corr_lengths[10] <- 1.4 # 
Corr_lengths[11] <- 1.2 # 
Corr_lengths[12] <- 1.3 # 

# Save 
save(Act_params, Corr_lengths, file = "RData/Results_Emulator/Emulator_Parameters.RData")
