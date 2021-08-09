#####################################################################################
#
# This script defines, for each of the 12 months, which covariates should be used 
# in building the stochastic process part of the corresponding emulator, and which 
# correlation length to use in the prior correlation function of the emulator 
# (same correlation length for all covariates).
#
###############################################################################


month.names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

Act_inputs <- sapply(month.names, function(x) NULL)
Act_inputs[[1]] <- c(1,2,3,4,6)
Act_inputs[[2]] <- c(1,2,3,4,6)
Act_inputs[[3]] <- c(1,2,3,4,6,7)
Act_inputs[[4]] <- c(1,2,3,4,6)
Act_inputs[[5]] <- c(1,2,3,6,8)
Act_inputs[[9]] <- c(1,2,3,4,6,8)
Act_inputs[[10]] <- c(1,2,3,4,6,8)
Act_inputs[[11]] <- c(1,2,3,4,6,8)
Act_inputs[[12]] <- c(1,2,3,4,6)

Corr_lengths <- numeric(length = 12)
Corr_lengths[1] <- 0.35
Corr_lengths[2] <- 0.3
Corr_lengths[3] <- 0.35
Corr_lengths[4] <- 0.3
Corr_lengths[5] <- 0.4
Corr_lengths[9] <- 0.4
Corr_lengths[10] <- 0.45
Corr_lengths[11] <- 0.5
Corr_lengths[12] <- 0.35

# Rescale correlation lengths to account for new definition of corr function (exp(-X/2)) wrt to old version (exp(-X))
Corr_lengths <- sqrt(2)*Corr_lengths



# NEW PARAMETERS

Act_inputs <- sapply(month.names, function(x) NULL)
Act_inputs[[1]] <- c(1,2,3,6,8)
Act_inputs[[2]] <- c(1,2,3,7,8)
Act_inputs[[3]] <- c(1,2,3,6,7,8)
Act_inputs[[4]] <- c(1,2,3,6,8)
Act_inputs[[5]] <- c(1,2,3,4,6,8)
Act_inputs[[9]] <- c(1,2,3,6,7,8)
Act_inputs[[10]] <- c(1,2,3,6,7,8)
Act_inputs[[11]] <- c(1,2,3,6,7,8)
Act_inputs[[12]] <- c(1,2,3,6,7,8)

Corr_lengths <- numeric(length = 12)
Corr_lengths[1] <- 0.8 # also 0.9
Corr_lengths[2] <- 0.5 # 
Corr_lengths[3] <- 1.2 # 
Corr_lengths[4] <- 0.7 # 
Corr_lengths[5] <- 1.2 # 
Corr_lengths[9] <- 1 # 
Corr_lengths[10] <- 1.2 # 
Corr_lengths[11] <- 1.2 # 
Corr_lengths[12] <- 1.3 # 









