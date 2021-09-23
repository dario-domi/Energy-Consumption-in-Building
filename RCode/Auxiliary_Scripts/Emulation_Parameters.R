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
Corr_lengths[1] <- 0.8 # also 0.8, 0.9
Corr_lengths[2] <- 0.65 # 
Corr_lengths[3] <- 1.3 # 
Corr_lengths[4] <- 1 # 
Corr_lengths[5] <- 1 # 
Corr_lengths[9] <- 1.2 # 
Corr_lengths[10] <- 1.4 # 
Corr_lengths[11] <- 1.2 # 
Corr_lengths[12] <- 1.3 # 


