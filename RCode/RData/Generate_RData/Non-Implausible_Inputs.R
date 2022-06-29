################################################################################
#
# This script loads the results (means and variances) of emulation at a large 
# sequence of inputs, for:
# 1) monthly gas consumption in the flat; 
# 2) average day/night temperature difference in July, in the kitchen;
# 3) average day/night temperature difference in July, in the master room.
#
# It then computes implausibility measures associated with each input
# for each of the three quantities, and saves them in the file
#"RData/Results_Emulation/Non-Implausible-Inputs.RData".
#
################################################################################


##################################################
## PRELIMINARY ACTIONS: LOAD FUNCTIONS AND DATA
##################################################

# SET FOLDER AND LOAD LIBRARIES
setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')
library(cgwtools)                # to use resave(), to append data to an existing .RData file

# LOAD CUSTOM FUNCTION TO COMPUTE IMPLAUSIBILITY MEASURE
source("../../Emulation.R")
rm(BL.Emul, Corr.fun)

# INPUTS
load('RData/Results_Emulation/Eval_Inputs.RData') # loads Eval.points.full
Eval.points <- Eval.points.full
rm(Eval.points.full)  # clean workspace
invisible(gc())       # release memory

# EMULATED GAS CONSUMPTIONS, AND OBSERVED CONSUMPTION
load('RData/Results_Emulation/Gas_Emulation_Results.RData')   # Emulated gas consumptions (Emul.Res)
load('RData/Inputs/Simulated_and_Observed_Gas.RData')         # Gas data (observed and simulated)
rm(Design.Original, Gas.Sim)

# EMULATED aAND OBSERVED TEMPERATURE DIFFERENCE IN KITCHEN AND MASTER
load("RData/Results_Emulation/Emul_SummTempDiff.RData")


##########################################################################
# COMPUTE IMPLAUSIBILITY MEASURES FOR GAS AND SAVE THEM
##########################################################################

# USEFUL VARIABLES
month.indices <- c(1:5, 9:12)
N.months <- length(month.indices)
N <- nrow(Emul.Gas[[1]])
Global_IM <- matrix(nrow = N, ncol = N.months)     # Impl-measure matrix
colnames(Global_IM) <- month.names[month.indices]

# BUILD MATRIX WITH IMPLAUSIBILITY MEASURES: ROWS <-> INPUTS, COLS <-> MONTHS
Mod.Discr <- 0.1
Meas.Err <- 0.05
for (i in month.indices){
  month <- month.names[i]
  X <- Emul.Gas[[month]]
  z <- Gas.Obs[, month]
  IM <- Compute_IM( X[,"Mean"], X[,"Var"], z, Mod.Discr, Meas.Err)
  Global_IM[, month] <- IM
  rm(X, IM)
  invisible(gc())
}

# BUILD MATRIX Y WITH: Y[i,j]=1 if abs(Global_IM[i,j])<C; Y[i,j]=0 otherwise.
C <- 4                                 # threshold for history matching
Y <- ifelse( abs(Global_IM) < C, 1, 0) 
Z <- rowSums(Y, na.rm = T)             # N-vector. Z[i] = # of months with a non-implausible match

# COMPUTE AND SAVE VECTOR OF NON-IMPLAUSIBLE INPUTS
gas.compat <- (Z > N.months-0.5)
#resave(gas.compat, file = "RData/Results_Emulation/Non-Implausible-Inputs.RData")

cat("The percentage of non-implausible space is: ", 
    format(100*mean(gas.compat), digits = 2, nsmall = 5), "%", sep = "")

# PERCENTAGE OF NON-IMPLAUSIBLE SPACE FOR A GIVEN MONTH
for (i in c(1:5,9:12)){
  month <- month.names[i]
  cat("Percentage of non-implausible space for ", month, ": ", 
      format(100*sum( Y[,month] )/N, digits = 3, nsmall = 1), "%\n", sep = "")
}


################################################################################
# COMPUTE IMPLAUSIBILITY MEASURES FOR KITCHEN AND MASTER TEMPERATURE:
# average temp diff in July, between day (8am-11pm) and night (00am-7am).
################################################################################

Mod.Discr <- 0.1
Meas.Err <- 0.05

# KTCHEN
X <- Emul.K.diff
z <- obs.kitch
IM.kitch <- Compute_IM( X[,"Mean"], X[,"Var"], z, Mod.Discr, Meas.Err)
kitch.compat <- abs(IM.kitch)<3

# MASTER
X <- Emul.M.diff
z <- obs.mast
IM.mast <- Compute_IM( X[,"Mean"], X[,"Var"], z, Mod.Discr, Meas.Err)
mast.compat <- abs(IM.mast)<3

#resave('kitch.compat', 'mast.compat', 
#       file = "RData/Results_Emulation/Non-Implausible-Inputs.RData")

