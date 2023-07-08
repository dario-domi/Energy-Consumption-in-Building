################################################################################
#
# This script loads the results (means and variances) of emulation at a large 
# sequence of inputs, for:
# 1) monthly gas consumption in the building; 
# 2) average day/night temperature difference in July, in the kitchen;
# 3) average day/night temperature difference in July, in the master room.
#
# It then generates the following RData files in RData/Results_Emulator:
# Full_Implausibilities.RData: Impl. Measures for Gas and Temperature
# Non-Implausible_Inputs.RData: Logical vector identifying indices of non-impl inputs
#
################################################################################


##################################################
## PRELIMINARY ACTIONS: LOAD FUNCTIONS AND DATA
##################################################

library(cgwtools)                # to use resave(), to append data to an existing .RData file

# SET FOLDER AND LOAD LIBRARIES
setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')

# LOAD CUSTOM FUNCTION TO COMPUTE IMPLAUSIBILITY MEASURE
source("../../Emulation.R")
rm(BL.Emul, Corr.matrix)

# INPUTS
load('RData/Results_Emulator/Evaluation_Set.RData') # loads Eval.points.full, 610Mb
Eval.points <- Eval.points.full
rm(Eval.points.full); invisible(gc())   # remove variable and release memory

# EMULATED GAS CONSUMPTIONS, AND OBSERVED CONSUMPTION
load('RData/Results_Simulator/Simulated_and_Observed_Gas.RData')    # Gas data (observed and simulated)
load('RData/Results_Emulator/Gas_Emulation_Results.RData')          # Emulated gas consumptions (Emul.Gas, 1.34 Gb)
rm(Design.Original, Gas.Sim)

# EMULATED AND OBSERVED TEMPERATURE DIFFERENCE IN KITCHEN AND MASTER
load("RData/Results_Emulation/Emul_SummTempDiff.RData")


##########################################################################
# COMPUTE IMPLAUSIBILITY MEASURES FOR GAS AND SAVE THEM
##########################################################################

# PREPARE MATRIX WHERE IMPLAUSIBILITY MEASURES WILL BE STORED

# IM.Gas: Nx9 matrix, with: IM.Gas[i,j]= Impl measures for month j, at input i
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Sep", "Oct", "Nov", "Dec")
N.months <- length(months)
N <- nrow(Emul.Gas[[1]])
IM.Gas <- matrix(nrow = N, ncol = N.months)   # Implausibility measures, 0.34Gb
colnames(IM.Gas) <- months

# BUILD MATRIX WITH IMPLAUSIBILITY MEASURES: ROWS: INPUTS; COLS: MONTHS
Mod.Discr <- 0.1
Meas.Err <- 0.05
for (month in months){
  X <- Emul.Gas[[month]]
  z <- Gas.Obs[, month]
  IM_month <- Compute_IM( X[,"Mean"], X[,"Var"], z, Mod.Discr, Meas.Err)
  IM.Gas[, month] <- IM_month
  rm(X, IM_month)
  invisible(gc())
}

# SAVE VARIABLE IN RDATA FILE

# Run the above with 0.1 and 0.2 Mod.Discr, and save the results
# The function "resave" *adds* the specified variable to the RData file. Does not overwrite the file.
IM.Gas10 <- IM.Gas
resave(IM.Gas10, file = "RData/Results_Emulator/Full_Implausibilities.RData") # needs cgwtools library

# If file does not exist, use save instead
# save(IM.Gas10, file = "RData/Results_Emulator/Full_Implausibilities.RData")


##########################################################################
# IDENTIFY INDICES OF NON-IMPLAUSIBLE INPUTS AND SAVE THEM
##########################################################################

# BUILD INDICATOR MATRIX Y: Y[i,j]=1 if abs(IM.Gas[i,j])<C.
C <- 4                                 # threshold for history matching
Y <- ifelse( abs(IM.Gas) < C, 1, 0); invisible(gc())
Z <- rowSums(Y, na.rm = T)             # N-vector. Z[i] = # of months where IM.Gas[i,] < C

# COMPUTE AND SAVE VECTOR OF NON-IMPLAUSIBLE INPUTS
gas.compat <- (Z > N.months-0.5)       # Logical vector, N-dim. Which inputs are non-implausible
resave(gas.compat, file = "RData/Results_Emulator/Non-Implausible_Inputs.RData") # needs cgwtools library


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
#       file = "RData/Results_Emulation/Non-Implausible_Inputs.RData")

