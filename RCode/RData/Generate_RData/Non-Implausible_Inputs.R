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

#library(cgwtools)                # to use resave(), to append data to an existing .RData file

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

# IMs: Nx9 matrix, with: IMs[i,j]= Impl measures for month j, at input i
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Sep", "Oct", "Nov", "Dec")
N.months <- length(months)
N <- nrow(Emul.Gas[[1]])
IMs <- matrix(nrow = N, ncol = N.months)   # Implausibility measures, 0.34Gb
colnames(IMs) <- months

# BUILD MATRIX WITH IMPLAUSIBILITY MEASURES: ROWS: INPUTS; COLS: MONTHS
Mod.Discr <- 0.1
Meas.Err <- 0.05
for (month in months){
  X <- Emul.Gas[[month]]
  z <- Gas.Obs[, month]
  IM_month <- Compute_IM( X[,"Mean"], X[,"Var"], z, Mod.Discr, Meas.Err)
  IMs[, month] <- IM_month
  rm(X, IM_month)
  invisible(gc())
}

# BUILD INDICATOR MATRIX Y: Y[i,j]=1 if abs(IMs[i,j])<C.
C <- 4                                 # threshold for history matching
Y <- ifelse( abs(IMs) < C, 1, 0); invisible(gc())
Z <- rowSums(Y, na.rm = T)             # N-vector. Z[i] = # of months whose IM is < C

# COMPUTE AND SAVE VECTOR OF NON-IMPLAUSIBLE INPUTS
gas.compat <- (Z > N.months-0.5)       # Logical vector, N-dim. Which inputs are non-implausible
# The function "resave" *adds* the specified variable to the RData file. Does not replace the file.
# resave(gas.compat, file = "RData/Results_Emulator/Non-Implausible-Inputs.RData") # needs cgwtools library

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

