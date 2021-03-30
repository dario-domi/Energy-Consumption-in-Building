# This script loads the results (means and variances) of emulated monthly gas
# consumption at a large set of test inputs. It then computes the implausibility 
# measure associated with each input, for each month.

################################################################################


##################################################
## PRELIMINARY ACTIONS: LOAD LIBRARIES AND DATA
#################################################

# Set folder
setwd('/Users/Durham/Desktop/Post Doc/Projects/UQ for building energy systems/')

# Load custom function to compute implausibility measure
library(xlsx)
source("../Emulation.R")

# Load the observations (1 x 12)
month.names <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
Obs_gas <- read.xlsx("Data/Observation vs Simulated - Hailiang.xlsx", sheetIndex =1, 
                     startRow = 19, endRow = 31, colIndex = 3)    # dim: 12x1
Obs_gas <- t(Obs_gas)                                             # transpose to have months in columns: 1x12
colnames(Obs_gas) <- month.names                                  # name the column by months

# Load 1 million emulator estimates (The first million of test points)
load('RData/Results_First_Million.RData') # takes about 10 seconds to load
N <- dim(res1[[1]])[1]


############################################################
# CARRY OUT THE ACTUAL HISTORY MATCHING
############################################################

# Build matrix of Implausibility Measures for all observations (rows) and all months of interest (cols)
month.indices <- c(1:5, 9:12)
N.months <- length(month.indices)
Global_IM <- matrix(nrow = N, ncol = N.months)
colnames(Global_IM) <- month.names[month.indices]

Mod.Discr <- 0.1
Meas.Err <- 0.05
for (i in month.indices){
  month <- month.names[i]
  X <- res1[[month]]
  z <- Obs_gas[, month]
  IM <- Compute_IM( X[,"Mean"], X[,"Var"], z, Mod.Discr, Meas.Err)
  Global_IM[, month] <- IM
  rm(X, IM)
  gc()
}


# Y is a matrix with the same dimensions of Global_IM: 
# Y[i,j]=1 if abs(Global[i,j])<C, and Y[i,j]=0 otherwise.
C <- 4
Y <- ifelse( abs(Global_IM) < C, 1, 0)
# For test input i, Z[i] is the number of months where a non-implausible match has been found
Z <- as.matrix(rowSums(Y, na.rm = T))


ind.MD01.C4 = Z > N.months-0.5
save(ind.MD01.C4, ind.MD02.C4, file = 'HM_Indices.RData')



