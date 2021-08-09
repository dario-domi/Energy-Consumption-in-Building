################################################################################
#
# This script reads:
#   i) simulated temperatures from Data/Temperature_Data/hourly-results
#  ii) observed  temperatures from Data/Temperature_Data/Space_Temperatures.xlsx
# and saves them in the file RData/Simulated-and-Observed-Temperatures.RData.
#
# Temperatures are read for two rooms, Master and Kitchen:
# observed temperatures are stored in a vector of length 8760=24x365,
# simulated temperatures in a matrix 8760x1000 (one simulation per column).
#
# In addition to the above:
#  i) The 8760 times&dates at which temperatures are recorded are stored in DateTimes
# ii) The 1000 8-dim design points (one for each simulation) are stored in Design.
#
################################################################################


## SET FOLDER AND LOAD PACKAGES/FUNCTIONS ##
setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')
library(openxlsx)
source('Auxiliary_Scripts/Auxiliary_Functions.R') # for Rescale.Linearly()


# DESIGN POINTS (1000 x 8) ##
X <- read.csv("../Data/inputs-batch2.csv")
Design.Original <- X[, -1]           # delete first column with numbers from 1 to 1000
Design <- Rescale.Linearly(Design.Original)


######################################################################
## OBSERVED TEMPERATURES (MASTER AND KITCHEN) AND SEQUENCE OF DATES ##

file <- "../Data/Temperature_Data/Space_Temperatures.xlsx"
Table <- read.xlsx(file)

# DATES AND TIMES
DateTimes <- Table[,1]
DateTimes <- as.POSIXct(DateTimes*86400, origin="1899-12-30", tz='Europe/London')

# KITCHEN TEMPERATURES
Kitch.Obs <- Table[, "Kitchen"]

# MASTER TEMPERATURES
Mast.Obs <- Table[, "Master"]
rm(file, Table)

########################################################
## SIMULATED TEMPERATURES IN MASTER AND KITCHEN ROOMS ##

N.sim <- 1000
N.steps <- 8760
# These variables will contain simulated kitchen and master temperatures for all 1000 runs
Kitch.Sim  <- matrix(0, N.steps, N.sim)
Mast.Sim <- matrix(0, N.steps, N.sim)

filename.base <- "../Data/Temperature_Data/hourly-results/"
# The for loop takes about 3 seconds for 100 files
for (i in 1:1000){
  file <- paste(filename.base, as.character(i), '-var.csv', sep = "")
  sample <- read.csv(file)
  Kitch.Sim[,i]  <- sample[,2]
  Mast.Sim[,i] <- sample[,3]
}


##################
## SAVE RESULTS ##

save(Design, Design.Original, DateTimes,
     Kitch.Sim, Mast.Sim, 
     Kitch.Obs, Mast.Obs,
     file = 'RData/Inputs/Simulated_and_Observed_Temperatures.RData')
