################################################################################

# Saves the data regarding simulated and observed monthly consumption, both gas
# and electricity. Data saved in:
# RData/Results_Simulator/Simulated_and_Observed_Gas.RData. 
# RData/Results_Simulator/Simulated_and_Observed_Elec.RData.
#
# The file Simulated_and_Observed_Gas.RData contains:
#
# Design.Original: matrix, 1000x8, with the  1000 8dim input configurations for the design runs
# Design:          1000x8, as Design, but columns rescaled in [-1,1]
# Gas.Sim:         1000x12, simulated consumption at each of the 1000 inputs, for the 12 months
# Gas.Obs:         1x12, observed monthly gas consumption
#
# Similarly for Simulated_and_Observed_Elec.RData

#################################################################################


## SET FOLDER, LOAD PACKAGES/FUNCTIONS
setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')
library(openxlsx)

month.names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


## DESIGN POINTS (1000 x 8) ##
load('RData/Results_Simulator/Design_Points.RData')

## GAS OUTPUTS (1000 x 12) ##
Raw.out <- read.csv("../Data/results-batch2.csv", skip = 3008, nrow=1001) # Only read the gas rows
Gas.Sim <- Raw.out[-1,]          # remove baseline simulation
Gas.Sim[,"File.Name"] <- NULL    # remove first column with file name of runs
rownames(Gas.Sim) <- 1:1000      # name the simulations by numbers
colnames(Gas.Sim) <- month.names # assign month names
Gas.Sim <- data.matrix(Gas.Sim)


## GAS OBSERVATIONS (1x12) ##
Gas.Obs <- read.xlsx("../Data/Observation vs Simulated - Hailiang.xlsx", 
                     sheet =1, rows = 20:31, cols = 3, colNames = F)
Gas.Obs <- as.matrix(t(Gas.Obs)) # 1x12 matrix
rownames(Gas.Obs) <- NULL
colnames(Gas.Obs) <- month.names # name columns by month


## ELEC OUTPUTS (1000 x 12) ##
Raw.out <- read.csv("../Data/results-batch2.csv", skip = 2005, nrow=1001)
Elec.Sim <- Raw.out[-1,]          # remove baseline simulation
Elec.Sim[,"File.Name"] <- NULL    # remove first column with file name of runs
rownames(Elec.Sim) <- 1:1000      # name the simulations by numbers
colnames(Elec.Sim) <- month.names # assign month names
Elec.Sim <- data.matrix(Elec.Sim)

## ELEC OBSERVATIONS (1x12) ##
Elec.Obs <- read.xlsx("../Data/Observation vs Simulated - Hailiang.xlsx", 
                     sheet =1, rows = 3:14, cols = 3, colNames = F)
Elec.Obs <- as.matrix(t(Elec.Obs)) # 1x12 matrix
rownames(Elec.Obs) <- NULL
colnames(Elec.Obs) <- month.names # name columns by month


## SAVE RESULTS GAS ##
save(Design, Design.Original, Gas.Obs, Gas.Sim, month.names,
     file = 'RData/Results_Simulator/Simulated_and_Observed_Gas.RData')

## SAVE RESULTS ELEC ##
save(Design, Design.Original, Elec.Obs, Elec.Sim, month.names,
     file = 'RData/Results_Simulator/Simulated_and_Observed_Elec.RData')



# Alternative for Observations
# Gas.Obs <- matrix(c(3201, 2130, 1687, 1452, 1109, 628, 588, 520, 563, 1010, 1985, 2564), 
#                  ncol = 12)
# Elec.Obs <- matrix(c(385.316, 258.3586, 227.2641, 211.3829, 199.5385, 184.077, 
#                    200.5691, 198.8144, 210.6247, 225.172, 387.4517, 302.8832),
#                    ncol = 12)
# colnames(Gas.Obs) <- month.names # name the column by months
