# Loads the data re simulated monthly gas consumptions. Creates the following variables:

# month.names: list with the abbreviations of the 12 month names
# Design:      1000x8, each row contains values of the 8 inputs at which the simulator has been run
# Outputs_gas: 1000x12, simulated consumption at each of the 1000 inputs, for the 12 months
# Obs_gas:     1x12, monthly gas observations

#################################################################################

month.names <- c("Jan", "Feb", "Mar", "Apr", "May", "June", 
                 "July", "Aug", "Sep", "Oct", "Nov", "Dec")

# INPUTS (1000 x 8)
Raw.inp <- read.csv("../Data/inputs-batch2.csv")
Design <- Raw.inp[, -1]           # delete first column with numbers from 1 to 1000
#var.names <- colnames(Inputs)    # store names of input variables

# OUTPUTS (1000 x 12)
Raw.inp <- read.csv("../Data/results-batch2.csv", skip = 3008, nrow=1001)
Outputs_gas <- Raw.inp[-1,]          # remove baseline simulation
Outputs_gas[,"File.Name"] <- NULL    # remove first column with file name of runs
rownames(Outputs_gas) <- 1:1000      # name the simulations by numbers
colnames(Outputs_gas) <- month.names # assign month names
rm(Raw.inp)

# OBSERVATIONS (1x12)
Obs_gas <- matrix(c(3201, 2130, 1687, 1452, 1109, 628, 588, 520, 563, 1010, 1985, 2564), ncol = 12)
colnames(Obs_gas) <- month.names # name the column by months
