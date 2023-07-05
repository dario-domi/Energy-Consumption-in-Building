#############################################################################
#
# Reads the (8-dimensional) coordinates of the 1000 design points used to
# train the emulators. A rescaled version of the matrix is also saved, where 
# each variable (column) is rescaled linearly into the interval [-1,1].
# The original ranges are saved into the matrix 'Range_Params'.
#
# Variables saved:
# Design.Original: matrix, 1000x8
# Design:          matrix, 1000x8 (columns rescaled into [-1,1])
# Ranges_Params:   matrix, 2x8
#
#############################################################################


setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')
source('Auxiliary_Scripts/Auxiliary_Functions.R') # For the function "Rescale.Linearly"


## Set the ranges of the 8 input parameters
N_var<-8
Ranges_Params <- matrix(NA, nrow=2, ncol=N_var)

Ranges_Params[1,1]<-17.5     # Heating setpoint [Celcius degrees]
Ranges_Params[2,1]<-20.5

Ranges_Params[1,2]<-0.6      # Gas boiler seasonal efficiency
Ranges_Params[2,2]<-0.75

Ranges_Params[1,3]<-0.04     # External wall thickness [m]
Ranges_Params[2,3]<-0.063

Ranges_Params[1,4]<-0.15     # ~Roof thickness [m]
Ranges_Params[2,4]<-0.21

Ranges_Params[1,5]<-0.045    # ~Floor thickness [m] 
Ranges_Params[2,5]<-0.055

Ranges_Params[1,6]<-0.2      # Infiltration [ac/h]
Ranges_Params[2,6]<-0.95 

Ranges_Params[1,7]<-6.15e-06 # DHW consumption [litre/day]
Ranges_Params[2,7]<-2.20e-05

Ranges_Params[1,8]<-1.05     # Cooking [W/m2]
Ranges_Params[2,8]<-6.3


## DESIGN POINTS (1000 x 8) ##
X <- read.csv("../Data/inputs-batch2.csv")
Design.Original <- X[, -1]           # delete first column with numbers from 1 to 1000
Design <- Rescale.Linearly(Design.Original, Ranges_Params)

Design <- data.matrix(Design)
Design.Original <- data.matrix(Design.Original)
colnames(Ranges_Params) <- colnames(Design)
rownames(Ranges_Params) <- c("lower", "upper")

save(Design, Design.Original, Ranges_Params, file = 'RData/Results_Simulator/Design_Points.RData')
