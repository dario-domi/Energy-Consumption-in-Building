#############################################################################
#
# Stores the coordinates of the 1000 design points used in the project. A
# rescaled version of the 1000x8 matrix is also saved, where each variable 
# (column) is rescaled linearly into the interval [-1,1]. Moreover, the 1000
# points are divided into a training(750), validation(150) and test(100) sets.
#
#############################################################################


setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')
source('Auxiliary_Scripts/Auxiliary_Functions.R')

## DESIGN POINTS (1000 x 8) ##
X <- read.csv("../Data/inputs-batch2.csv")
Design.Original <- X[, -1]           # delete first column with numbers from 1 to 1000
Design <- Rescale.Linearly(Design.Original)

## Select training, validation and test sets
set.seed(5879, kind = "default")
train <- sample(1:1000, 750)
valid <- sample((1:1000)[-train], 150)
test <- (1:1000)[-c(train, valid)]

save(Design, Design.Original, train, valid, test, file = 'RData/Inputs/Design_Points.RData')
