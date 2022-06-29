################################################################################
#
# Splits the original set of 1000 simulations into a training, evaluation 
# and test set, to be used to build and validate emulators.
#
################################################################################

# Set folder
setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')

# Set seed and generate subdivision into three sets
set.seed(6321, kind = "default")  # 5879, 6321
train <- sample(1:1000, 700)                           # Training set,   700 points
eval <- sample((1:1000)[-train], 150)                  # Validation set, 150 points
test  <- (1:1000)[-c(train, eval)]                     # Test set,       150 points

# Sort in ascending order
train <- sort(train)
eval <- sort(eval)












