##############################################################################

# Generates 1 million (or whatever) 8D points in the cube of side [-1,1].
# These are the points over which emulation is carried out in several other
# scripts.

##############################################################################

library(randtoolbox)

# The following line is only used to get names of 8 variables
load('RData/Inputs/Design_Points.RData')

# Generate sobol sequence of N 8D points
N <- 1.e7
Eval.points.full <- 2*sobol(N, dim = 8, scrambling = 1, seed = 2341) -1
colnames(Eval.points.full) <- names(Design)

# Save sequence for future use
save(Eval.points.full, file = 'RData/Results/Eval_Inputs.RData')




