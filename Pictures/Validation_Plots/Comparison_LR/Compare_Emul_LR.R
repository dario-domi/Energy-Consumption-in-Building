#################################################################################
#
# The script produces a plot to compare the predictive performance of the 
# emulator to the one of linear regression. The case of March Gas consumption
# is considered.
#
################################################################################


###############################################################
####    SET FOLDER AND LOAD DATA

# Set folder
setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')

# Load data to use Emulation_Small_TestSet.R
load('RData/Results_Simulator/Design_Points.RData')   # Design Points
load('RData/Results_Emulator/SplitSet.RData')         # Training, Evaluation, Test sets
load('RData/Results_Emulator/Regressors.RData')       # Regressors used in emulation

# Library needed to use function error.bars
library(psych)


###################################################################
###    PERFORM LINEAR REGRESSION AND EMULATION ON TEST POINTS

# Set of points where to make predictions
Test.Set <- Design[test, ]

# Perform emulation (for all months)
source("Emulation_Small_TestSet.R")

# Train and test outputs
month <- "Mar"
y.train <- Gas.Sim[train, month]
y.test  <- Gas.Sim[test, month]

# Linear regression predictions (March gas consumption) at test points 
regr <- Regressors[[month]]                       
fit <- lm(y.train ~ ., data = as.data.frame(Interactions.train[, regr]))
All.Regr.Test <- predict(Interactions.train, newdata = data.matrix(Design[test, ]))
LRpreds <- predict(fit, as.data.frame(All.Regr.Test[, regr]))


####################################################################
###     PREPARE VARIABLE FOR CATS EYE PLOT

# Preparation to plot cats eyes: the variable 'data' contains lower and upper
# bounds of emulator predictions (mean +/- 3std)
# (correction "/sqrt(3)" due to the way the error.bars() function handles data)
n.std <- 3
X <- Emul[[month]]
data <- rbind(X[,1] + n.std*sqrt(X[,2])/sqrt(3),
              X[,1],
              X[,1] - n.std*sqrt(X[,2])/sqrt(3))

# Sort test indices in increasing order of y.test values 
sorted <- sort(y.test, index.return=T)$ix

# Indices of the 82nd-to-89th y.test values
ind <- sorted[82:89] 
L <- length(ind)

# Store min and max y-values needed in plot
m <- min( c(X[ind,1] - n.std*sqrt(X[ind,2]), LRpreds[ind], y.test[ind]) )
M <- max( c(X[ind,1] + n.std*sqrt(X[ind,2]), LRpreds[ind], y.test[ind]) )


####################################################################
###     PRODUCE CATS EYE PLOTS

# Path to save plot
file.name <- paste("../Pictures/Validation_Plots/Comparison_LR/", "LR_", 
                   month, "_82-89", ".png", sep = "")

# Colour of cats eyes
col.em <- rgb(1, 0.4, 0.4)

# Figure specifications
png(file.name, width = 7.5, height = 4.5, unit="in", res=1000)
par(cex =1.2, lwd=1.5, 
    mai=c(0.8, 0.8, 0.1, 0.1), # for each subplot: bott, left, top, right margin
    mgp = c(2,0.5,0))


#####    ACTUAL PLOTS   ####

# 1) Emulator predictions shown as cat eyes plot
error.bars(data[,ind], alpha = 0.0954659663, eyes=T, col=col.em,
           main = '',
           xlab = 'Simulation Index',
           ylab = 'March Gas Consumption [kWh]',
           ylim = c(m,M)
)
# 2) Add linear regression predictions\
points(1:L, LRpreds[ind], pch=23, col = 'black', bg = 'yellow', lwd=1.7, cex = 1.3)
# 3) Add real y.test values (star is pch=8b)
points(1:L, y.test[ind], pch=43, cex=1.3, col='blue')
legend('topleft', legend = c('Simulator', 'Emulator', 'Linear regression'), 
       pch = c(43, 22, 23), col = c('blue', 'black', 'black'), 
       pt.bg = c(NA, col.em, 'yellow'),  pt.cex= c(1.5, 1.5, 1.3), 
       bg = 'aliceblue')
dev.off()

