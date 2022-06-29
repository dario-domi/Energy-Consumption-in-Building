
####################################################################################################


#####################################################
# SET DIRECTORY AND LOAD LIBRARIES/CUSTOM FUNCTIONS
#####################################################

setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')

source('../../Emulation.R')                            # Function to perform emulation
library(leaps)

load('RData/Inputs/Design_Points.RData')               # Design Points
load('RData/Inputs/Simulated_and_Observed_Gas.RData')  # Gas data (observed and simulated)

load('RData/Inputs/Regressors.RData')


##############################################################################
# COMPARE RESULTS OF SMALL/LARGE TRAINING SET, ON TEST SET
# CASE 1: FULL EMULATOR
# CASE 2: ONLY LINEAR REGRESSION

load("RData/Inputs/Design_Points.RData")
Test.Set <- Design

# Small and large training sets
set.seed(78324)
train80 <- sample(1000, 80)
train700 <- sample(1000, 700)
test <- intersect( (1:1000)[-train80], (1:1000)[-train700] )

## Regressors
Interactions80 <- poly(data.matrix(Design[train80,]), degree=2)
Interactions700 <- poly(data.matrix(Design[train700,]), degree=2)
Regressors80 <- sapply(month.names, function(x) NULL)
Regressors700 <- sapply(month.names, function(x) NULL)

for (month in c(1:5, 9:12)){
  y.train <- Gas.Sim[train80, month]
  L <- summary(regsubsets(y.train~., data=Interactions80, method = "exhaustive", nvmax = 10))
  ind <- which.max(L$adjr2)                 # index (between 1 and nvmax) of model with max adj-R2
  Regressors80[[month]] <- L$which[ind,-1]  # logical vector with regressors for selected model
  
  y.train <- Gas.Sim[train700, month]
  L <- summary(regsubsets(y.train~., data=Interactions700, method = "exhaustive", nvmax = 10))
  ind <- which.max(L$adjr2)                 
  Regressors700[[month]] <- L$which[ind,-1]  
}

for (month in c(1:5, 9:12)){
  cat(month.name[month], ": ", identical(Regressors80[[month]], Regressors700[[month]]), "\n")
}

###################      FULL EMULATOR      #####################

# Emulation on all design points
train <- train80
Regressors <- Regressors80
source("Emulation_Small_TestSet.R")
Emul80 <- Emul

train <- train700
Regressors <- Regressors700
source("Emulation_Small_TestSet.R")
Emul700 <- Emul
rm(Emul)

# Scatter plot variance on training and test sets
m <- 11
M80 <- Emul80[[m]][test,2]
M700 <- Emul700[[m]][test,2]
M <- max(c(M80,M700))

par(mfrow=c(1,1))
plot(test, M80, pch = 20, col="blue", ylim = c(0, M))
points(train80, Emul80[[m]][train80,2], pch = 20, col="red")

points(test, M700, pch = 20, col="green")
points(train700, Emul700[[m]][train700,2], pch = 20, col="yellow")

X <- Emul80[[m]][test, ]
Y <- Emul700[[m]][test,]
y <- Gas.Sim[test,m]

# Histograms of errors
par(mfrow=c(1,2))
hist(X[,1]- y)
hist(Y[,1]- y)

par(mfrow=c(1,1))
plot(abs(X[,1]- y), abs(Y[,1]- y)); abline(0,1, col="red")

par(mfrow=c(1,2))
hist(X[,2])
hist(Y[,2])

hist( (X[,1]-y)/sqrt(X[,2]))
hist( (Y[,1]-y)/sqrt(Y[,2]))


#############     LINEAR REGRESSION      #####################

month <- 1

y80 <- Gas.Sim[train80, month]
regr80 <- Regressors80[[month]]
fit80 <- lm(y80~., data = as.data.frame(Interactions80[, regr80]) )

y700 <- Gas.Sim[train700, month]
regr700 <- Regressors700[[month]]
fit700 <- lm(y700~., data = as.data.frame(Interactions700[, regr700]) )

test.regr <- predict(Interactions80, newdata = Design[test, ])[,regr80]
y.pred80 <- predict(fit80, newdata = as.data.frame(test.regr))

test.regr <- predict(Interactions700, newdata = Design[test, ])[,regr700]
y.pred700 <- predict(fit700, newdata = as.data.frame(test.regr))

y <- Gas.Sim[test, month]

var(fit80$residuals)
var(fit700$residuals)

var(y - y.pred80)
var(y - y.pred700)


par(mfrow=c(1,2))
hist(y.pred80- y)
hist(y.pred700- y)

View(cbind(y.pred700, y, Y[,1]))
par(mfrow=c(1,2))
hist(y.pred80-X[,1])
hist(y.pred700-Y[,1])

i <- 1:30
hist(y.pred700[i]-y[i], xlim = c(-40,30))#, ylim = c(0,85))
hist(Y[i,1] -y[i], xlim = c(-40,30))#, ylim = c(0,85))

plot( abs(y.pred700[i]-y[i]), abs(Y[i,1]-y[i])); abline(0,1, col="red")

hist(X[,2])
hist(Y[,2])

hist( (X[,1]-y)/sqrt(X[,2]))
hist( (Y[,1]-y)/sqrt(Y[,2]))

par(mfrow=c(1,2))
plot(X[,1]-y, ylim = 1.1*c(-1,1))
plot(Y[,1]-y, ylim = 1.1*c(-1,1))




################################################################################
# GOODNESS OF LM MODEL AS TRAINING SIZE DECREASES
#

load('RData/Inputs/Design_Points.RData')               # Design Points
load('RData/Inputs/Simulated_and_Observed_Gas.RData')  # Gas data (observed and simulated)

assess_var <- function(s, month, npred){
  train <- sort(sample(1:1000, s))        
  Interactions <- poly(data.matrix(Design[train,]), degree=2)
  
  y.train <- Gas.Sim[train, month]
  L <- summary(regsubsets(y.train~., data=Interactions, method = "exhaustive", nvmax = npred))
  ind <- which.max(L$adjr2)
  regr <- which(L$which[ind, -1])
  fit <- lm(y.train ~ ., data = as.data.frame(Interactions[, regr]) )
  
  test <- sample((1:1000)[-train], 200)
  test.regr <- predict(Interactions, newdata = Design[test, ])[,regr]
  y.pred <- predict(fit, newdata = as.data.frame(test.regr))
  y <- Gas.Sim[test, month]
  
  return( var(y.pred - y) )
}

Sz <- seq(log(30), log(800), len = 10)
Sz <- rev( round(exp(Sz)) )
n <- 50
vars <- rep(0, length(Sz))

for (i in 1:length(Sz)){
  s <- Sz[i]
  vars[i] <- 0
  for (j in 1:n){
    vars[i] <- vars[i] + assess_var(s, month = 1, npred = 6)
  }
  print(i)
  vars[i] <- vars[i]/n
}

#vars6 <- vars
vars8 <- vars
vars10 <- vars

################################################################################
# Plot picture after having run the above for loop with 6, 8, and 10 predictors

png(filename = "LR_Variance2.png", width = 8, height = 6, units = "in", res = 288)
plot(Sz, sqrt(vars10), ty = "b", col = "red", ylim = c(0,35), 
     ylab = "Standard deviation of errors on test set", xaxt = "n", xlab = "Training set size",
     main = "Each dot obtained by averaging variance over 50 training sets of size 200",
     cex.main = 0.95)
points(Sz, sqrt(vars8), ty = "b", col = "blue")
points(Sz, sqrt(vars6), ty = "b", col = "green")
lbl <- seq(0, 800, 50)
lbl[seq(2, 17, 2)] <- NA
axis(1, at = seq(0, 800, 50), labels = lbl)
legend("topright", legend = paste(c(6, 8, 10), "predictors"),
       col = c("green", "blue", "red"), lty = 1)
dev.off()


##############################################################################


