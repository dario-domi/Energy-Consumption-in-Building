###############################################################################
#
# Builds linear regressions for the monthly gas consumptions by selecting the 
# "best" regressors among choices of quadratic terms of the 8 inputs
#
###############################################################################

# SET FOLDER AND LOAD SCRIPTS/DATA
setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')

library(leaps)
load('RData/Inputs/Design_Points.RData')                 # Design Points
load('RData/Inputs/Simulated_and_Observed_Gas.RData')    # Gas data (observed and simulated)
source('../../Emulation.R')                              # Script to carry out Emulation

# FIX TRAINING AND VALIDATION SETS
rm(train, valid, test)
set.seed(5879, kind = "default")
train <- sample(1:1000, 800)                             # Training set, 800 points
valid <-(1:1000)[-train]                                 # Validation set, 200 points


###############################################################################
# BUILD EMULATORS AND PREDICT VALUES ON VALIDATION SET

load('RData/Inputs/Regressors.RData')               # Regressors
source('Auxiliary_Scripts/Emulation_Parameters.R')  # Active Inputs and Correlation lengths for each month

Interactions.train <- poly(data.matrix(Design[train,]), degree=2) # orthogonal polys computed on 800 points only
Emul.Validation <- sapply(month.names, function(x) NULL)          # will contain emulated predictions on validation set

month.vector <- c(1:5, 9:12)

for (month in month.vector){
  y.train <- Gas.Sim[train, month]
  
  # Linear Regression
  regr <- Regressors[[month]]                       # logical vector with regressors to be used
  fit <- lm(y.train ~ ., data = as.data.frame(Interactions.train[, regr]))
  beta <- fit$coefficients
  
  # Active inputs and Correlation Lengths
  Active.Inputs <- Act_inputs[[month]]              # Active Inputs
  N_Act <- length(Active.Inputs)                         
  d <- Corr_lengths[month] * replicate(N_Act, 1)    # Correlation lengths

  # Training and Validation Variables
  ActInp.Train <- Design[train, Active.Inputs, drop=F]          # Design points used to train the emulator
  ActInp.Valid <- Design[valid, Active.Inputs, drop=F]          # Validation points
  Regr.Train   <- cbind(1, Interactions.train[, regr, drop=F])  # Regressors, training: add a column of 1s for intercept
  Regr.Valid   <- predict(Interactions.train, newdata = data.matrix(Design[valid,])) 
  Regr.Valid   <- cbind(1, Regr.Valid[, regr, drop=F])          # Regressors, validation set
  
  # Prior variances (explained and residual) used in the emulator
  sigma2.tot <- var(fit$residuals)                       # Prior cumulative variance of emulator+nugget
  nugget <- 0.05                                         # Fraction of residual variability not explained by regressors
  sig2 <- (1-nugget)*sigma2.tot                          # Variance of 2nd-order process
  nu2 <- nugget*sigma2.tot                               # Variance of nugget term (Gaussian noise)
  
  Emul.Validation[[month]] <- BL.Emul(ActInp.Train, ActInp.Valid, y.train, 
                                      Regress.Design = Regr.Train, 
                                      Regress.Test = Regr.Valid, 
                                      beta = beta,
                                      sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)
}



################################################################################
# SCATTER PLOT OF EMULATED STANDARDISED RESIDUALS VS FITTED VALUES

# Colors of scattered points and line of observed gas value

col_points <- "chartreuse3"
col_obs    <- "#FF5500FF"

xlabel <- expression(plain("Emulated Values ")*hat(y)[i])
ylabel <- expression(plain("Standardised Errors ")*hat(epsilon)[i])#*plain(" (Validation)"))

y.limits <- c(4, 3.5, 3, 3.5, 5, NA, NA, NA, 3.5, 3, 3, 3)

for (month in month.vector){
  # Errors to be plotted
  X <- Emul.Validation[[month]]            # Emulated mean and var
  y.valid <- Gas.Sim[valid, month]         # Actual simulator outputs
  err.std <- (y.valid- X[,1])/sqrt(X[,2])  # Standardised errors
  
  # File name and plot title
  file.name <- paste("../Pictures/Validation_Plots/", "Validation_Scatter_", 
                     formatC(month, width=2, flag="0"), "_", month.names[month], ".pdf", sep = "")
  th <- paste(month.name[month])
  
  # Actual plot
  pdf(file.name)#, width = 7, height = 5)
  par(cex =1.6, lwd=1.5, mgp = c(2,0.5,0))
  plot(X[,1], err.std, type = "n", xlab = xlabel, ylab = ylabel, ylim = y.limits[month]*c(-1,1))
  abline(h = 0, col = "darkolivegreen4", lty = 1, lwd = 1.5)
  abline(v = Gas.Obs[month], col = col_obs, lty = 2, lwd = 3.5)
  points(X[,1], err.std, pch = 21, bg = col_points)
  title(th, line = 1)
  dev.off()
}

################################################################################
# COMPUTE 2.5 AND 97.5 PERCENTILES OF EMULATOR VARIANCES ON VALIDATION POINTS

for (month in month.vector){
  v <- Emul.Validation[[month]][,2]
  q1 <- quantile(v, 0.025)
  q2 <- quantile(v, 0.975)
  cat('Quantiles for', month.name[month], ':', q1, '-', q2, '\n')
}


################################################################################
# HISTOGRAMS OF CORRELATION ACROSS SPACE

for (month in month.vector){
  Active.Inputs <- Act_inputs[[month]]              # Active Inputs
  N_Act <- length(Active.Inputs)
  d <- Corr_lengths[month] * replicate(N_Act, 1)    # Correlation lengths
  
  Samp <- matrix(0, 1, N_Act)
  c <- Corr.fun(Samp, Design[, Active.Inputs], d, 'exp2')

  th <- paste(month.name[month])
  file.name <- paste("../Pictures/Validation_Plots/Correlation/", "Correlation_", 
                     formatC(month, width=2, flag="0"), "_", month.names[month], ".pdf", sep = "")
  
  pdf(file.name)
  par(cex =1.6, lwd=1.5, mgp = c(2,0.5,0))
  hist(c, breaks = 40, main = th, xlab = "correlation", col =  "chartreuse3", xlim = c(0,1))
  dev.off()
}

################################################################################
# COMPARISON WITH LINEAR REGRESSION (cats eye plot)

library(psych) # needed to use error.bars


# march, 111:120
# novem, 31:40
month <- 3

# Train and validation outputs
y.train <- Gas.Sim[train, month]
y.valid <- Gas.Sim[valid, month]

# Linear regression predictions at validation points
regr <- Regressors[[month]]                       
fit <- lm(y.train ~ ., data = as.data.frame(Interactions.train[, regr]))
All.Regr.Train <- predict(Interactions.train, newdata = data.matrix(Design[valid, ]))
LRpreds <- predict(fit, as.data.frame(All.Regr.Train[, regr]))

# Prepare variables to plot cats eye of mean and std of emulator predictions
n.std <- 3
X <- Emul.Validation[[month]]
# Matrix containing emulator mean predictions and +/- 3 std from it
# (the corrections "/sqrt(3)" due to way the error.bars() function handles data
data <- rbind(X[,1] + n.std*sqrt(X[,2])/sqrt(3),
              X[,1],
              X[,1] - n.std*sqrt(X[,2])/sqrt(3))
# Sort test indices in increasing order of y.valid values 
sorted <- sort(y.valid, index.return=T)$ix

# Now plot each emulator prediction +/- 3 std for the test set, the actual test
# values, and the linear regression prediction
i <- sorted[111:120]
L <- length(i)
col.em <- rgb(1, 0.4, 0.4)

file.name <- paste("../Pictures/Validation_Plots/Comparison_LR/", "LR_", 
                   month.names[month], "_111-120", ".pdf", sep = "")
# Plots
pdf(file.name)
par(cex =1.4, lwd=1.5, mgp = c(2,0.5,0))
# 1) Emulator predictions shown as cat eyes plot
error.bars(data[,i], alpha = 0.0954659663, eyes=T, col=col.em,
           main = 'Emulator and regression predictions',
           xlab = 'Simulation Index',
           ylab = 'Implausibility Measure'
)
# 2) Add real y.test values
points(1:L, y.valid[i], pch=8, cex=1.3, col='blue')
# 3) Add linear regression predictions
points(1:L, LRpreds[i], pch=23, col = 'black', bg = 'yellow', lwd=1.9, cex = 1.3)
legend('topleft', legend = c('Simulator', 'Emulator', 'Linear regression'), 
       pch = c(8, 22, 23), col = c('blue', 'black', 'black'), 
       pt.bg = c(NA, col.em, 'yellow'),  pt.cex= c(1, 1.5, 1.3), 
       bg = 'aliceblue')
dev.off()



##############################################################################

#                       END OF THE SCRIPT

##############################################################################














###############################################################################

## FIND THE OPTIMAL PARAMETERS AUTOMATICALLY....
N_var <- length(Active.Inputs)
# Initial values of d, s2 and nugget for optimisation
d0 <- 1*replicate(N_var, 1)       # Correlation lengths
s2.tot0 <- var(fit$residuals)       # Prior cumulative variance of homoschedastic process
nug_frac0 <- 0.05                   # Fraction of variability not explained by regressors
kernel <- 'exp2'
L = length(beta)

source('Auxiliary_Scripts/Find_Optimal_Params.R')
Opt.pars2 <- Find.Optimal.Parameters(ActInp.Train, Regr.Train,
                                    ActInp.Valid, Regr.Valid,
                                    y.train, y.valid,
                                    beta, matrix(0, L, L),
                                    kernel, d0, s2.tot0, nug_frac0, 
                                    same.corr.lengths = T)

# ...AND EVALUATE OPTIMISED EMULATOR

sig2 <- Opt.pars$s2
nu2  <- Opt.pars$nu2
d    <- Opt.pars$d

sig2
nu2
d

Emul.Valid.opt <- BL.Emul(ActInp.Train, ActInp.Valid, y.train, 
                      Regress.Design = Regr.Train, 
                      Regress.Test = Regr.Valid, 
                      beta = beta, Cov.beta = matrix(0, L, L),
                      sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)

X <- Emul.Valid
Y <- Emul.Valid.opt

err <- y.valid- X[,1]
err.std <- err/sqrt(X[,2])
err.opt <- y.valid- Y[,1]
err.std.opt <- err/sqrt(Y[,2])

hist(err, breaks = 20)
hist(err.std, breaks = 20)
hist(err.opt, breaks = 20)
hist(err.std.opt, breaks = 20)


##############################################################################


#############################################################################

# New inputs
Test.points <- Eval.points.full[1:1000,]
Test.ActInp <- Test.points[, Active.Inputs, drop=F]
Test.regr <- predict(Interactions.Design, newdata = Test.points)
Test.regr <- as.data.frame( cbind(1, Test.regr[, regr]) )

sig2 <- Opt.pars$s2
nu2  <- Opt.pars$nu2
d    <- Opt.pars$d

Emul.Eval <- BL.Emul(ActInp.Train, Test.ActInp, y.train, 
                          Regress.Design = Regr.Train, 
                          Regress.Test = Test.regr, 
                          beta = beta, Cov.beta = Cov.beta, 
                          sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)

d <- Corr_lengths[month]* replicate(N_var, 1)         # Correlation lengths
sigma2.tot <- var(fit$residuals)                 # Prior cumulative variance of homoschedastic Gaussian process
nugget <- 0.05                                  # Fraction of residual variability not explained by regressors
nu2 <- nugget*sigma2.tot                                 # Variance of nugget term (Gaussian noise)
sig2 <- (1-nugget)*sigma2.tot 

Emul.Eval2 <- BL.Emul(ActInp.Train, Eval.ActInp, y.train, 
                     Regress.Design = Regr.Train, 
                     Regress.Test = Eval.regr, 
                     beta = beta, Cov.beta = Cov.beta, 
                     sigma2 = sig2, kernel = 'exp2', d = d, nu2 = nu2)

View(cbind(Emul.Test[,1], sqrt(Emul.Test[,2])))
summary( (y.eval-Emul.Eval[,1])/sqrt(Emul.Eval[,2]))
hist((y.eval-Emul.Eval[,1])/sqrt(Emul.Eval[,2]))
qqnorm((y.eval-Emul.Eval[,1])/sqrt(Emul.Eval[,2]))


y.eval <- Gas.Sim[test, month]
View(cbind(Emul.Eval[,1], sqrt(Emul.Eval[,2]), y.eval))
summary( (y.eval-Emul.Eval[,1])/sqrt(Emul.Eval[,2]))
hist((y.eval-Emul.Eval[,1])/sqrt(Emul.Eval[,2]))
qqnorm((y.eval-Emul.Eval[,1])/sqrt(Emul.Eval[,2]))

####################################################################

#################################################################################

# PREPARE EMULATION VARIABLES, WITH POLY BUILT ON 1000 POINTS, OLD TRAIN SET

Interactions.Design <- poly(data.matrix(Design), degree=2)
Interactions.train <- as.data.frame(Interactions.Design[train, ])  # subset of interactions on 1000 points

L <- summary(regsubsets(y.train~., data=Interactions.train, method = "exhaustive", nvmax = 10))
ind <- which.max(L$adjr2)          # index (between 1 and nvmax) of model with max adj-R2
regr <- L$which[ind,-1]            # logical vector with regressors corresponding to selected model
fit <- lm(y.train ~ ., data = as.data.frame(Interactions.train[, regr]))
summary(fit)

# Regression coefficients mean and covariance
beta <- fit$coefficients
Cov.beta <- vcov(fit)

# Evaluation points and active inputs
Active.Inputs <- GP_inputs[[month]]   #c(1,2,3,4,6)  

ActInp.Train  <- Design[train, Active.Inputs, drop=F]             # design points used to train the emulator
ActInp.Valid  <- Design[valid, Active.Inputs, drop=F]
Regr.Train    <- cbind(1, Interactions.Design[train, regr, drop=F]) # regressors: add a column of 1s for intercept
Regr.Valid    <- cbind(1, Interactions.Design[valid, regr, drop=F]) # regressors: add a column of 1s for intercept

