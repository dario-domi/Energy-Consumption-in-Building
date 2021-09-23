#################################################################################
#
# This script, or parts thereof, should be run after 
# Build_and_Validate_Gas_Emulators.R.
# It will produce plots to test the performance of the gas consumption emulators
# on points not used in training.
#
################################################################################

month.vector <- c(1:5, 9:12)

################################################################################
# SCATTER PLOT OF EMULATED STANDARDISED RESIDUALS VS FITTED VALUES

# Colors of scattered points and line of observed gas value

col_points <- "chartreuse3"
col_obs    <- "#FF5500FF"

xlabel <- expression(plain("Emulated Values ")*hat(y)[i])
ylabel <- expression(plain("Standardised Errors ")*hat(epsilon)[i])#*plain(" (Validation)"))

y.limits.eval <- c(4.0, 3.5, 3.5, 4.0, 5.0, NA, NA, NA, 3.3, 3.5, 4.2, 4.0)
y.limits.test <- c(3.7, 3.5, 3.0, 3.0, 4.5, NA, NA, NA, 4.0, 3.5, 3.0, 3.0)

for (month in month.vector){
  
  th <- paste(month.name[month]) # title
  
  # PLOT OF EVALUATION SET
  
  # Errors to be plotted
  X <- Emul.Eval[[month]]                 # Emulated mean and var
  y.eval <- Gas.Sim[eval, month]          # Actual simulator outputs
  err.std <- (y.eval- X[,1])/sqrt(X[,2])  # Standardised errors
  
  # File name and plot title
  file.name <- paste("../Pictures/Validation_Plots/Evaluation_Set/", "Evaluation_Scatter_", 
                     formatC(month, width=2, flag="0"), "_", month.names[month], ".pdf", sep = "")
  
  # Actual scatter plot
  pdf(file.name)#, width = 7, height = 5)
  par(cex =1.6, lwd=1.5, mgp = c(2,0.5,0))
  plot(X[,1], err.std, type = "n", xlab = xlabel, ylab = ylabel, ylim = y.limits.eval[month]*c(-1,1))
  abline(h = 0, col = "darkolivegreen4", lty = 1, lwd = 1.5)
  abline(v = Gas.Obs[month], col = col_obs, lty = 2, lwd = 3.5)
  points(X[,1], err.std, pch = 21, bg = col_points)
  title(th, line = 1)
  dev.off()
  
  # PLOT OF TEST SET
  
  # Errors to be plotted
  X <- Emul.Test[[month]]                 # Emulated mean and var
  y.test <- Gas.Sim[test, month]          # Actual simulator outputs
  err.std <- (y.test - X[,1])/sqrt(X[,2])  # Standardised errors
  
  # File name and plot title
  file.name <- paste("../Pictures/Validation_Plots/Test_Set/", "Test_Scatter_", 
                     formatC(month, width=2, flag="0"), "_", month.names[month], ".pdf", sep = "")
  
  # Actual scatter plot
  pdf(file.name)#, width = 7, height = 5)
  par(cex =1.6, lwd=1.5, mgp = c(2,0.5,0))
  plot(X[,1], err.std, type = "n", xlab = xlabel, ylab = ylabel, ylim = y.limits.test[month]*c(-1,1))
  abline(h = 0, col = "darkolivegreen4", lty = 1, lwd = 1.5)
  abline(v = Gas.Obs[month], col = col_obs, lty = 2, lwd = 3.5)
  points(X[,1], err.std, pch = 21, bg = col_points)
  title(th, line = 1)
  dev.off()
  
}

################################################################################
# LINEAR REGRESSION RESIDUAL VARIANCE
# AND
# 2.5 AND 97.5 PERCENTILES OF EMULATOR VARIANCES ON VALIDATION POINTS


Res.Var <- array(0, 12)
R.sqr <- array(0, 12)

for (month in month.vector){
  y.train <- Gas.Sim[train, month]
  
  regr <- Regressors[[month]]
  fit <- lm(y.train ~ ., data = as.data.frame(Interactions.train[, regr]))
  Res.Var[month] <- var(fit$residuals)
  R.sqr[month] <- summary(fit)$r.squared
}


for (month in month.vector){
  v <- Emul.Test[[month]][,2]
  q1 <- quantile(v, 0.025)
  q2 <- quantile(v, 0.975)
  cat('Quantiles for', month.name[month], ':', q1, '-', q2, '\n')
}


################################################################################
# HISTOGRAMS OF CORRELATION ACROSS SPACE

library(randtoolbox)
N <- 2.e6
Points <- 2*sobol(N, dim = dim(Design)[2], scrambling = 1) -1 

for (month in month.vector){
  Active.Inputs <- Act_inputs[[month]]              # Active Inputs
  N_Act <- length(Active.Inputs)
  d <- Corr_lengths[month] * replicate(N_Act, 1)    # Correlation lengths
  
  Center <- matrix(0, 1, N_Act)
  c <- Corr.fun(Center, Points[, Active.Inputs], d, 'exp2')
  
  th <- paste(month.name[month])
  file.name <- paste("../Pictures/Validation_Plots/Correlation/", "Correlation_", 
                     formatC(month, width=2, flag="0"), "_", month.names[month], ".pdf", sep = "")
  
  pdf(file.name)
  par(cex =1.6, lwd=1.5, mgp = c(2,0.5,0))
  if (month!=2){
    h <- hist(c, breaks = 400, main = th, xlab = "correlation", col =  "chartreuse3", xlim = c(0,1), 
              freq = F, border = NA)
  } else{
    h <- hist(c, breaks = 400, main = th, xlab = "correlation", col =  "chartreuse3", xlim = c(0,0.5), 
              freq = F, border = NA)
  }
  # Add a line at the top of the density plot
  lines(rep(h$breaks, each=2)[-c(1,2*length(h$breaks))], rep(h$density, each=2), lwd=3)
  
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
y.eval <- Gas.Sim[eval, month]
y.test <- Gas.Sim[test, month]

# Linear regression predictions at validation points
regr <- Regressors[[month]]                       
fit <- lm(y.train ~ ., data = as.data.frame(Interactions.train[, regr]))
All.Regr.Test <- predict(Interactions.train, newdata = data.matrix(Design[test, ]))
LRpreds <- predict(fit, as.data.frame(All.Regr.Test[, regr]))

# Prepare variables to plot cats eye of mean and std of emulator predictions
n.std <- 3
X <- Emul.Test[[month]]
# Matrix containing emulator mean predictions and +/- 3 std from it
# (the corrections "/sqrt(3)" due to way the error.bars() function handles data
data <- rbind(X[,1] + n.std*sqrt(X[,2])/sqrt(3),
              X[,1],
              X[,1] - n.std*sqrt(X[,2])/sqrt(3))
# Sort test indices in increasing order of y.test values 
sorted <- sort(y.test, index.return=T)$ix

# Now plot each emulator prediction +/- 3 std for the test set, the actual test
# values, and the linear regression prediction
i <- sorted[82:89]
L <- length(i)
col.em <- rgb(1, 0.4, 0.4)

file.name <- paste("../Pictures/Validation_Plots/Comparison_LR/", "LR_", 
                   month.names[month], "_82-89", ".pdf", sep = "")

# Plots
# Store min and max y-values
m <- min( c(X[i,1] - n.std*sqrt(X[i,2]), LRpreds[i], y.test[i]) )
M <- max( c(X[i,1] + n.std*sqrt(X[i,2]), LRpreds[i], y.test[i]) )

pdf(file.name)
par(cex =1.4, lwd=1.5, mgp = c(2,0.5,0))

# 1) Emulator predictions shown as cat eyes plot
error.bars(data[,i], alpha = 0.0954659663, eyes=T, col=col.em,
           main = 'Emulator and regression predictions',
           xlab = 'Simulation Index',
           ylab = 'Implausibility Measure',
           ylim = c(m,M)
           )
# 2) Add linear regression predictions
points(1:L, LRpreds[i], pch=23, col = 'black', bg = 'yellow', lwd=1.7, cex = 1.3)
# 3) Add real y.test values (star is pch=8b)
points(1:L, y.test[i], pch=43, cex=1.3, col='blue')
legend('topleft', legend = c('Simulator', 'Emulator', 'Linear regression'), 
       pch = c(43, 22, 23), col = c('blue', 'black', 'black'), 
       pt.bg = c(NA, col.em, 'yellow'),  pt.cex= c(1.5, 1.5, 1.3), 
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
