# COMPARE COEFFICIENTS FOR THE LINEAR REGRESSION MODELS AT THE BASE OF THE EMULATORS


setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/')

# LOAD LIBRARIES AND CUSTOM FUNCTIONS
library(openxlsx)    
library(leaps)
library(randtoolbox)
source('Scripts/Auxiliary_Functions.R')


#######################################################################
##  LOAD THE DATA: SIMULATION INPUTS, SIMULATION OUTPUTS, OBSERVATIONS  
#######################################################################

month.names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

source('Scripts/Load_data.R')

## RESCALE INPUTS LINEARLY WITHIN [-1,1]  
Design <- Rescale.Linearly(Design) 


####################################################
##   RUN LINEAR REGRESSIONS AND STORE SOME RESULTS
####################################################

Interactions <- poly(as.matrix(Design), degree=2)

## Choose always same regressors (the ones already chosen for Jan-Apr + Nov,Dec)
y <- Outputs_gas[,1]
L <- summary(regsubsets(y~., data=Interactions, method = "exhaustive", nvmax = 10))
regr.same <- L$which[10,-1]            # logical vector with regressors corresponding to selected model
coefs <- matrix(nrow=0, ncol=11)
for (month in 1:12){
  y <- Outputs_gas[, month]
  fit <- lm(y~ ., data = as.data.frame(Interactions[,regr.same]))
  coefs <- rbind(coefs, fit$coefficients)
}
rownames(coefs) <- month.names
View(coefs)


## Compute R^2 of regressing set of coefficients for a month against the other

Rsq <- matrix(NA, nrow = 12, ncol = 12)
colnames(Rsq) <- month.names
rownames(Rsq) <- month.names
indices <- c(1,2,3,4,11,12)
for (m1 in indices){
  for(m2 in indices){
    fit <- lm(coefs[m1,-1]~coefs[m2,-1]-1)
    Rsq[m1,m2] <- summary(fit)$r.squared
  }
}

# UT: indices of upper triangular matrix
UT <- upper.tri(Rsq, diag = F)
vec.corr <- Rsq[UT]
vec.corr <- vec.corr[!is.na(vec.corr)]
summary(vec.corr)



#####################
##     SAVE PLOTS
#####################

# Plot LR coefficients for different months against each other, and save them as pdfs
indices <- c(1,2,3,4,11,12)
base.str <- 'Pictures/Compare_LRs/LR_'

for (m1 in indices){
  # Select only months after the current one
  m2.ind <- indices[indices>m1]
  for (m2 in m2.ind){
    str <- paste(base.str, month.names[m1], '_', month.names[m2], '.pdf', sep = '')
    # Run LR without intercept term and store slope of line y=mx
    slope <- lm(coefs[m2, -1] ~ coefs[m1,-1] -1)$coefficient
    pdf(str)

    # Set plot frame, labels, fontsizes
    plot(coefs[m1,-1], coefs[m2,-1], 'n',
         xlab = paste(month.name[m1], "'s coeffs", sep=''),
         ylab = paste(month.name[m2], "'s coeffs", sep=''),
         cex.lab=1.5, cex.axis=1.2)
    box(lwd=1.7)
    # Add regression line
    abline(a=0, b = slope, col = "red", lty = "dashed", lwd=2)
    # Add points except for intercept
    points(coefs[m1,-1], coefs[m2,-1], cex=2.6, pch=24, col = "blue", bg = "green", lwd = 1.4)
    # Add title
    title(sprintf("m = %1.3f", slope), line = 1, cex.main=1.8)
    # Save plot
    dev.off()
  }
}



# The same as above, but with intercept coefficient included in plot
indices <- c(1,2,3,4,11,12)
base.str <- 'Pictures/Compare_LRs/LR_'

for (m1 in indices){
  # Select only months after the current one
  m2.ind <- indices[indices>m1]
  for (m2 in m2.ind){
    str <- paste(base.str, month.names[m1], '_', month.names[m2], '_with_intercept.pdf', sep = '')
    # Run LR without intercept term and store slope of line y=mx
    slope <- lm(coefs[m2, -1] ~ coefs[m1,-1] -1)$coefficient
    pdf(str)
    
    # Set plot frame, labels, fontsizes
    plot(coefs[m1,-1], coefs[m2,-1], 'n',
         xlab = paste(month.name[m1], "'s coeffs", sep=''),
         ylab = paste(month.name[m2], "'s coeffs", sep=''),
         cex.lab=1.5, cex.axis=1.2)
    box(lwd=1.7)
    # Add regression line
    abline(a=0, b = slope, col = "red", lty = "dashed")
    # Add points except for intercept
    points(coefs[m1,-1], coefs[m2,-1], cex=2.6, pch=24, col = "blue", bg = "green", lwd = 1.4)
    # Add intercept point
    points(coefs[m1,1], coefs[m2,1], cex=2.6, pch=24, col = "blue", bg = "red", lwd = 1.4)
    # Add title
    title(sprintf("m = %1.3f", slope), line = 1, cex.main=1.8)
    # Save plot
    dev.off()
  }
}



m1 <- 1
m2 <- 3
slope <- lm(coefs[m2, -1] ~ coefs[m1,-1] -1)$coefficient
intercept <- coefs[m2, 1] - slope*coefs[m1,1]
plot(Outputs_gas[,m1], Outputs_gas[,m2], xlim = c(0,5500), ylim = c(0, max(Outputs_gas[,m2])))
abline(a=0, b=slope, col='red')










