# Start from folder "UQ for building energy system_tutorial paper/R codes".
# This script loads the inputs and outputs of simulations runs

Temp <- read.csv("../Data/inputs.csv")
Inputs <- Temp[, -1] # delete first column with numbers from 1 to 1000

Temp <- read.csv("../Data/results_w1.csv", skip = 2005, nrow=1001)
Outputs_Elec <- Temp[-1,] # remove baseline simulation
Outputs_Elec[,"File.Name"] <- NULL # remove first column with file name of runs
rownames(Outputs_Elec) <- 1:1000

colnm <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames(Outputs_Elec) <- colnm


# EXPLORE OUTPUTS
month=1 # 1=Jan, 2=Feb, etc
hist(Outputs_Elec[,month], breaks = 50)


# CARRY OUT SOME LINEAR REGRESSION
pVal <- data.frame(matrix(nrow = 6, ncol =12)) # will store p-values of lm fits, for all months
rownames(pVal) <- colnames(Inputs)
colnames(pVal) <- colnames(Outputs_Elec)

for (month in colnames(pVal)) { # storing p-values
  y <- Outputs_Elec[, month]
  fit <- lm(y~., data=Inputs)
  pVal[, month] <- summary(fit)$coefficients[-1,4]
}
Overview <- log(pVal)< -6


# HAVE A LOOK AT RESIDUALS
month <- 6
y <- Outputs_Elec[, month]
fit <- lm(y~., data=Inputs)
var <- 7
plot(model.matrix(fit)[,var], fit$residuals)
abline(0,0, col = "red")

plot(y, fit$residuals)

fit <- lm(y~ V1+V2+V3+V4+V5+ I(sqrt(V6)), data=Inputs)
fit <- lm(y~ V1+V2+V3+V4+V5, data=Inputs)
var <- 5
plot(model.matrix(fit)[,var], fit$residuals)
abline(0,0, col = "red")

plot(fit$model[,var], y)

