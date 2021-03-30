# This script reads simulated temperatures from Data/Temperature_Data/hourly-results
# and saves them as two matrices in Simulated-Temperatures.RData.

setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/')

N.sim <- 1000
N.steps <- 8760
# These variables will contain simulated kitchen and master temperatures for all 1000 runs
Kitch.Sim  <- matrix(0, N.steps, N.sim)
Master.Sim <- matrix(0, N.steps, N.sim)

filename.base <- "Data/Temperature_Data/hourly-results/"
# The for loop takes about 3 seconds each 100 files
for (i in 1:1000){
  file <- paste(filename.base, as.character(i), '-var.csv', sep = "")
  sample <- read.csv(file)
  Kitch.Sim[,i]  <- sample[,2]
  Master.Sim[,i] <- sample[,3]
}
save(Kitch.Sim, Master.Sim, file = 'RData/Simulated-Temperatures.RData')


