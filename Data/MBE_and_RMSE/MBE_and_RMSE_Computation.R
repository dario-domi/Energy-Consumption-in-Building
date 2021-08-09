##############################################################################

# Compute MBE and CV(RMSE) for 1000 simulations, wrt:
# a) gas consumption,  monthly and yearly (1 obs per month)
# b) elec consumption, monthly and yearly (1 obs per month)
# c) kitchen temperature, monthly and yearly (hourly time series)
# d) master temperature,  monthly and yearly (hourly time series).
# Results are saved in xlsx files, within the folder UQ_Energy_Building/Data/MBE_and_RMSE

##############################################################################

# Set folder and load data #
setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')

library(xlsx)
load('RData/Inputs/Simulated_and_Observed_Gas.RData')
load('RData/Inputs/Simulated_and_Observed_Elec.RData')
load('RData/Inputs/Simulated_and_Observed_Temperatures.RData')
Kitch.Sim <- t(Kitch.Sim) # 1000 x 8760
Mast.Sim <- t(Mast.Sim)   # 1000 x 8760


#########################################################
# Monthly and yearly computation of MBE/CVRMSE for Gas
#########################################################

# Monthly MBE (just one obs)
Diff.Gas <- -sweep(Gas.Sim, 2, Gas.Obs) # Observed minus simulated monthly gas consumption
MBE.Gas.month <- 100*sweep(Diff.Gas, 2, FUN = '/', Gas.Obs) # Diff.Gas/Gas.Obs

# Yearly MBE
yearly.gas.sim <- apply(Gas.Sim, 1, sum) # for each simulation, sum over months
yearly.gas.obs <- sum(Gas.Obs)
MBE.Gas.year <- 100*(1 - yearly.gas.sim/yearly.gas.obs)

# Yearly MBE without summer months
summer <- c(6,7,8)
yearly.gas.sim <- apply(Gas.Sim[,-summer], 1, sum) # for each simulation, sum over months
yearly.gas.obs <- sum(Gas.Obs[-summer])
MBE.Gas.nosummer <- 100*(1 - yearly.gas.sim/yearly.gas.obs)

# Monthly CV(RMSE) Gas (just one obs)
CV.Gas.month <- abs(MBE.Gas.month)

# Yearly CV(RMSE)
N <- 12
top <- sqrt(rowSums(Diff.Gas^2)/N)       # 1000-dim vector
bottom <-  sum(Gas.Obs)/N                # one number
CV.Gas.year <- 100*top/bottom            # 1000-dim vector

# Yearly CV(RMSE) without summer months
N <- 12 - length(summer)
top <- sqrt(rowSums(Diff.Gas[,-summer]^2)/N)   # 1000-dim vector
bottom <-  sum(Gas.Obs[-summer])/N             # one number
CV.Gas.nosummer <- 100*top/bottom              # 1000-dim vector

# FINAL DATAFRAMES MBE & CV(RMSE) GAS
MBE.Gas <- data.frame(MBE.Gas.month, 
                      'Yearly'= MBE.Gas.year, 
                      'Yearly_No_Summer'=MBE.Gas.nosummer) 

CV.Gas <- data.frame(CV.Gas.month, 
                      'Yearly'= CV.Gas.year, 
                      'Yearly_No_Summer'=CV.Gas.nosummer) 


#################################################################
# Monthly and yearly computation of MBE/CVRMSE for Electricity
#################################################################

# Monthly MBE (just one obs)
Diff.Elec <- -sweep(Elec.Sim, 2, Elec.Obs) # Observed minus simulated monthly elec consumption
MBE.Elec.month <- 100*sweep(Diff.Elec, 2, FUN = '/', Elec.Obs) # Diff.Elec/Elec.Obs

# Yearly MBE
yearly.elec.sim <- apply(Elec.Sim, 1, sum) # for each simulation, sum over months
yearly.elec.obs <- sum(Elec.Obs)
MBE.Elec.year <- 100*(1 - yearly.elec.sim/yearly.elec.obs)

# Yearly MBE without summer months
summer <- c(6,7,8)
yearly.elec.sim <- apply(Elec.Sim[,-summer], 1, sum) # for each simulation, sum over months
yearly.elec.obs <- sum(Elec.Obs[-summer])
MBE.Elec.nosummer <- 100*(1 - yearly.elec.sim/yearly.elec.obs)

# Monthly CV(RMSE) (just one obs)
CV.Elec.month <- abs(MBE.Elec.month)

# Yearly CV(RMSE)
N <- 12
top <- sqrt(rowSums(Diff.Elec^2)/N)       # 1000-dim vector
bottom <-  sum(Elec.Obs)/N                # one number
CV.Elec.year <- 100*top/bottom            # 1000-dim vector

# Yearly CV(RMSE) without summer months
N <- 12 - length(summer)
top <- sqrt(rowSums(Diff.Elec[,-summer]^2)/N)   # 1000-dim vector
bottom <-  sum(Elec.Obs[-summer])/N             # one number
CV.Elec.nosummer <- 100*top/bottom          # 1000-dim vector

# FINAL DATAFRAMES MBE & CV(RMSE) ELECTRICITY
MBE.Elec <- data.frame(MBE.Elec.month, 
                      'Yearly'= MBE.Elec.year, 
                      'Yearly_No_Summer'=MBE.Elec.nosummer) 

CV.Elec <- data.frame(CV.Elec.month, 
                     'Yearly'= CV.Elec.year, 
                     'Yearly_No_Summer'=CV.Elec.nosummer) 

##############################################################################


###################################################
# Yearly computations for temperature time series
###################################################

# Compute MBE for Kitchen Temperature, over the whole year
Diff.Kitch <- -sweep(Kitch.Sim, 2, Kitch.Obs) # Observed minus Simulated: 1000x8760
MBE.Kitch <- 100* rowSums(Diff.Kitch)/sum(Kitch.Obs)

# Compute MBE for Master Temperature, over the whole year
Diff.Mast <- -sweep(Mast.Sim, 2, Mast.Obs) # Observed minus Simulated: 1000x8760
MBE.Mast <- 100*rowSums(Diff.Mast)/sum(Mast.Obs)

# Compute CVRMSE for Kitchen Temperature, over the whole year
N <- dim(Kitch.Sim)[2]
top <- sqrt(rowSums(Diff.Kitch^2)/N)          # 1000-dim vector
bottom <-  sum(Kitch.Obs)/N                   # one number
CVRMSE.Kitch <- 100*top/bottom                # 1000-dim vector

# Compute CVRMSE for Master Temperature, over the whole year
top <- sqrt(rowSums(Diff.Mast^2)/N)           # 1000-dim vector
bottom <-  sum(Mast.Obs)/N                    # one number
CVRMSE.Mast <- 100*top/bottom                 # 1000-dim vector


#####################################################
# Monthly computations for temperature time series
#####################################################

# Next line: number of hours in each month, as per variable 'DateTimes'. 
# Differences between March and October due to daylight saving time, 
# between January and December due to time 0:00 of 01-Jan assigned to December.
month.hours <- c(743, 672, 743, 720, 744, 720, 744, 744, 720, 745, 720, 745)
# Vector of length 8760: each hour in the year is assigned to a month
month.groups <- rep(month.names, month.hours)

# MBE for Kitchen temperature, monthly
X <- rowsum(t(Diff.Kitch), group = month.groups)  # sum temperatures for each month
top <- t(X[month.names,])                         # 1000x12: reorder rows, and transpose
X <- tapply(Kitch.Obs, month.groups, sum)         # sum of temperatures in each month
bottom <- X[month.names]                          # put months in correct order
MBE.Kitch.Monthly <- 100*sweep(top, 2, FUN='/', bottom)  # 1000 x 12, top/bottom

# MBE for Master temperature, monthly
X <- rowsum(t(Diff.Mast), group = month.groups)   # sum temperatures for each month
top <- t(X[month.names,])                         # 1000x12: reorder rows, and transpose
X <- tapply(Mast.Obs, month.groups, sum)          # sum of temperatures in each month
bottom <- X[month.names]                         # 1000x12: reorder rows, and transpose
MBE.Mast.Monthly <- 100*sweep(top, 2, FUN='/', bottom)  # 1000 x 12, top/bottom

# CVRMSE for Kitchen temperature, monthly
X <- rowsum(t(Diff.Kitch^2), group = month.groups)
X <- t(X[month.names,])                                 # reorder rows, 1000 x 12
# The next line divides each column of X by the corresponding element of month.hours
top <- sweep(X, 2, month.hours, FUN = '/')              # 1000 x 12
top <- sqrt(top)
X <- tapply(Kitch.Obs, month.groups, sum)[month.names]  # sum of temperatures in each month
bottom <- (X/month.hours)[month.names]                  # average observed temperature per month
CVRMSE.Kitch.Monthly <- 100*sweep(top, 2, FUN='/', bottom)

# CVRMSE for Master temperature, monthly
X <- rowsum(t(Diff.Mast^2), group = month.groups)
X <- t(X[month.names,])                                 # reorder rows, 1000 x12
# The next line divides each column of X by the corresponding element of month.hours
top <- sweep(X, 2, month.hours, FUN = '/')              # 1000 x 12
top <- sqrt(top)
X <- tapply(Mast.Obs, month.groups, sum)[month.names]   # sum of temperatures in each month
bottom <- (X/month.hours)[month.names]                  # average observed temperature per month
CVRMSE.Mast.Monthly <- 100*sweep(top, 2, FUN='/', bottom)


###################################
## Save the data in xlsx files
###################################

setwd('../Data/MBE_and_RMSE')

# Gas MBE & CVRMSE
write.xlsx(MBE.Gas, file = 'Gas.xlsx', row.names=T, sheetName = 'MBE Gas (%)')
write.xlsx(CV.Gas,  file = 'Gas.xlsx', row.names=T, sheetName = 'CV(RMSE) Gas (%)',
           append = T)

# Electricity MBE & CVRMSE
write.xlsx(MBE.Elec, file = 'Electricity.xlsx', row.names=T, sheetName = 'MBE Elec (%)')
write.xlsx(CV.Elec,  file = 'Electricity.xlsx', row.names=T, sheetName = 'CV(RMSE) Elec (%)',
           append = T)

# Kitchen MBE
write.xlsx(data.frame(MBE.Kitch.Monthly, 'Yearly'= MBE.Kitch), 
           file = 'Kitchen.xlsx', row.names=T, sheetName = 'MBE Kitchen (%)')
# Kitchen CV(RMSE)
write.xlsx(data.frame(CVRMSE.Kitch.Monthly, 'Yearly'= CVRMSE.Kitch), 
           file = 'Kitchen.xlsx', row.names=T, sheetName = 'CV(RMSE) Kitchen (%)',
           append = T)

# Master MBE
write.xlsx(data.frame(MBE.Mast.Monthly, 'Yearly'= MBE.Mast), 
           file = 'Master.xlsx', row.names=T, sheetName = 'MBE Master (%)')
# Master CV(RMSE)
write.xlsx(data.frame(CVRMSE.Mast.Monthly, 'Yearly'= CVRMSE.Mast), 
           file = 'Master.xlsx', row.names=T, sheetName = 'CV(RMSE) Master (%)',
           append = T)


##############################################################################
## CHECKS CORRECTNESS OF CALCULATIONS, FOR RANDOM INDEX OF RUN AND MONTH

# Check correctness of MBE for Gas, monthly
i <- sample(1000,1)
m <- sample(12,  1)
100*(Gas.Obs[m]-Gas.Sim[i,m])/Gas.Obs[m]
MBE.Gas[i,m]

# Check correctness of MBE for Gas, yearly
i <- sample(1000,1)
100*(1 - sum(Gas.Sim[i,])/sum(Gas.Obs))
as.numeric(MBE.Gas.year[i])

# Check correctness of MBE for Gas, no summer
i <- sample(1000,1)
100*(1 - sum(Gas.Sim[i,-summer])/sum(Gas.Obs[-summer]))
as.numeric(MBE.Gas.nosummer[i])

# Check correctness of CV(RMSE) for Gas, yearly
i <- sample(1000,1)
top <- sqrt( sum( (Gas.Obs - Gas.Sim[i,])^2 ))
bottom <- sum(Gas.Obs)
100*sqrt(12)*top/bottom
as.numeric(CV.Gas.year[i])

# Check correctness of CV(RMSE) for Gas, no summer
i <- sample(1000,1)
top <- sqrt( sum( (Gas.Obs[-summer] - Gas.Sim[i,-summer])^2 ))
bottom <- sum(Gas.Obs[-summer])
100*3*top/bottom
as.numeric(CV.Gas.nosummer[i])



# Check correctness of MBE for Elec, monthly
i <- sample(1000,1)
m <- sample(12,  1)
100*(Elec.Obs[m]-Elec.Sim[i,m])/Elec.Obs[m]
MBE.Elec[i,m]

# Check correctness of MBE for Elec, yearly
i <- sample(1000,1)
100*(1 - sum(Elec.Sim[i,])/sum(Elec.Obs))
as.numeric(MBE.Elec.year[i])

# Check correctness of MBE for Elec, no summer
i <- sample(1000,1)
100*(1 - sum(Elec.Sim[i,-summer])/sum(Elec.Obs[-summer]))
as.numeric(MBE.Elec.nosummer[i])

<- # Check correctness of CV(RMSE) for Elec, yearly
i <- sample(1000,1)
top <- sqrt( sum( (Elec.Obs - Elec.Sim[i,])^2 ))
bottom <- sum(Elec.Obs)
100*sqrt(12)*top/bottom
as.numeric(CV.Elec.year[i])

# Check correctness of CV(RMSE) for Elec, no summer
i <- sample(1000,1)
top <- sqrt( sum( (Elec.Obs[-summer] - Elec.Sim[i,-summer])^2 ))
bottom <- sum(Elec.Obs[-summer])
100*3*top/bottom
as.numeric(CV.Elec.nosummer[i])


# Check correctness of MBE for Kitchen, yearly
i <- sample(1000,1)
100*sum(Kitch.Obs-Kitch.Sim[i,])/sum(Kitch.Obs)
MBE.Kitch[i]

# Check correctness of MBE for Master, yearly
i <- sample(1000,1)
100*sum(Mast.Obs-Mast.Sim[i,])/sum(Mast.Obs)
MBE.Mast[i]

# Check correctness of CV(RMSE) for Kitchen, yearly
i <- sample(1000,1)
100*sqrt(sum((Kitch.Sim[i,]-Kitch.Obs)^2)/8760)/(sum(Kitch.Obs)/8760)
CVRMSE.Kitch[i]

# Check correctness of CV(RMSE) for Master, yearly
i <- sample(1000,1)
100*sqrt(sum((Mast.Sim[i,]-Mast.Obs)^2)/8760)/(sum(Mast.Obs)/8760)
CVRMSE.Mast[i]


t <- cumsum(month.hours)
i <- sample(1000,1)
m <- sample(12,1)
ind1 <- t[m-1]+1
if (m==1){
  ind1 <- 1
}
ind2 <- t[m]

# Check correctness of MBE for Kitchen, monthly
100*sum(Kitch.Obs[ind1:ind2]-Kitch.Sim[i,ind1:ind2])/sum(Kitch.Obs[ind1:ind2])
as.numeric(MBE.Kitch.Monthly[i,m])

# Check correctness of MBE for Master, monthly
100*(1 - sum(Mast.Sim[i,ind1:ind2])/sum(Mast.Obs[ind1:ind2]))
as.numeric(MBE.Mast.Monthly[i,m])

# Check correctness of CV(RMSE) for Kitchen, monthly
100*sqrt(sum((Kitch.Sim[i,ind1:ind2]-Kitch.Obs[ind1:ind2])^2))/(sum(Kitch.Obs[ind1:ind2]))*sqrt(ind2-ind1+1)
as.numeric(CVRMSE.Kitch.Monthly[i,m])

# Check correctness of CV(RMSE) for Master, monthly
100*sqrt(sum((Mast.Sim[i,ind1:ind2]-Mast.Obs[ind1:ind2])^2))/(sum(Mast.Obs[ind1:ind2]))*sqrt(ind2-ind1+1)
as.numeric(CVRMSE.Mast.Monthly[i,m])






