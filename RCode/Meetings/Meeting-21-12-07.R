##################################################
## PRELIMINARY ACTIONS: LOAD FUNCTIONS AND DATA
##################################################

# Set folder
setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')

# Load gas data, and emulated results with associated inputs
load('RData/Inputs/Simulated_and_Observed_Gas.RData')         # Gas data (observed and simulated)
rm(Design.Original, Gas.Sim)

# Load custom function to compute implausibility measure
source("../../Emulation.R")
rm(BL.Emul, Corr.fun)


###########################################################################
# DIFFERENCES IN EMULATORS BY CHOOSING DIFFERENT 100-STRONG TRAINING SETS
###########################################################################


Test.Set <- Design[701:1000,]

set.seed(9211, kind = "default")  # 6321
train <- sort(sample(1:700, 100))
source("Emulation_Small_TestSet.R")
Emul1 <- Emul

set.seed(450, kind = "default")  
train <- sort(sample(1:700, 100))
source("Emulation_Small_TestSet.R")
Emul2 <- Emul

set.seed(4829, kind = "default")  
train <- sort(sample(1:700, 100))
source("Emulation_Small_TestSet.R")
Emul3 <- Emul

#file.name <- paste0("Meetings/Pic_different_trainings/Diff_training_", formatC(m, width=2, flag="0"), "_", month.names[m], ".png")

file.name <- "Meetings/Pic_different_trainings/Diff_training.png"
png(file.name, width = 6, height = 18, unit="in", res=288)
par(mfrow=c(9,3), 
    cex.lab = 1.2, cex.axis =1.2, cex.main = 1.4,
    mai=c(0.5, 0.5, 0.3, 0.1), # for each subplot: bott, left, top, right margins
    mgp = c(2.8,1,0)         # mgp[1] = dist label-axis; mgp[2] = dist numb-axis
)

for (m in c(1:5,9:12)) {
  
  y <- Gas.Sim[701:1000,m]
  a1 <- (Emul1[[m]][,1]-y)/sqrt(Emul1[[m]][,2])
  a2 <- (Emul2[[m]][,1]-y)/sqrt(Emul2[[m]][,2])
  a3 <- (Emul3[[m]][,1]-y)/sqrt(Emul3[[m]][,2])
  
  hist(a1, xlab = "", ylab = "", main = paste(month.names[m], '1'))
  hist(a2, xlab = "", ylab = "", main = paste(month.names[m], '2'))
  hist(a3, xlab = "", ylab = "", main = paste(month.names[m], '3'))
}
dev.off()
rm(Emul)


########################################################################
# PRETEND TO ONLY HAVE 100 TRAINING RUNS: 
# HOW MANY OF THE ORIGINAL 1000 ARE NON-IMPLAUSIBLE?
########################################################################



#########################################################
# EMULATOR OF 1000 RUNS UNDER NEW TRAINING SET
#########################################################

load("RData/Inputs/SplitSet.RData")
train_old <- train
set.seed(6321, kind = "default")  # 6321
train_new <- sort(sample(train_old, 100))
rm(train, eval, test)

Test.Set <- Design
N <- dim(Test.Set)[1]

# Evaluate new emulator on Test.Set
train <- train_new
system.time(source("Emulation_Small_TestSet.R"))
Emul.new <- Emul
rm(Emul)


############################################
# HISTORY MATCHING FOR THE 1000 RUNS
############################################

# Build matrix of Implausibility Measures for all observations (rows) and all months of interest (cols)
month.indices <- c(1:5, 9:12)
N.months <- length(month.indices)
Global_IM <- matrix(nrow = N, ncol = N.months)
colnames(Global_IM) <- month.names[month.indices]

Mod.Discr <- 0.1
Meas.Err <- 0.05
Emul <- Emul.new
for (i in month.indices){
  month <- month.names[i]
  X <- Emul[[month]]
  z <- Gas.Obs[, month]
  IM <- Compute_IM( X[,"Mean"], X[,"Var"], z, Mod.Discr, Meas.Err)
  Global_IM[, month] <- IM
  rm(X, IM)
  invisible(gc())
}
Global_IM.new <- Global_IM
rm(Global_IM)


###################################################
# RUNS WHICH ARE NON-IMPLAUSIBLE UNDER train_new
###################################################

C <- 4     # threshold for history matching
Y <- ifelse( abs(Global_IM.new) < C, 1, 0) 
Z <- as.matrix(rowSums(Y, na.rm = T))  # Nx1. Z[i] = # of months with a non-implausible match

index_train <- which(Z > N.months-0.5)
train_newbis <- sort(c(train_new, index_train))


########################################################
# PERFORM EMULATION UNDER train_new ON LARGE TEST SET
########################################################

load('RData/Results_Emulation/Eval_Inputs.RData')     # Eval.points.full, inputs used for emulation
N1 <- 1
N2 <- 1e5
N <- N2-N1+1   # total number of points for which IM is computed
Test.Set <- Eval.points.full[N1:N2,]
rm(Eval.points.full)
invisible(gc())

train <- train_new
system.time(source("Emulation_Small_TestSet.R"))
Emul.new <- Emul
rm(Emul)
invisible(gc())


###########################################
# AND IDENTIFY NON-IMPLAUSIBLE POINTS
###########################################

# Build matrix of Implausibility Measures for all observations (rows) and all months of interest (cols)
month.indices <- c(1:5, 9:12)
N.months <- length(month.indices)
Global_IM <- matrix(nrow = N, ncol = N.months)
colnames(Global_IM) <- month.names[month.indices]
Mod.Discr <- 0.1
Meas.Err <- 0.05
Emul <- Emul.new
for (i in month.indices){
  month <- month.names[i]
  X <- Emul[[month]][N1:N2,]
  z <- Gas.Obs[, month]
  IM <- Compute_IM( X[,"Mean"], X[,"Var"], z, Mod.Discr, Meas.Err)
  Global_IM[, month] <- IM
  rm(X, IM)
  invisible(gc())
}
Global_IM.new <- Global_IM
rm(Global_IM)

C <- 4     # threshold for history matching
Y <- ifelse( abs(Global_IM.new) < C, 1, 0) 
Z <- as.matrix(rowSums(Y, na.rm = T))  # Nx1. Z[i] = # of months with a non-implausible match

gas.compat.new <- (Z > N.months-0.5)
cat("The percentage of non-implausible space is: ", 
    format(100*sum(gas.compat.new)/N, digits = 2, nsmall = 2), "%", sep = "")

Emul.new.compat <- Emul.new
for (m in c(1:5, 9:12)){
  Emul.new.compat[[m]] <- Emul.new[[m]][gas.compat.new,]
}


#######################################################################
# COMPARE SUCH NON-IMPLAUSIBLE SPACE WITH OLD NON-IMPLAUSIBLE SPACE
#######################################################################

load('RData/Results_Emulation/Gas_Emulation_Results.RData')   # Emulated gas consumptions (Emul.Res)


Global_IM <- matrix(nrow = N, ncol = N.months)
colnames(Global_IM) <- month.names[month.indices]
Mod.Discr <- 0.1
Meas.Err <- 0.05
Emul <- Emul.Res
for (i in month.indices){
  month <- month.names[i]
  X <- Emul[[month]][N1:N2,]
  z <- Gas.Obs[, month]
  IM <- Compute_IM( X[,"Mean"], X[,"Var"], z, Mod.Discr, Meas.Err)
  Global_IM[, month] <- IM
  rm(X, IM)
  invisible(gc())
}
Global_IM.old <- Global_IM
rm(Global_IM)

C <- 4     # threshold for history matching
Y <- ifelse( abs(Global_IM.old) < C, 1, 0) 
Z <- as.matrix(rowSums(Y, na.rm = T))  # Nx1. Z[i] = # of months with a non-implausible match

gas.compat.old <- (Z > N.months-0.5)
cat("The percentage of non-implausible space is: ", 
    format(100*sum(gas.compat.old)/N, digits = 2, nsmall = 2), "%", sep = "")

cat('Non-implausible with old training:', sum(gas.compat.old))
cat('Non-implausible with new training:', sum(gas.compat.new))
cat('Common between the two:', sum(gas.compat.new & gas.compat.old))

# PLOTS OF NON IMPLAUSIBLE REGIONS
c1=1
c2=6
plot(Test.Set[gas.compat.new,c1], Test.Set[gas.compat.new,c2], col = 'red',
     cex=0.5,
     xlab = colnames(Design)[c1],
     ylab = colnames(Design)[c2])

points(Test.Set[gas.compat.old,c1], Test.Set[gas.compat.old,c2], col = 'blue',
     cex=0.5,
     xlab = colnames(Design)[c1],
     ylab = colnames(Design)[c2])


######################################################################
# EMULATION UNDER train_newbis, AT INPUTS COMPATIBLE UNDER train_new
######################################################################

#Test.Set <- Test.Set[gas.compat,]
train <- train_newbis
Test.Set <- Test.Set[gas.compat.new,]
system.time(source("Emulation_Small_TestSet.R"))
Emul.newbis <- Emul
rm(Emul)

m=1
a <- Emul.new.compat
b <- Emul.newbis
View( cbind(a[[m]][,1], b[[m]][,1], sqrt(a[[m]][,2]), sqrt(b[[m]][,2]) ))



