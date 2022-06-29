# This script plots a 9x9 figure showing correlated monthly outputs of gas 
# consumption and distribution of monthly implausibility measures

######################################################################
# Load data and functions
######################################################################

setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode/')

library(plot3D)

source('../../Emulation.R')  # to load Compute_IM
rm(BL.Emul, Corr.fun)

load('RData/Inputs/Simulated_and_Observed_Gas.RData') # to have Gas.Obs
rm(Design, Design.Original, Gas.Sim, month.names)

load('RData/Results_Emulation/Eval_Inputs.RData')           # loads Eval.points.full
load('RData/Results_Emulation/Gas_Emulation_Results.RData') # loads Emul.Gas

# Change month names from abbreviated to full ones. Remove Jun, Jul, Aug
names(Emul.Gas)   <- month.name  
Emul.Gas[c(6,7,8)] <- NULL       
colnames(Gas.Obs) <- month.name
Gas.Obs <- Gas.Obs[, -c(6,7,8), drop=F]    


##########################################################################
# Build matrix of Implausibility Measures for all observations (rows) 
# and months (cols)
##########################################################################

N <- dim(Eval.points.full)[1]
L <- length(Gas.Obs)

Global_IM <- matrix(nrow = N, ncol = L)
colnames(Global_IM) <- colnames(Gas.Obs)

Mod.Discr <- 0.1
Meas.Err <- 0.05
for (month in 1:L){
  X <- Emul.Gas[[month]]
  z <- Gas.Obs[, month]
  Global_IM[, month] <- Compute_IM( X[,"Mean"], X[,"Var"], z, Mod.Discr, Meas.Err)
  gc()
}


#########################################################
# Identify non-implausible inputs, for 
# 1) either all months, or 2) all but one month 
#########################################################

Thr <- 4 # threshold

# Inputs that are non-implausible for all months
Y <- abs(Global_IM) < Thr
Z <- as.matrix(rowSums(Y))
gas.compat <- which( as.logical(Z > L-0.1) )

# Inputs that are non-implausible for all but one specific month
Monthly_NonImpl <- matrix(nrow = N, ncol = L)
colnames(Monthly_NonImpl) <- colnames(Global_IM)

for (i in 1:L){
  Y1 <- Y[, -i]
  Z <- as.matrix(rowSums(Y1))
  Monthly_NonImpl[,i] <- (Z > L-1.1)
}

# Plot percentage of space ruled out by each month, once all others are considered
for (i in 1:L){
  x <- length(gas.compat)/sum(Monthly_NonImpl[,i])
  cat("Percentage of space ruled out by", colnames(Global_IM)[i], "constraint:", 100*(1-x), "%.\n")
}







################################################################################


################################################################################
#
# PLOTTING OF THE 9X9 FIGURE
#
################################################################################


##############################################################################
# CUSTOM FUNCTIONS FOR DIAGONAL AND OUT-OF-DIAGONAL PLOTS

# The function takes a numerical vector x and a +ve scalar fac. Returns the range
# obtained by rescaling the original range of x by a factor fac around its center
rescale_range <- function(x, fac){
  rg <- range(x)
  a <- rg[1];  b <- rg[2]
  d <- (b-a)/2
  A <- a - (fac-1)*d
  B <- b + (fac-1)*d
  return(c(A,B))
}

# Make a named color transparent
t_col <- function(color, alpha = 1) {
  rgb.val <- col2rgb(color)/255   # Get RGB values for named color
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               alpha = alpha)
}

# This function adds the numbers n and -n to an ordered vector, if these are 
# within the range of the vector. Returns NA if the range of the vector does not
# overlap with -n nor n; return the vector with n and/or -n otherwise. 
add.n <- function(x, n){
  rg <- range(x)
  to_the_left   <- rg[2] <= -n                  # rg to the left of -n
  in_the_middle <- rg[1] >= -n & rg[2] <= n     # rg between -n and n
  to_the_right  <- rg[1] >= n                   # rg to the right of n
  
  if(to_the_left | in_the_middle | to_the_right)
    return(NA)
  
  # Otherwise the range of x intersects [-n,n] ==> insert -n and/or n
  if ( !(-n %in% x) & rg[1]< -n){  # in this case we need to insert -n
    ind <- (x < -n)
    x <- c(x[ind], -n, x[!ind]) 
  }
  if ( !(n %in% x) & rg[2]>n){     # in this case we need to insert n
    ind <- (x < n)
    x <- c(x[ind], n, x[!ind])
  }
  return(x)
}


#### Function plotting histograms, with different colours and a title  ####

panel.hist <- function(x, ttl){
  
  col1 <- t_col("tan1", 0.6)
  col2 <- t_col("tan3", 0.9)
  
  # Get the breaks, add -4 and/or 4 if needed, set plot frame
  h <- hist(x, breaks = 15, plot = FALSE)
  breaks <- h$breaks
  res <- add.n(breaks, 4)
  plot(range(breaks), c(0,1.4), type = 'n', yaxt = 'n', xaxt = 'n', yaxs = "i", ann = F)
  
  # If range(breaks) is either contained in [-4,4] or totally outside it, 
  # do a one-colour histogram
  if(identical(res, NA)){     
    y <- h$density/max(h$density)      
    col <- ifelse( min(breaks)>=-4 & max(breaks)<=4, col1, col2)
    L <- length(breaks)
    rect(breaks[-L], 0, breaks[-1], y, col = col)
  }
  
  # If range(breaks) overlaps with [-4,4], divide into 3 plots (one may be empty)
  else{
    b <- res
    h <- hist(x, breaks = b, plot = FALSE)
    y <- h$density/max(h$density)
    
    ind1 <- which(b <= -4);           L1 <- length(ind1)
    ind2 <- which(b >= -4 & b <= 4);  L2 <- length(ind2)
    ind3 <- which(b >= 4);            L3 <- length(ind3)
    
    b1 <- b[ind1];   b2 <- b[ind2];   b3 <- b[ind3]
    
  # Plot left histogram (less than -4), if not empty
    if(L1 > 1){
      y1 <- y[1:(L1-1)]
      rect(b1[-L1], 0, b1[-1], y1, col = col2)
    }
    
  # Plot middle histogram (btw -4 and 4)
    temp <- max(L1-1,0) + 1:(L2-1)   # take the appropriate L2-1 indices
    y2 <- y[temp]
    rect(b2[-L2], 0, b2[-1], y2, col = col1)
    
  # Plot right histogram (greater than 4), if not empty
    if(L3 > 1){
      y3 <- y[(length(y)-L3+2):(length(y))] # take the last (L3-1) elements of y
      rect(b3[-L3], 0, b3[-1], y3, col = col2)
    }
  }
  axis(1, cex.axis=1.1, tck=-0.03, mgp = c(1,0.2,0))
  m <- (min(breaks) + max(breaks))/2
  text(m, 1.21, ttl, cex = 1.7)
}


#### Scatter plot of emulated monthly gas consumptions for the two specified months ####

panel.scatter.full <- function(ind.x, ind.y){
  x <- Emul.Gas[[ind.x]][ind,1]
  y <- Emul.Gas[[ind.y]][ind,1]
  plot(x, y, pch = 20, col = rgb(0,0,0,0.05), xaxt='n', yaxt='n', ann = F)
  x <- Emul.Gas[[ind.x]][subsample,1]
  y <- Emul.Gas[[ind.y]][subsample,1]
  points(x, y, pch = 20, cex = 0.5, col = t_col("turquoise1", 0.2) )  # non-implausible points
  points(Gas.Obs[ind.x], Gas.Obs[ind.y], pch=4, cex=1.4, lwd=2.5, col = 'red') # cross
  axis(1, labels = F, tck = 0.03)
  axis(2, labels = F, tck = 0.03)
  axis(3, labels = F, tck = 0.03)
  axis(4, labels = F, tck = 0.03)
}


#### Scatter plot of non-implausible consumptions for the two speficied months, 
#### coloured by implausibility measure  ####

panel.scatter.nonimpl <- function(ind.x, ind.y){
  x <- Emul.Gas[[ind.x]][gas.compat,1]
  y <- Emul.Gas[[ind.y]][gas.compat,1]
  obs.x <- Gas.Obs[ind.x]
  obs.y <- Gas.Obs[ind.y]
  xrng <- rescale_range(c(x, obs.x), 1.25)
  yrng <- rescale_range(c(y, obs.y), 1.25)
  z <- ifelse(IM<3.2, 3.2, IM)
  # Main scatter
  scatter2D(x, y, colvar = z, col = col.pal, clim = c(3.2,4), alpha= 0.8, 
            pch = 16, cex = 0.5, xlim = xrng, ylim = yrng, 
            ann = F, xaxt='n', yaxt='n', colkey = F)
  # Rectangle with observational error
  rect(0.975*obs.x, 0.975*obs.y, 1.025*obs.x, 1.025*obs.y,
       border = NA, col = t_col("wheat3", 0.3))
  box()
  # Cross for observation
  points(obs.x, obs.y, pch=4, cex=1.2, lwd=2.5, col = '#E1341E')
  axis(1, labels = F, tck = 0.03)
  axis(2, labels = F, tck = 0.03)
  axis(3, labels = F, tck = 0.03)
  axis(4, labels = F, tck = 0.03)
}



##############################################################
# PREPARING VARIABLES FOR PLOT

# Sort non-implausible points by implausibility measure
IM <- apply(abs(Global_IM[gas.compat, ]), 1, max)
temp <- sort(IM, decreasing = T, index.return = T)
IM <- temp$x;
gas.compat <- gas.compat[temp$ix]; rm(temp)

# Select random sample of overall N points
set.seed(4832)
ind <- sample(x=N, size=5000)
subsample <- sample(gas.compat, 5000)

# Color palette
col.pal <- hcl.colors(8, "viridis", rev = T)

# Lists of tick labels
# x ticks for full scatterplots (upperdiag)
at.x.full <- list(      NA,           100*seq(15, 35, 5), 100*seq(10, 30, 5),
                  100*seq(10, 35, 5), 100*seq( 5, 25, 5), 100*seq( 4,  6,.5),
                  100*seq( 8, 20, 2), 100*seq(15, 35, 5), 100*seq(20, 45, 5))

# x labels for full scatterplots (upper diag)
lab.x.full <- list(       NA,                   1000*c(NA,2,NA,3,NA), 100*c(NA,15,NA,25,NA),
                    000*c(1,NA,2,NA,3,NA),      1000*c(NA,1,NA,2,NA), 100*seq( 4,  6,.5),
                    00*c(NA,10,NA,14,NA,18,NA), 100*c(15,NA,25,NA,35), 1000*c(2,NA,3,NA,4,NA))

# x ticks for non-implausible scatterplots (lower diag)
at.x.nonimpl <- list(100*(29:33),         seq(1850, 2150, 50), seq(1550, 1700, 50),
                     seq(1450, 1650, 50), seq(950, 1150, 50),  seq(500, 560, 20),
                     seq(1000, 1150, 50), seq(1950, 2250, 50), NA)

# x labels for non-implausible scatterplots (lower diag)
lab.x.nonimpl <- list(100*c(NA,30,NA,32,NA), 100*c(NA,19,NA,20,NA,21,NA), seq(1550, 1700, 50),
                      c(NA,1500,NA,1600,NA), c(NA,1000,NA,1100,NA),       seq(500, 560, 20),
                      c(1000,1050,1100,1150), 100*c(NA,20,NA,21,NA,22,NA),        NA    )


###############################################################################
# MAKE 9X9 PLOT

# Set picture dimensions
file.name <- '../Pictures/Non-Implausible_Plots/Gas/Gas_outputs.png'
png(file.name, width = 18, height = 12, unit="in", res=576)

# Adjust plotting parameters
par(mfrow=c(L,L), 
    oma = c(1, 3, 1, 14),         # overall margins (leave to the right for colorkey)
    mai=c(0.1, 0.05, 0.1, 0.05)  # for each subplot: bott, left, top, right margins
)

# Loop to make all subplots
for (i in 1:L){
  for (j in 1:L){
    if(i==j){                         # diagonal, histograms
      index <- Monthly_NonImpl[,i]    # inputs non-impl for all months but month i
      x <- Global_IM[index, i]
      ttl <- colnames(Gas.Obs)[i]
      panel.hist(x, ttl)
    }
    if(i<j){
      panel.scatter.full(j,i)
      if(i==1)
        axis(3, cex.axis = 1.4, tck = 0.03, mgp = c(3,0.3,0), 
             at = at.x.full[[j]], labels = lab.x.full[[j]])
      if(j==L)
        axis(4, cex.axis = 1.4, tck = 0.03, las = 1, mgp = c(3,0.5,0), gap.axis = 1.4)
      }
    if(i>j){
      panel.scatter.nonimpl(j,i)
      if(j==1)
        axis(2, cex.axis = 1.4, tck = 0.03, las = 1, mgp = c(3,0.5,0), gap.axis = 1.5)
      if(i==L)
        axis(1, cex.axis = 1.4, tck = 0.03, mgp = c(3,0.5,0), gap.axis = 0.01,
             at = at.x.nonimpl[[j]], labels = lab.x.nonimpl[[j]])
    }
  }
}

# Add colorkey to the right
par(mfrow=c(1,1), 
    oma = c(1, 3, 1, 0),  # controls margins with respect to "interior" plot
    mai=c(1, 1, 1, 1.4))  # controls outer margins

colkey(col = col.pal, clim = c(3.2, 4), clab="Implaus\n  Meas",
       width = 0.55, length = 0.8, shift = -0.05, at = (32:40)/10,
       add = T, cex.clab = 1.5, cex.axis = 1.5, adj.clab = 0)

dev.off()


#x <- y <- seq(-8*pi, 8*pi, len = 200)
#r <- sqrt(outer(x^2, y^2, "+"))
#filled.contour(cos(r^2)*exp(-r/(2*pi)), axes=FALSE, color.palette=vir


# mgp[1] = dist label-axis; 
# mgp[2] = dist numbers-axis;
# mgp[3] = dist axis line from plot box. 
# Default: 3,1,0
