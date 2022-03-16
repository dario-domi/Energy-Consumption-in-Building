################################################################################

# This script loads the results (means and variances) of emulated monthly gas
# consumption at a large set of test inputs. It then computes the implausibility 
# measure associated with each input, for each month.

# Minimum_Implausibility and Optical Depth plots are produced.

################################################################################


##################################################
## PRELIMINARY ACTIONS: LOAD FUNCTIONS AND DATA
##################################################

# Set folder
setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')

# Load gas data, and emulated results with associated inputs
load('RData/Inputs/Simulated_and_Observed_Gas.RData')         # Gas data (observed and simulated)
rm(Design.Original, Gas.Sim)
load('RData/Results_Emulation/Gas_Emulation_Results.RData')   # Emulated gas consumptions (Emul.Res)
load('RData/Results_Emulation/Eval_Inputs.RData')             # Eval.points.full, inputs used for emulation

# Load custom function to compute implausibility measure
source("../../Emulation.R")
rm(BL.Emul, Corr.fun)


##########################################################################
# CARRY OUT THE ACTUAL HISTORY MATCHING (compute Implausibility Measures)
##########################################################################

# Indeces between which IM will be computed
N1 <- 1
N2 <- 6e6
N <- N2-N1+1   # total number of points for which IM is computed
Eval.points.full <- Eval.points.full[N1:N2,]
invisible(gc())

# Build matrix of Implausibility Measures for all observations (rows) and all months of interest (cols)
month.indices <- c(1:5, 9:12)
N.months <- length(month.indices)
Global_IM <- matrix(nrow = N, ncol = N.months)
colnames(Global_IM) <- month.names[month.indices]

Mod.Discr <- 0.1
Meas.Err <- 0.05
for (i in month.indices){
  month <- month.names[i]
  X <- Emul.Gas[[month]][N1:N2,]
  z <- Gas.Obs[, month]
  IM <- Compute_IM( X[,"Mean"], X[,"Var"], z, Mod.Discr, Meas.Err)
  Global_IM[, month] <- IM
  rm(X, IM)
  invisible(gc())
}
#Global_IM <- Global_IM[, -6]

#############################################################
# ANALYSE RESULTS AND PRODUCE SOME PLOTS
#############################################################

# USEFUL MATRICES AND VECTORS
C <- 4     # threshold for history matching
Y <- ifelse( abs(Global_IM) < C, 1, 0) # dim(Y)=dim(Global_IM): Y[i,j]=1 if abs(Global[i,j])<C, and Y[i,j]=0 otherwise.
Z <- as.matrix(rowSums(Y, na.rm = T))  # Nx1. Z[i] = # of months with a non-implausible match

gas.compat <- (Z > N.months-0.5)
cat("The percentage of non-implausible space is: ", 
    format(100*sum(gas.compat)/N, digits = 2, nsmall = 2), "%", sep = "")

# Save Compatibility results
save(gas.compat, file = "RData/Results_Emulation/Non-Implausible-Inputs.RData")


# PERCENTAGE OF NON-IMPLAUSIBLE SPACE FOR A GIVEN MONTH
for (i in c(1:5,9:12)){
  month <- month.names[i]
  cat("Percentage of non-implausible space for ", month, ": ", 
      format(100*sum( Y[,month] )/N, digits = 3, nsmall = 1), "%\n", sep = "")
}


# SCATTER OF NON-IMPLAUSIBLE POINTS

c1=6
c2=8

limits <- apply(Eval.points.full[1:10000,], 2, range)

plot(Eval.points.full[gas.compat,c1], Eval.points.full[gas.compat,c2], col = 'red',
     xlim = limits[,c1], ylim = limits[,c2], 
     cex=0.0005,
     xlab = colnames(Design)[c1],
     ylab = colnames(Design)[c2])


# SCATTER PLOT OF STANDARD DEVIATION
library("plot3D")
c1 <- 1
c2 <- 8
subs <- 1:1e4
scatter2D(Eval.points.full[subs,c1], Eval.points.full[subs,c2], colvar = (sqrt(Emul.Res[[1]][subs,2])), 
          pch=20, cex=0.2)


# PAIRS SCATTER PLOT (8X8)
k <- 5e5
X <- Eval.points.full[1:k, ]
X <- X[gas.compat[1:k],]
invisible(gc())
pairs(X, pch=16, cex=0.5, xlim=c(-1,1), ylim=c(-1,1))




#############################################
# 2D DEPTH PLOTS OF IMPLAUSIBILITY MEASURE
#############################################

source('CrossSec.R') # custom function to compute matrix
library(plotly)      # to produce contour plots

# vals[i] is the max Implausibility over months, for input i
vals <- apply(abs(Global_IM), 1, max)
invisible(gc())
#val <- t(apply(abs(Global_IM[1:10000,]), 1, sort))   # matrix, in each row implausibilities are sorted in ascending order


# MINIMUM IMPLAUSIBILITY PLOT
dims <- c(1,6,8)

for (c1 in dims){
  for (c2 in setdiff(dims, c1)){
    min.Impl <- Cross.Sect(Eval.points.full, vals, c1, c2, min)
    fig1 <- plot_ly(data = min.Impl,
                    x = ~x, y = ~y, z = ~z,
                    type = 'contour',
                    contours = list(coloring = 'heatmap'),
                    line = list(smoothing=1, width = 1.5, color = rgb(0.75, 0.75, 0.75))
                    )
    
    # see https://plotly.com/r/reference/#scatter-marker-colorbar
    fig <- fig1 %>% 
      layout(xaxis = list(title = names(Design)[c1], range = c(-1,1), #limits[,c1],
                          showline = T, mirror = T, linewidth = 1),
             yaxis = list(title = names(Design)[c2], range = c(-1,1), #limits[,c2],
                          showline = T, mirror = T, linewidth = 1),
             font = list(family = "Balto", size = 30)
      ) %>% 
      colorbar(fig1, len=1, outlinecolor = 'black', outlinewidth = 1,
               thickness = 25, ticklabelposition = 'outside',
               title = list(text = 'min I.M.', side = 'top'))

    fig
    # SAVE PLOT
    file.name <- paste("../Pictures/Gas_Compatibility/Min_Impl/minImpl_MD=20_x=",
                       as.character(c1), "_y=", as.character(c2), ".png", sep = "")
    save_image(fig, file.name, scale = 3)
  }
}

# If the save_image command doesn't work, do the following first:
# install.packages('reticulate')
# reticulate::install_miniconda()
# reticulate::conda_install('r-reticulate', 'python-kaleido')
# reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
# reticulate::use_miniconda('r-reticulate')





# DENSITY 'OPTICAL DEPTH' PLOTS

my.palette <- c("#E24C80", "#E64D8B", "#E84F96", "#EA52A1", "#EC55AB", "#ED59B4", "#EE5DBD", "#EE62C6", "#ED67CE", "#EC6CD6", "#EA72DD", "#E878E4", "#E57EEA", "#E284F0", "#DE8AF5", "#DA90F9", "#D597FE", "#D09DFF", "#CBA3FF", "#C6A9FF", "#C0AFFF", "#BAB5FF", "#B4BBFF", "#AEC0FF", "#A9C6FF", "#A3CBFF", "#9ED0FF", "#99D5FF", "#95DAFF", "#92DFFF", "#8FE3FF", "#8DE7FF", "#8CEBFF", "#8CEFFF", "#8DF3FF", "#8FF7FF", "#92FBFF", "#96FEFF", "#9AFFFF", "#9FFFFF", "#C1C1C1")

for (c1 in dims){
  for (c2 in setdiff(dims, c1)){
    opt.depth <- Cross.Sect(Eval.points.full, vals, c1, c2, function(x){100*sum(x<4)/length(x)})
    fig1 <- plot_ly(
      type = 'contour',
      x = opt.depth[[1]],
      y = opt.depth[[2]],
      z = opt.depth[[3]],
      colors = my.palette,
      reversescale = T,
      contours = list(coloring = 'heatmap'),
      line = list(smoothing=1, width = 0.5, color = 'black')
      )
    fig <- fig1 %>% 
      layout(xaxis = list(title = names(Design)[c1], range = c(-1,1), #limits[,c1],
                          showline = T, mirror = T, linewidth = 1),
             yaxis = list(title = names(Design)[c2], range = c(-1,1), #limits[,c2],
                          showline = T, mirror = T, linewidth = 1),
             font = list(family = "Balto", size = 30)
      ) %>% 
      colorbar(fig1, len=1, outlinecolor = 'black', outlinewidth = 1,
               thickness = 25, ticklabelposition = 'outside',
               ticksuffix = "%")
    
    # SAVE PLOT
    file.name <- paste("../Pictures/Gas_Compatibility/Opt_Depth/Depth_MD=20_x=",
                       as.character(c1), "_y=", as.character(c2), ".png", sep = "")
    
    save_image(fig, file.name, scale = 3)
  }
}

################################################################################
# TRANSFORM (-1,1) INTO CORRECT RANGES
ranges <- apply(Design.Original, 2, range)

#' Rescales the columns of a matrix from the range (-1,1) into the provided ranges
#' @param X a matrix of numbers between -1 and 1
#' @param rng a matrix with two rows and the same number of columns as X
#' @value A matrix with the same `dim` as X, whose column j is obtained by
#'        linearly rescaling column j of X into the range (rng[1,j], rng[2,j])
transform_ranges <- function(X, rng){
  p <- ncol(X)
  for (j in 1:p){
    a <- (rng[2,j] - rng[1,j])/2
    b <- (rng[2,j] + rng[1,j])/2
    X[,j] <- a*X[,j] + b
  }
  return(X)
}

Y <- transform_ranges(Design, ranges)
View(Design)
View(Design.Original)
View(Y)

f <- function(x, a, b) {(a+b)/2 + (b-a)*x/2}
j <- 6
f(0., ranges[1,j], ranges[2,j])











############################################################################
#
# SENSITIVITY ANALYSIS
#

library(sensitivity)
load('RData/Inputs/Simulated_and_Observed_Gas.RData')         # Gas data (observed and simulated)
rm(Design.Original, Gas.Obs)

sens <- matrix(nrow = dim(Design)[2], ncol = dim(Gas.Sim)[2])
rownames(sens) <- names(Design)
colnames(sens) <- names(Gas.Sim)

for (month in 1:12){
  temp <- src(Design, Gas.Sim[, month])
  sens[, month] <- temp$SRC[,1]
}

# Alternative method to use ggplot

library(utils)
df <- expand.grid(x = min.Impl[[1]], y = min.Impl[[2]])
df$z <- as.vector(t(min.Impl[[3]]))
ggplot(df, aes(x=x, y=y, z=z)) + 
  geom_contour_filled()
