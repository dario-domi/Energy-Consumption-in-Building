################################################################################
#
# This script loads gas and temperature non-implausible inputs from
# "RData/Results_Emulation/Non-Implausible-Inputs.RData"
# and produces summary statistics and plots about the results.
#
# Minimum_Implausibility and Optical Depth plots are produced.
#
###############################################################################


#############################################################
## SET FOLDER, LOAD LIBRARIES AND EMULATION INPUT-OUTPUTS
#############################################################

setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')

# DESIGN POINTS (for variable names)
load("RData/Inputs/Design_Points.RData")
rm(Design.Original)


# INPUTS
load('RData/Results_Emulation/Eval_Inputs.RData') # loads Eval.points.full
Eval.points <- Eval.points.full
rm(Eval.points.full)  # clean workspace
invisible(gc())       # release memory

# NON-IMPLAUSIBLE INPUTS (gas and temperature)
load("RData/Results_Emulation/Non-Implausible-Inputs.RData")
temp.compat <- kitch.compat & mast.compat


################################################################################
# PERCENTAGES OF NON-IMPLAUSIBLE SPACE (GAS, TEMPERATURE, BOTH)
################################################################################

###############

# Separate percentages

cat("Percentage of gas non-implausible space: ", 
    format(100*mean(gas.compat), digits = 4, nsmall = 2), "%", sep = "")

cat("Percentage of kitchen non-implausible space: ", 
    format(100*mean(kitch.compat), digits = 4, nsmall = 2), "%\n", 
    "Percentage of kitchen non-implausible given master non-implausible: ", 
     format(100*mean(temp.compat)/mean(mast.compat), digits = 4, nsmall = 2), "%",
    sep = "")

cat("Percentage of master non-implausible space: ", 
    format(100*mean(mast.compat), digits = 4, nsmall = 2), "%\n", 
    "Percentage of master non-implausible given kitchen non-implausible: ", 
    format(100*mean(temp.compat)/mean(kitch.compat), digits = 4, nsmall = 2), "%",
    sep = "")

cat("Percentage of temperature non-implausible space: ", 
    format(100*mean(temp.compat), digits = 2, nsmall = 2), "%", sep = "")


################

# Relation between energy and environmental constraints

cat("Percentage of temperature non-implausible space: ", 
    format(100*mean(temp.compat), digits = 2, nsmall = 2), "%", sep = "")

cat("Percentage of temperature non-implausible space given it being energy non-implausible: ", 
    format(100*mean(temp.compat & gas.compat)/mean(gas.compat), digits = 2, nsmall = 2), "%", sep = "")

cat("Percentage of overall non-implausible space: ", 
    format(100*mean(temp.compat & gas.compat), digits = 4, nsmall = 2), "%", sep = "")


########################################################################
# SCATTER PLOTS FOR TEMPERATURE AND ENERGY NON-IMPLAUSIBLE INPUTS
########################################################################


c1=1
c2=6
n <- 1e4

limits <- apply(Eval.points[1:n,], 2, range)

# Gas
x <- Eval.points[gas.compat,c1]
y <- Eval.points[gas.compat,c2]
plot(x[1:n], y[1:n], col = 'red',
     xlim = limits[,c1], ylim = limits[,c2], 
     cex=0.05, pch = 19, 
     xlab = colnames(Design)[c1],
     ylab = colnames(Design)[c2])

# Kitchen
x <- Eval.points[kitch.compat, c1]
y <- Eval.points[kitch.compat, c2]
points(x[1:n], y[1:n], col = 3, cex = 0.001)

# Master
x <- Eval.points[mast.compat, c1]
y <- Eval.points[mast.compat, c2]
points(x[1:n], y[1:n], col = 4, cex = 0.001)

# Kitchen, Master and Gas
x <- Eval.points[temp.compat & gas.compat, c1]
y <- Eval.points[temp.compat & gas.compat, c2]
points(x[1:n], y[1:n], col = 7, cex = 0.1)


##############################################################################
#                                                                            #
# 1D OVERLAPPING HISTOGRAMS: GAS, TEMPERATURE AND GAS&TEMP NON-IMPLAUSIBLE   #
# COORDINATES                                                                #
#                                                                            #
##############################################################################


###############################################################################

# 1D HISTOGRAMS: QUICKLY ASSESS WHICH COORDINATES BECOMES IMPORTANT WHEN BOTH
# CONSTRAINTS ARE CONSIDERED

v <- 1
hist(Eval.points[gas.compat, v], 50, xlab = colnames(Design)[v], freq = F, xlim = c(-1,1))
hist(Eval.points[temp.compat, v], 50, xlab = colnames(Design)[v], freq = F, xlim = c(-1,1))
hist(Eval.points[temp.compat & gas.compat, v], xlab = colnames(Design)[v], freq = F, xlim = c(-1,1))

# Comments:
# Heating setpoint (1) much more bounded when both constraints are accounted for
# Efficiency boiler (2) as well (that's remarkable). A bit with infiltration too (6).

################################################################################

# Custom function to make a given named colour trasparent
t_col <- function(color, alpha = 0.5) {
  rgb.val <- col2rgb(color)/255
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3], alpha = alpha)
}

# Colours of histograms
colGas <-  "royalblue3"
colTemp <- "firebrick3"
colBoth <- t_col("chartreuse3", 0.5)

ylims <- c(2.4, 2.6, NA, NA, NA, 10, NA, NA)


#######################################################
# HISTOGRAMS: ONE INPUT VARIABLE AT A TIME
#######################################################

for (v in c(1,2,6)){
  
  # Data to be plotted
  Gas  <- Eval.points[gas.compat, v]
  Temp <- Eval.points[temp.compat, v]
  Both <- Eval.points[gas.compat & temp.compat, v]
  
  # Prepare png file
  file_name <- paste0("../Pictures/Non-Implausible_Plots/Gas_and_Temperature/",
                      colnames(Design)[v], ".png")
  png(file_name, width = 8, height = 6, unit="in", res=288)
  
  # Set graphical parameters
  par(cex = 1.2, cex.lab = 1.2, 
      mai=c(1, 1, 0, 0) # for each subplot: bott, left, top, right margins
      #mgp = c(5, 2, 0)   # mgp[1] = dist label-axis; mgp[2] = dist numbers-axis; 
      # mgp[3] = dist axis line from plot box. Default: 3,1,0
  )
  
  # Set all histogram details (with empty histogram)
  hist(Both, breaks = 10, freq = F, border=NA, col = NULL,
       main = "", xlab = colnames(Design)[v], 
       xlim = c(-1,1), ylim = c(0, ylims[v]) )
  
  # Plot the three histograms: interior and then outer border
  h1 <- hist(Temp, breaks = 50, freq = F, density = 15, border=NA, col = colTemp, add = T)
  lines(c(-1,h1$breaks), c(0, h1$density, 0), type="s", col = colTemp, lwd = 1.5)
  
  h2 <- hist(Gas, breaks = 50, freq = F, density = 15, border=NA, col = colGas, angle = 135, add = T)
  lines(c(-1,h2$breaks), c(0, h2$density, 0), type="s", col = colGas, lwd =1.5)
  
  h3 <- hist(Both, breaks = 12, freq = F, col = colBoth, border = NA, add = T)
  lines(c(h3$breaks[1],h3$breaks), c(0, h3$density, 0), type="s", col = "chartreuse4", lwd =2)
  
  legend(ifelse(v==1, "topleft", "topright"), 
         legend=c("Temp Non-Impl", "Gas Non-Impl", "Non-Implaus"),
         fill = c(colTemp, colGas, colBoth),            # colour of oblique lines inside squares
         border = c(colTemp, colGas, "Chartreuse4"),    # colour of square border
         density = c(30, 30, NA),                       # density of oblique lines
         angle = c(45, 135, NA),                        # angle of oblique lines
         y.intersp=1.2,                  # vertical distance factor between legend lines
         inset=0.05,                     # put legend a bit more inside, not on the edge
         bty = "n",                      # no box is draw
         cex = 1.3)                      # scaling factor of text, squares etc
  
  dev.off()
}


############################################################################
# 1x3 PLOT, EACH PANEL CONCERNING ONE INPUT VARIABLE
############################################################################

# Prepare png file
file_name <- "../Pictures/Non-Implausible_Plots/Gas_and_Temperature/Non-Implas.png"
png(file_name, width = 18, height = 6, unit="in", res=288)

# Set graphical parameters
par(mfrow=c(1,3), cex.lab = 2, cex.axis = 2,
    mai=c(1, 1, 0.5, 0), # for each subplot: bott, left, top, right margins
    mgp = c(5, 2, 0)   # mgp[1] = dist label-axis; mgp[2] = dist numbers-axis; 
                           # mgp[3] = dist axis line from plot box. Default: 3,1,0
)

# Plot of 3 panels, each with three histograms
for (v in c(1,2,6)){

  # Data to be plotted
  Gas  <- Eval.points[gas.compat, v]
  Temp <- Eval.points[temp.compat, v]
  Both <- Eval.points[gas.compat & temp.compat, v]
  
  # Set all histogram details (with empty histogram)
  hist(Both, breaks = 10, freq = F, border=NA, col = NULL,
       main = "", xlab = colnames(Design)[v], ylab = ifelse(v==1, "Density", ""),
       xlim = c(-1,1), ylim = c(0, ylims[v]) )
  
  # Plot the three histograms: interior and then outer border
  h1 <- hist(Temp, breaks = 50, freq = F, density = 15, border=NA, col = colTemp, add = T)
  lines(c(-1,h1$breaks), c(0, h1$density, 0), type="s", col = colTemp, lwd = 1.5)
  
  h2 <- hist(Gas, breaks = 50, freq = F, density = 15, border=NA, col = colGas, angle = 135, add = T)
  lines(c(-1,h2$breaks), c(0, h2$density, 0), type="s", col = colGas, lwd =1.5)
  
  h3 <- hist(Both, breaks = 10, freq = F, col = colBoth, border = NA, add = T)
  lines(c(h3$breaks[1],h3$breaks), c(0, h3$density, 0), type="s", col = "chartreuse4", lwd =2)
  
  legend(ifelse(v==1, "topleft", "topright"), 
         legend=c("Temp Non-Impl", "Gas Non-Impl", "Non-Implaus"),
         fill = c(colTemp, colGas, colBoth),            # colour of oblique lines inside squares
         border = c(colTemp, colGas, "Chartreuse4"),    # colour of square border
         density = c(30, 30, NA),                       # density of oblique lines
         angle = c(45, 135, NA),                        # angle of oblique lines
         y.intersp=1.2,                  # vertical distance factor between legend lines
         inset=0.05,                     # put legend a bit more inside, not on the edge
         bty = "n",                      # no box is draw
         cex = 2.2)                      # scaling factor of text, squares etc
}

dev.off()


################################################################
# SCATTER PLOT OF STANDARD DEVIATION

load("RData/Results_Emulation/")
library("plot3D")
c1 <- 1
c2 <- 8
subs <- 1:1e4
scatter2D(Eval.points[subs,c1], Eval.points[subs,c2], colvar = (sqrt(Emul.Res[[1]][subs,2])), 
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










#################################
#PLOTS

c1 <- 3
c2 <- 6

plot(Eval.points[gas,c1], Eval.points[gas,c2], col = 'red',
     xlim = c(-1,1), ylim = c(-1,1), 
     cex=0.7, pch=20,
     xlab = colnames(Design)[c1],
     ylab = colnames(Design)[c2])

x1 <- Eval.points[kitch.compat & mast.compat,c1]
x2 <- Eval.points[kitch.compat & mast.compat,c2]
ind <- sample(length(kitch.compat), 1000)
points(x1[ind], x2[ind], col = 'blue',
       xlim = c(-1,1), ylim = c(-1,1), 
       cex=0.7, pch=20,
       xlab = colnames(Design)[c1],
       ylab = colnames(Design)[c2])

plot(Eval.points[1:10000, 8], Emul.Kitch[1:10000,1])




































#################################################################
# LINEAR REGRESSION AND EMULATION (KITCHEN)

# EVALUATION INPUTS
load('RData/Results_Emulation/Eval_Inputs.RData') # loads Eval.points.full
N <- 1.e6
Eval.points <- Eval.points.full[1:N, ]
rm(Eval.points.full)  # clean workspace
invisible(gc())       # release memory

# OUTPUTS AT DESIGN POINTS
y.train <- Impl.Kitch.Sum[train]

# LINEAR REGRESSION
Interactions <- poly(Design[train,], degree=4)
L <- regsubsets(y.train~., data=Interactions, method = "forward", nvmax = 10)
regr <- summary(L)$which[10,-1]            # logical vector with selected regressors
fit <- lm(y.train~ ., data = as.data.frame(Interactions[, regr]))

# ACTIVE INPUTS AND REGRESSORS FOR TRAINING, EVALUATION, TEST SETS
Active.Inputs <- c(1,3,6,8)
Train.ActInp <- Design[train, Active.Inputs, drop=F]    # design points used to train the emulator
Train.Regr   <- cbind(1, Interactions[, regr, drop=F])  # regressors: add a column of 1s for intercept

Eval.ActInp  <- Eval.points[, Active.Inputs, drop=F]
temp <- predict(Interactions, newdata = Eval.points)
Eval.Regr    <- cbind(1, temp[, regr, drop=F])

# EMULATION PARAMETERS
beta <- fit$coefficients
s2.tot <- var(fit$residuals)
nugget.frac <- 0.05
s2 <- (1-nugget.frac)*s2.tot
nu2 <- nugget.frac*s2.tot
d <- 0.5

system.time(Emul.Kitch <- BL.Emul(ActInp.Design = Train.ActInp, 
                                  ActInp.Test = Eval.ActInp, 
                                  y = y.train, 
                                  Regress.Design = Train.Regr, 
                                  Regress.Test = Eval.Regr, 
                                  beta = beta,
                                  sigma2 = s2, kernel = 'exp2', d = d, nu2 = nu2)
)

kitch.compat <- (Emul.Kitch[,1] - 3*sqrt(Emul.Kitch[,2]))<9


#################################################################
# LINEAR REGRESSION AND EMULATION (MASTER)

# OUTPUTS AT DESIGN POINTS
y.train <- Impl.Mast.Sum[train]

# LINEAR REGRESSION
Interactions <- poly(Design[train,], degree=4)
L <- regsubsets(y.train~., data=Interactions, method = "forward", nvmax = 10)
regr <- summary(L)$which[10,-1]            # logical vector with selected regressors
fit <- lm(y.train~ ., data = as.data.frame(Interactions[, regr]))

# ACTIVE INPUTS AND REGRESSORS FOR TRAINING, EVALUATION, TEST SETS
Active.Inputs <- c(1,3,4,6)
Train.ActInp <- Design[train, Active.Inputs, drop=F]    # design points used to train the emulator
Train.Regr   <- cbind(1, Interactions[, regr, drop=F])  # regressors: add a column of 1s for intercept

Eval.ActInp  <- Eval.points[, Active.Inputs, drop=F]
temp <- predict(Interactions, newdata = Eval.points)
Eval.Regr    <- cbind(1, temp[, regr, drop=F])

# EMULATION PARAMETERS
beta <- fit$coefficients
s2.tot <- var(fit$residuals)
nugget.frac <- 0.05
s2 <- (1-nugget.frac)*s2.tot
nu2 <- nugget.frac*s2.tot
d <- 0.4

system.time(Emul.Mast <- BL.Emul(ActInp.Design = Train.ActInp, 
                                 ActInp.Test = Eval.ActInp, 
                                 y = y.train, 
                                 Regress.Design = Train.Regr, 
                                 Regress.Test = Eval.Regr, 
                                 beta = beta,
                                 sigma2 = s2, kernel = 'exp2', d = d, nu2 = nu2)
)

mast.compat <- (Emul.Mast[,1] - 3*sqrt(Emul.Mast[,2]))<9



###############################################################################
# SCATTER PLOT OF NON-IMPLAUSIBLE POINTS

c1=3
c2=6

plot(Eval.points[gas,c1], Eval.points[gas,c2], col = 'red',
     xlim = c(-1,1), ylim = c(-1,1), 
     cex=0.7, pch=20,
     xlab = colnames(Design)[c1],
     ylab = colnames(Design)[c2])

x1 <- Eval.points[kitch.compat & mast.compat,c1]
x2 <- Eval.points[kitch.compat & mast.compat,c2]
ind <- sample(length(kitch.compat), 1000)
points(x1[ind], x2[ind], col = 'blue',
       xlim = c(-1,1), ylim = c(-1,1), 
       cex=0.7, pch=20,
       xlab = colnames(Design)[c1],
       ylab = colnames(Design)[c2])

plot(Eval.points[1:10000, 8], Emul.Kitch[1:10000,1])


library("plot3D")
subs <- 1:1e4
scatter2D(Eval.points[subs,8], Emul.Kitch[subs,1], 
          colvar = Emul.Kitch[subs,1], 
          pch=20, cex=0.2)





