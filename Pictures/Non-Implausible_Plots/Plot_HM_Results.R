################################################################################
#
# This script produces plots summarising the results of History Matching (HM).
# Details of plots produced:
# 
# - 
#
################################################################################


#############################################################
## SET FOLDER, LOAD LIBRARIES AND EMULATION INPUT-OUTPUTS
#############################################################

setwd('/Users/Durham/Desktop/PostDoc/Projects/UQ_Energy_Building/RCode')

# DESIGN POINTS (for variable names)
load("RData/Results_Simulator/Design_Points.RData")

# INPUTS
load('RData/Results_Emulator/Evaluation_Set.RData') # loads Eval.points.full
Eval.points <- Eval.points.full
rm(Eval.points.full); invisible(gc())   # remove variable and release memory

# FULL NON IMPLAUSIBILITY MATRIX (10% AND 20% MODEL DISCREPANCY)
load("RData/Results_Emulator/Full_Implausibilities.RData")

# NON-IMPLAUSIBLE INPUTS (gas and temperature)
load("RData/Results_Emulator/Non-Implausible-Inputs.RData")
temp.compat <- kitch.compat & mast.compat


#############################################################
## PREPARE FUNCTIONS AND VARIABLES FOR SUBSEQUENT PLOTS
#############################################################

source('CrossSec.R') # custom function to compute min.impl and opt.depth matrices
library(plotly)      # to produce contour plots

# Max_IM[i] is the max Implausibility over months, for input i
IM.Gas <- IM.Gas10
Max_IM <- apply(abs(IM.Gas), 1, max); invisible(gc())

# Names of parameters to be used as axis labels
par_names <- c('heating setpoint (°C)', 'boiler efficiency', NA, NA, NA, 
               'infiltration (ach)', 'DHW consumption', '% of gas cooking')

# Rescale inputs from [-1,1] into original ranges
for (j in 1:8){
  a <- Ranges_Params[1,j]
  b <- Ranges_Params[2,j]
  x <- Eval.points[,j]
  Eval.points[,j] <- (a+b)/2 + (b-a)*x/2
}
invisible(gc())


##############################################################################
#                                                                            #
#        MINIMUM IMPLAUSIBILITY SUBPLOTS FOR GAS (10% MODEL DISCR)           #
#                                                                            #
##############################################################################

# For each relevant pair, compute minimum implaus matrix to be plotted as contour
min.Impl16 <- Cross.Sect(Eval.points, Max_IM, 1, 6, min); invisible(gc())
min.Impl18 <- Cross.Sect(Eval.points, Max_IM, 1, 8, min); invisible(gc())
min.Impl68 <- Cross.Sect(Eval.points, Max_IM, 6, 8, min); invisible(gc())

# Common color range to all subplots
col_lims <- c(3,18)  


#######   FIRST SUBPLOT   #######
c1 <- 1; c2 <- 6
fig1 <- plot_ly(data = min.Impl16,                            
                x = ~x, y = ~y, z = ~z,                      
                type = 'contour',                     
                colorscale = 'Viridis',               # any RColorBrewer palette: display.brewer.all()
                contours = list(coloring = 'heatmap', # one of: 'heatmap', 'fill', 'lines', 'none'
                                showlabels=T,         # show contour levels
                                labelfont = list(size=14)),
                line = list(smoothing=1, width = 1.5, # specification of contour lines
                            color = rgb(0.75, 0.75, 0.75))
) %>%
  colorbar(limits = col_lims) %>%                      # sets colorscale limits
  layout(xaxis = list(title = list(text=par_names[c1], standoff=0)), # standoff to have axis labels closer to axis
         yaxis = list(title = list(text=par_names[c2], standoff=10)) )


#######   SECOND SUBPLOT   #######
c1 <- 1; c2 <- 8
fig2 <- plot_ly(data = min.Impl18,
                x = ~x, y = ~y, z = ~z,
                type = 'contour', colorscale = 'Viridis',
                contours = list(coloring = 'heatmap', showlabels=T, labelfont = list(size=14),
                                start=3, end=10, size=0.5),         # levels where to plot contour lines
                line = list(smoothing=1, width = 1.5, color = rgb(0.75, 0.75, 0.75))
) %>% 
  colorbar(limits = col_lims) %>%
  layout(xaxis = list(title = list(text=par_names[c1], standoff=0)),
         yaxis = list(title = list(text=par_names[c2], standoff=0)))


#######   THIRD SUBPLOT   #######
c1 <- 6; c2 <- 8
fig3 <- plot_ly(data = min.Impl68,
                x = ~x, y = ~y, z = ~z,
                type = 'contour', colorscale = 'Viridis',
                contours = list(coloring = 'heatmap', showlabels=T, labelfont = list(size=14),
                                size=1, start=2, end=40),
                line = list(smoothing=1, width = 1.5, color = rgb(0.75, 0.75, 0.75)),
                height = 400, 
                width = 1400
) %>% 
  colorbar(limits = col_lims) %>%
  layout(xaxis = list(title = list(text=par_names[c1], standoff=0)),
         yaxis = list(title = list(text=par_names[c2], standoff=0)))


######  ADD OVERALL COLORBAR AND SETTINGS   ######

fig3 <- fig3 %>% 
  layout(font = list(family = "Balto", size = 18)) %>%
  colorbar(limits = col_lims, len=1, title = list(text = 'min I.M.', side = 'top'),#, font=list(size=18)),
           tickmode = 'array', tickvals = seq(2, 20, 2)
  )

#####   PRODUCE AND SAVE FIGURE   #####

fig <- subplot(hide_colorbar(fig1), hide_colorbar(fig2), fig3,
               margin = 0.035, widths = c(0.322, 0.356, 0.322),
               titleX = T, titleY = T, shareX = T)

save_image(fig, '../Pictures/Non-Implausible_Plots/Gas/Min_Impl_10%.png', scale = 3)


# If the save_image command doesn't work, do the following first:
# install.packages('reticulate')
# reticulate::install_miniconda()
# reticulate::conda_install('r-reticulate', 'python-kaleido')
# reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
# reticulate::use_miniconda('r-reticulate')

###############################################################################


##############################################################################
#                                                                            #
#             OPTICAL DEPTH SUBPLOTS FOR GAS (10% MODEL DISCR)               #
#                                                                            #
##############################################################################

# For each relevant pair, compute minimum implaus matrix to be plotted as contour
f <- function(x){100*mean(x<4)}
opt.depth16 <- Cross.Sect(Eval.points, Max_IM, 1, 6, f); invisible(gc())
opt.depth18 <- Cross.Sect(Eval.points, Max_IM, 1, 8, f); invisible(gc())
opt.depth68 <- Cross.Sect(Eval.points, Max_IM, 6, 8, f); invisible(gc())

# Customised color palette to distinguish clearly 0 and non-zero
my.palette <- c("#E24C80", "#E64D8B", "#E84F96", "#EA52A1", "#EC55AB", "#ED59B4", "#EE5DBD", "#EE62C6", "#ED67CE", "#EC6CD6", "#EA72DD", "#E878E4", "#E57EEA", "#E284F0", "#DE8AF5", "#DA90F9", "#D597FE", "#D09DFF", "#CBA3FF", "#C6A9FF", "#C0AFFF", "#BAB5FF", "#B4BBFF", "#AEC0FF", "#A9C6FF", "#A3CBFF", "#9ED0FF", "#99D5FF", "#95DAFF", "#92DFFF", "#8FE3FF", "#8DE7FF", "#8CEBFF", "#8CEFFF", "#8DF3FF", "#8FF7FF", "#92FBFF", "#96FEFF", "#9AFFFF", "#9FFFFF", "#C1C1C1")

# Common color range to all subplots
col_lims <- c(0, 6.6)  


#######   FOURTH SUBPLOT   #######
c1 <- 1; c2 <- 6
fig4 <- plot_ly(data = opt.depth16,                            
                x = ~x, y = ~y, z = ~z,                      
                type = 'contour', 
                colors = my.palette,  reversescale = T,             
                contours = list(coloring = 'heatmap', showlabels=T, labelfont = list(size=14)),
                line = list(smoothing=1, width = 1, color = 'black')
) %>%
  colorbar(limits = col_lims) %>%                      # sets colorscale limits
  layout(xaxis = list(title = list(text=par_names[c1], standoff=0)), 
         yaxis = list(title = list(text=par_names[c2], standoff=10)) )


#######   FIFTH SUBPLOT   #######
c1 <- 1; c2 <- 8
fig5 <- plot_ly(data = opt.depth18,                            
                x = ~x, y = ~y, z = ~z,                      
                type = 'contour', 
                colors = my.palette,  reversescale = T,             
                contours = list(coloring = 'heatmap', showlabels=T, labelfont = list(size=14)),
                line = list(smoothing=1, width = 1, color = 'black')
) %>%
  colorbar(limits = col_lims) %>% 
  layout(xaxis = list(title = list(text=par_names[c1], standoff=0)), 
         yaxis = list(title = list(text=par_names[c2], standoff=0)) )

#######   SIXTH SUBPLOT   #######
c1 <- 6; c2 <- 8
fig6 <- plot_ly(data = opt.depth68,                            
                x = ~x, y = ~y, z = ~z,                      
                type = 'contour', 
                colors = my.palette,  reversescale = T,             
                contours = list(coloring = 'heatmap', showlabels=T, labelfont = list(size=14),
                                start=1, end=6, size=1),
                line = list(smoothing=1, width = 1, color = 'black'),
                height = 400, 
                width = 1400
) %>%
  colorbar(limits = col_lims) %>%                      # sets colorscale limits
  layout(xaxis = list(title = list(text=par_names[c1], standoff=0)), 
         yaxis = list(title = list(text=par_names[c2], standoff=0)) )


######  ADD OVERALL COLORBAR AND SETTINGS   ######

fig6 <- fig6 %>% 
  layout(font = list(family = "Balto", size = 18)) %>%
  colorbar(limits = col_lims, len=0.9, title = '',
           outlinecolor = 'black', outlinewidth = 1,
           ticksuffix = '%',
           tickmode = 'array', tickvals = seq(0,6,1)
  )

fig <- subplot(hide_colorbar(fig4), hide_colorbar(fig5), fig6,
               margin = 0.031, widths = c(0.322, 0.356, 0.322),
               titleX = T, titleY = T, shareX = T)

#####   SAVE IMAGE    #####

save_image(fig, '../Pictures/Non-Implausible_Plots/Gas/Opt_Depth_10%.png', scale = 3)




##############################################################################
#                                                                            #
#       TOGETHER: MIN IMPL & OPT DEPTH PLOTS FOR GAS (10% MODEL DISCR)       #
#                                                                            #
##############################################################################

#######   UPDATE THIRD SUBPLOT   #######
col_lims <- c(3,18)  

c1 <- 6; c2 <- 8
fig3 <- plot_ly(data = min.Impl68,
                x = ~x, y = ~y, z = ~z,
                type = 'contour', colorscale = 'Viridis',
                contours = list(coloring = 'heatmap', showlabels=T, size=1, start=2, end=40),
                line = list(smoothing=1, width = 1.5, color = rgb(0.75, 0.75, 0.75)),
                height = 800, 
                width = 1400
) %>% 
  colorbar(limits = col_lims) %>%
  layout(xaxis = list(title = list(text=par_names[c1], standoff=0)),
         yaxis = list(title = list(text=par_names[c2], standoff=0)))


fig3 <- fig3 %>% 
  layout(font = list(family = "Balto", size = 18)) %>%
  colorbar(limits = col_lims, len=0.45, title = list(text = 'min I.M.', side = 'top'),#, font=list(size=18)),
           tickmode = 'array', tickvals = seq(2, 20, 2)
  )


#######   UPDATE SIXTH SUBPLOT   #######
col_lims <- c(0, 6.6)  

c1 <- 6; c2 <- 8
fig6 <- plot_ly(data = opt.depth68,                            
                x = ~x, y = ~y, z = ~z,                      
                type = 'contour', 
                colors = my.palette,  reversescale = T,             
                contours = list(coloring = 'heatmap', showlabels=T, labelfont = list(size=14),
                                start=1, end=6, size=1),
                line = list(smoothing=1, width = 1, color = 'black'),
                height = 800, 
                width = 1400
) %>%
  colorbar(limits = col_lims) %>%                      # sets colorscale limits
  layout(xaxis = list(title = list(text=par_names[c1], standoff=0)), 
         yaxis = list(title = list(text=par_names[c2], standoff=0)) )


fig6 <- fig6 %>% 
  layout(font = list(family = "Balto", size = 18)) %>%
  colorbar(limits = col_lims, len=0.4, y=0.43, title = '',
           outlinecolor = 'black', outlinewidth = 1,
           ticksuffix = '%',
           tickmode = 'array', tickvals = seq(0,6,1)
  )

##########################################

fig <- subplot(nrows = 2,
               hide_colorbar(fig1), hide_colorbar(fig2), fig3,
               hide_colorbar(fig4), hide_colorbar(fig5), fig6,
               margin = 0.035, widths =  c(0.322, 0.356, 0.322),
               titleX = T, titleY = T, shareX = T)


save_image(fig, '../Pictures/Non-Implausible_Plots/Gas/MinImpl_and_OptDepth_10%.png', scale = 3)




###############################################################################

###############################################################################

###############################################################################

# Max_IM[i] is the max Implausibility over months, for input i
IM.Gas <- IM.Gas20
Max_IM <- apply(abs(IM.Gas), 1, max); invisible(gc())

##############################################################################
#                                                                            #
#        MINIMUM IMPLAUSIBILITY SUBPLOTS FOR GAS (20% MODEL DISCR)           #
#                                                                            #
##############################################################################

# For each relevant pair, compute minimum implaus matrix to be plotted as contour
min.Impl16 <- Cross.Sect(Eval.points, Max_IM, 1, 6, min); invisible(gc())
min.Impl18 <- Cross.Sect(Eval.points, Max_IM, 1, 8, min); invisible(gc())
min.Impl68 <- Cross.Sect(Eval.points, Max_IM, 6, 8, min); invisible(gc())

# Common color range to all subplots
col_lims <- c(1.7,11.3)  


#######   FIRST SUBPLOT   #######
c1 <- 1; c2 <- 6
fig1 <- plot_ly(data = min.Impl16,                            
                x = ~x, y = ~y, z = ~z,                      
                type = 'contour',                     
                colorscale = 'Viridis',               # any RColorBrewer palette: display.brewer.all()
                contours = list(coloring = 'heatmap', # one of: 'heatmap', 'fill', 'lines', 'none'
                                showlabels=T,         # show contour levels
                                labelfont = list(size=14)),
                line = list(smoothing=1, width = 1.5, # specification of contour lines
                            color = rgb(0.75, 0.75, 0.75))
) %>%
  colorbar(limits = col_lims) %>%                      # sets colorscale limits
  layout(xaxis = list(title = list(text=par_names[c1], standoff=0)), # standoff to have axis labels closer to axis
         yaxis = list(title = list(text=par_names[c2], standoff=10)) )


#######   SECOND SUBPLOT   #######
c1 <- 1; c2 <- 8
fig2 <- plot_ly(data = min.Impl18,
                x = ~x, y = ~y, z = ~z,
                type = 'contour', colorscale = 'Viridis',
                contours = list(coloring = 'heatmap', showlabels=T, labelfont = list(size=14),
                                start=2, end=4, size=0.4),         # levels where to plot contour lines
                line = list(smoothing=1, width = 1.5, color = rgb(0.75, 0.75, 0.75))
) %>% 
  colorbar(limits = col_lims) %>%
  layout(xaxis = list(title = list(text=par_names[c1], standoff=0)),
         yaxis = list(title = list(text=par_names[c2], standoff=0)))


#######   THIRD SUBPLOT   #######
c1 <- 6; c2 <- 8
fig3 <- plot_ly(data = min.Impl68,
                x = ~x, y = ~y, z = ~z,
                type = 'contour', colorscale = 'Viridis',
                contours = list(coloring = 'heatmap', showlabels=T, labelfont = list(size=14),
                                start=1, end=12, size=0.5),
                line = list(smoothing=1, width = 1.5, color = rgb(0.75, 0.75, 0.75)),
                height = 400, 
                width = 1400
) %>% 
  colorbar(limits = col_lims) %>%
  layout(xaxis = list(title = list(text=par_names[c1], standoff=0)),
         yaxis = list(title = list(text=par_names[c2], standoff=0)))


######  ADD OVERALL COLORBAR AND SETTINGS   ######

fig3 <- fig3 %>% 
  layout(font = list(family = "Balto", size = 18)) %>%
  colorbar(limits = col_lims, len=1, title = list(text = 'min I.M.', side = 'top'),
           tickmode = 'array', tickvals = 2:11)

#####   PRODUCE AND SAVE FIGURE   #####

fig <- subplot(hide_colorbar(fig1), hide_colorbar(fig2), fig3,
               margin = 0.035, widths = c(0.322, 0.356, 0.322),
               titleX = T, titleY = T, shareX = T)

fig
save_image(fig, '../Pictures/Non-Implausible_Plots/Gas/Min_Impl_20%.png', scale = 3)



###############################################################################


##############################################################################
#                                                                            #
#             OPTICAL DEPTH SUBPLOTS FOR GAS (20% MODEL DISCR)               #
#                                                                            #
##############################################################################

# For each relevant pair, compute minimum implaus matrix to be plotted as contour
f <- function(x){100*mean(x<4)}
opt.depth16 <- Cross.Sect(Eval.points, Max_IM, 1, 6, f); invisible(gc())
opt.depth18 <- Cross.Sect(Eval.points, Max_IM, 1, 8, f); invisible(gc())
opt.depth68 <- Cross.Sect(Eval.points, Max_IM, 6, 8, f); invisible(gc())

# Common color range to all subplots
col_lims <- c(0, 81)  


#######   FOURTH SUBPLOT   #######
c1 <- 1; c2 <- 6
fig4 <- plot_ly(data = opt.depth16,                            
                x = ~x, y = ~y, z = ~z,                      
                type = 'contour', 
                colors = my.palette,  reversescale = T,             
                contours = list(coloring = 'heatmap', showlabels=T, labelfont = list(size=14)),
                line = list(smoothing=1, width = 1, color = 'black')
) %>%
  colorbar(limits = col_lims) %>%                      # sets colorscale limits
  layout(xaxis = list(title = list(text=par_names[c1], standoff=0)), 
         yaxis = list(title = list(text=par_names[c2], standoff=10)) )


#######   FIFTH SUBPLOT   #######
c1 <- 1; c2 <- 8
fig5 <- plot_ly(data = opt.depth18,                            
                x = ~x, y = ~y, z = ~z,                      
                type = 'contour', 
                colors = my.palette,  reversescale = T,             
                contours = list(coloring = 'heatmap', showlabels=T, labelfont = list(size=14),
                                start=2, end=30, size=2),
                line = list(smoothing=1, width = 1, color = 'black')
) %>%
  colorbar(limits = col_lims) %>% 
  layout(xaxis = list(title = list(text=par_names[c1], standoff=0)), 
         yaxis = list(title = list(text=par_names[c2], standoff=0)) )


#######   SIXTH SUBPLOT   #######
c1 <- 6; c2 <- 8
fig6 <- plot_ly(data = opt.depth68,                            
                x = ~x, y = ~y, z = ~z,                      
                type = 'contour', 
                colors = my.palette,  reversescale = T,             
                contours = list(coloring = 'heatmap', showlabels=T, labelfont = list(size=14)),
#                                start=1, end=6, size=1),
                line = list(smoothing=1, width = 1, color = 'black'),
                height = 400, 
                width = 1400
) %>%
  colorbar(limits = col_lims) %>%                      # sets colorscale limits
  layout(xaxis = list(title = list(text=par_names[c1], standoff=0)), 
         yaxis = list(title = list(text=par_names[c2], standoff=0)) )


######  ADD OVERALL COLORBAR AND SETTINGS   ######

fig6 <- fig6 %>% 
  layout(font = list(family = "Balto", size = 18)) %>%
  colorbar(limits = col_lims, len=0.9, title = '',
           outlinecolor = 'black', outlinewidth = 1,
           ticksuffix = '%',
           tickmode = 'array', tickvals = seq(0,80,10)
  )

fig <- subplot(hide_colorbar(fig4), hide_colorbar(fig5), fig6,
               margin = 0.031, widths = c(0.322, 0.356, 0.322),
               titleX = T, titleY = T, shareX = T)

#####   SAVE IMAGE    #####

save_image(fig, '../Pictures/Non-Implausible_Plots/Gas/Opt_Depth_20%_bis.pdf', scale = 3)




##############################################################################
#                                                                            #
#       TOGETHER: MIN IMPL & OPT DEPTH PLOTS FOR GAS (20% MODEL DISCR)       #
#                                                                            #
##############################################################################

#######   UPDATE THIRD SUBPLOT   #######
col_lims <- c(1.7,11.3)  

c1 <- 6; c2 <- 8
fig3 <- plot_ly(data = min.Impl68,
                x = ~x, y = ~y, z = ~z,
                type = 'contour', colorscale = 'Viridis',
                contours = list(coloring = 'heatmap', showlabels=T, labelfont = list(size=14),
                                start=1, end=12, size=0.5),
                line = list(smoothing=1, width = 1.5, color = rgb(0.75, 0.75, 0.75)),
                height = 800, 
                width = 1400
) %>% 
  colorbar(limits = col_lims) %>%
  layout(xaxis = list(title = list(text=par_names[c1], standoff=0)),
         yaxis = list(title = list(text=par_names[c2], standoff=0)))

fig3 <- fig3 %>% 
  layout(font = list(family = "Balto", size = 18)) %>%
  colorbar(limits = col_lims, len=0.45, title = list(text = 'min I.M.', side = 'top'),
           tickmode = 'array', tickvals = 2:11)


#######   UPDATE SIXTH SUBPLOT   #######
col_lims <- c(0, 81)  

fig6 <- plot_ly(data = opt.depth68,                            
                x = ~x, y = ~y, z = ~z,                      
                type = 'contour', 
                colors = my.palette,  reversescale = T,             
                contours = list(coloring = 'heatmap', showlabels=T, labelfont = list(size=14)),
                #                                start=1, end=6, size=1),
                line = list(smoothing=1, width = 1, color = 'black'),
                height = 800, 
                width = 1400
) %>%
  colorbar(limits = col_lims) %>%                      # sets colorscale limits
  layout(xaxis = list(title = list(text=par_names[c1], standoff=0)), 
         yaxis = list(title = list(text=par_names[c2], standoff=0)) )

fig6 <- fig6 %>% 
  layout(font = list(family = "Balto", size = 18)) %>%
  colorbar(limits = col_lims, len=0.4, y=0.43, title = '',
           outlinecolor = 'black', outlinewidth = 1,
           ticksuffix = '%', tickmode = 'array', tickvals = seq(0,80,10)
  )


##########################################

fig <- subplot(nrows = 2,
               hide_colorbar(fig1), hide_colorbar(fig2), fig3,
               hide_colorbar(fig4), hide_colorbar(fig5), fig6,
               margin = 0.035, widths =  c(0.322, 0.356, 0.322),
               titleX = T, titleY = T, shareX = T)


save_image(fig, '../Pictures/Non-Implausible_Plots/Gas/MinImpl_and_OptDepth_20%.png', scale = 3)



###############################################################################



###############################################################################



###############################################################################



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



#############################################################################
#
# Da qui in poi, plot vari non usati in figure del paper

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





