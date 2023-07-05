##################################################
# Produce bar plot of observed gas consumptions
##################################################



load('RCode/RData/Inputs/Simulated_and_Observed_Gas.RData')         # Gas data (observed and simulated)

gas <- table(sample(1:12, 100, replace=T))
names(gas) <- colnames(Gas.Obs)
gas[] <- Gas.Obs

barplot(gas, col=rainbow(12))
barplot(gas, col=rainbow(33)[12:30])
barplot(gas, col=rainbow(25)[12:25])
barplot(gas, col=rainbow(40)[7:20])
barplot(gas, col=rainbow(40)[7:20])
barplot(gas, col=rainbow(40)[7:20])



#oldpar <- par()
pdf("Pictures/Gas/Obs_Gas.pdf", width = 10, height = 7.5)
par(oma=c(0,3,0,0), lwd=2)
barplot(gas, col="chocolate", space = 0.5, yaxt="n",
        cex.names= 1.45,  # for size of x tick labels
        main = "Monthly Energy Consumption", cex.main=2)
#        cex.axis = 1.3 # for size of y axis labels
#        cex.lab=1.6     # for size of y label
axis(2, las=2, cex.axis=1.5)
mtext(side = 1, text = "2016", line = 3.2, cex=1.5) # y label
mtext(side = 2, text = "Energy [kWh]", line = 5, cex=1.6) # y label
graphics.off()






