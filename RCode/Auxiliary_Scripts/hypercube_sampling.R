# Generate input samples using hypercube sampling

library(lhs)
setwd("/Users/hailiangdu/HDU/Ongoing projects/Building_Energy/R_code")
N_var<-8
N_run<-30
input_var<-matrix(0,nrow=N_var,ncol=2)
input_var[1,1]<-17.5 #Heating setpoint
input_var[1,2]<-20.5

input_var[2,1]<-0.6 #Gas boiler seasonal efficiency
input_var[2,2]<-0.75

input_var[3,1]<-1.788-1.788*0.02 #Glazing (both with low emissivity coating)
input_var[3,2]<-1.788+1.788*0.02

input_var[4,1]<-0.691-0.691*0.05 #Glazing G Value (solar transmittance) 
input_var[4,2]<-0.691+0.691*0.05

input_var[5,1]<-0.544-0.544*0.15 #External walls [g]  ( W/m2K)
input_var[5,2]<-0.544+0.544*0.15

input_var[6,1]<-0.213-0.213*0.15 #Roof [g2]   (W/m2K)
input_var[6,2]<-0.213+0.213*0.15

input_var[7,1]<-0.337-0.337*0.05 #Floor [h]  ( W/m2K)
input_var[7,2]<-0.337+0.337*0.05

input_var[8,1]<-0.2 #Infiltration (ac/h) [f]
input_var[8,2]<-0.95

inputs<-randomLHS(N_run,N_var)

for (i in 1:N_var) {
  inputs[,i]<-inputs[,i]*(input_var[i,2]-input_var[i,1])+input_var[i,1]
  
}

write.csv(inputs, file = "inputs.csv")
