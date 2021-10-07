# X: Nxp (p>=2)
# y: vector of length N, values corresponding to each p-dimensional row in X
# C1: first of the two dimensions over which the plot will be made
# C2: second dimension over which the plot will be made

# nx: number of grid points in the x-axis
# ny: number of grid points in the x-axis


Cross.Sect <- function(Inputs, vals, Cx, Cy, f){
  
  # Create x and y grids, each with Nx/Ny subintervals
  Nx <- 25                  # number of grid-subintervals along the x axis
  Rx <- range(Inputs[,Cx])
  x <- seq(Rx[1], Rx[2], len = Nx+1)
  Ny <- 25
  Ry <- range(Inputs[,Cy])
  y <- seq(Ry[1], Ry[2], len = Ny+1)  
         
  z <- matrix(nrow = Nx+1, ncol = Ny+1)
  
  for (i in 1:(Nx+1)){
    ind1 <- max(i-1, 1)
    ind2 <- min(i+1, Nx+1)
    mx <- (x[ind1]+x[i])/2
    Mx <- (x[i]+x[ind2])/2
    x.compat <- (Inputs[,Cx]>=mx) & (Inputs[,Cx]<=Mx)
    
    # Restrict search to indices of interest only (x.compat)
    Inputs.temp <- Inputs[x.compat, , drop=F]
    vals.temp <- vals[x.compat]
    
    for (j in 1:(Ny+1)){
      ind1 <- max(j-1, 1)
      ind2 <- min(j+1, Ny+1)
      my <- (y[ind1]+y[j])/2
      My <- (y[j]+y[ind2])/2
      
      y.compat <- (Inputs.temp[,Cy]>=my) & (Inputs.temp[,Cy]<=My)
      z[j,i] <- f(vals.temp[y.compat])
    }
  }
  return(list(x,y,z))
}









