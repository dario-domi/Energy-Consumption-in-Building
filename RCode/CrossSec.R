#' Cross.Sect applies the summary statistics f to the values associated 
#' with a set of p-dimensional points, across any two of the p dimensions.
#' 
#' @param X  matrix with `dim` (N, p), p>=2. Should be thought of as collection 
#'           of N p-dimensional points.
#' @param vals vector of length N, containing values associated with each row in X.
#' @param Cx integer between 1 and p. First of the two components along which the
#'           summary statistics f will be computed.
#' @param Cy integer between 1 and p. Second component over which [...].
#' @param f  a function returning a single number when applied to a numeric vector.
#'           `mean` by default.
#' @value A dataframe with three columns, x, y, z:
#'        z[i] is computed by applying `f` to all values of `vals` associated 
#'        with points whose (Cx,Cy) coordinates are in a neighbourood of (x[i], y[i]).


Cross.Sect <- function(X, vals, Cx, Cy, f=mean){
  
  # Create x and y grids, each with Nx/Ny subintervals
  Nx <- 25                  # number of grid-subintervals along the x axis
  Rx <- range(X[,Cx])
  x <- seq(Rx[1], Rx[2], len = Nx+1)
  Ny <- 25
  Ry <- range(X[,Cy])
  y <- seq(Ry[1], Ry[2], len = Ny+1)  
         
  z <- matrix(nrow = Nx+1, ncol = Ny+1)
  
  for (i in 1:(Nx+1)){
    ind1 <- max(i-1, 1)
    ind2 <- min(i+1, Nx+1)
    mx <- (x[ind1]+x[i])/2
    Mx <- (x[i]+x[ind2])/2
    x.compat <- (X[,Cx]>=mx) & (X[,Cx]<=Mx)
    
    # Restrict search to indices of interest only (x.compat)
    Inputs.temp <- X[x.compat, , drop=F]
    vals.temp <- vals[x.compat]
    
    for (j in 1:(Ny+1)){
      ind1 <- max(j-1, 1)
      ind2 <- min(j+1, Ny+1)
      my <- (y[ind1]+y[j])/2
      My <- (y[j]+y[ind2])/2
      
      y.compat <- (Inputs.temp[,Cy]>=my) & (Inputs.temp[,Cy]<=My)
      z[i,j] <- f(vals.temp[y.compat])
    }
  }

  library(utils)
  ret <- expand.grid(x = x, y = y)
  ret$z <- as.vector(z)
  return(ret)
}









