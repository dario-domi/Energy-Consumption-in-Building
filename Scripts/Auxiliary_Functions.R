# This script contains a series of custom functions used to investigate the 
# "Building's Energy" project carried out with Mohammad Royapoor.


#######################################################################################################


# The following function linearly rescales the columns of a matrix, by mapping the ranges 
# specified in the body of the function into [-1,1]
Rescale.Linearly <- function(X){
  
  # dim(X) = N x N_var
  
  # Set the ranges...
  N_var<-8
  Rang <- matrix(NA, nrow=2, ncol=N_var)
  
  Rang[1,1]<-17.5     # Heating setpoint [Celcius degrees]
  Rang[2,1]<-20.5
  
  Rang[1,2]<-0.6      # Gas boiler seasonal efficiency
  Rang[2,2]<-0.75
  
  Rang[1,3]<-0.04     # External wall thickness [m]
  Rang[2,3]<-0.063
  
  Rang[1,4]<-0.15     # ~Roof thickness [m]
  Rang[2,4]<-0.21
  
  Rang[1,5]<-0.045    # ~Floor thickness [m] 
  Rang[2,5]<-0.055
  
  Rang[1,6]<-0.2      # Infiltration [ac/h]
  Rang[2,6]<-0.95 
  
  Rang[1,7]<-6.15e-06 # DHW consumption [litre/day]
  Rang[2,7]<-2.20e-05
  
  Rang[1,8]<-1.05     # Cooking [W/m2]
  Rang[2,8]<-6.3
  
  # ... and rescale the input variables
  for (i in 1:N_var){
    a <- Rang[1,i]
    b <- Rang[2,i]
    x <- X[,i]  # Nx1
    X[,i] <- 2*(x-a)/(b-a) -1
  }
  
  return(X)
}

################################################################################
# This function

Cross_Val <- function(Design.points.full, Design.regr.full, y.full, beta, Cov.beta, d, nu, sigma2, string){
  n <- dim(Design.points.full)[1]
  
  # PRIOR MEAN OF DESIGN POINTS
  prior.mean.Design.full <- Design.regr.full %*% beta              # n-dim vector
  
  # PRIOR COVARIANCE OF DESIGN POINTS
  a <- Design.regr.full %*% Cov.beta %*% t(Design.regr.full)       # nxn, prior covariance of regression part
  b <- sigma2*corr.fun(Design.points.full, Design.points.full, d, string)  # nxn, prior GP cov
  c <- nu*diag(n)                                                  # variance of nugget term
  A.full <- a + b + c  
  
  # PRIOR ERROR (REAL VALUE MINUS PREDICTION)
  e.full <- y - prior.mean.Design.full
  
  # VARIABLES TO STORE POSTERIOR CROSS-VALIDATED MEAN AND VARIANCE
  M <- array(dim=c(n,1))
  V <- array(dim=c(n,1))
  
  ind.full <- 1:n
  
  for (i in 1:n){
    
    ind <- ind.full[-i]
    
    # Quantities needed in emulation computation
    y <- y.full[ind]
    A <- A.full[ind, ind]
    e <- e.full[ind]
    tx <- A.full[ind,i, drop=F]
    
    # POSTERIOR MEAN AND COVARIANCE FOR INPUT PARAMETERS
    K <- solve(A, tx)                                  
    post.mean <- prior.mean.Design.full[i] + t(e)%*%K  
    post.var <-   A.full[i,i]   - t(tx)%*%K
    
    M[i] <- post.mean
    V[i] <- post.var
    
    if (i%%10 == 0){
      cat(sprintf("Iteration number %d out of %d.\n", i, n))
    }
    
  }
  
  return( list("Mean"=M, "Variance"=V) )
}



###########################################################################################
# The following function creates a list with element names as in "element.names".
# By default (ie, no variable "indices" provided) each element of the list is an Nx2 matrix 
# with all entries equal to "val", and column names "Mean", "Var".
# If "indices" is provided as a vector of integers, only the list element corresponding
# to those indices will be populated by the matrix as above, the remaining ones will be empty.

Create.my.list <- function(N, element.names, indices=c(1:length(element.names)), val=0){
  
  my.list <- sapply(element.names, function(x) NULL)
  
  mat <- matrix(data = val, nrow = N, ncol = 2)
  colnames(mat) <- c("Mean", "Var")
  
  for (i in indices){
    my.list[[i]] <- mat
  }
  return(my.list)
  
}






