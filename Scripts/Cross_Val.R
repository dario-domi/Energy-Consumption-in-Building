# This function is old. Check before using, probably rewritten within Store_CV_results

Cross_Val <- function(Design.par.full, Design.regr.full, y.full, beta, Cov.beta, d, nu, sigma2, string){
  N <- dim(Design.par.full)[1]
  p <- dim(Design.par.full)[2]
  q <- dim(Design.regr.full)[2]
  
  ind.full <- 1:N
  
  M <- array(dim=c(N,1))
  V <- array(dim=c(N,1))
  
  for (i in 1:N){
    ind <- ind.full[-i]
    Design.par  <- Design.par.full[ind,]
    Design.regr <- Design.regr.full[ind,]
    test.par    <- array( Design.par.full[i,],  dim=c(1,p) )
    test.regr   <- array( Design.regr.full[i,], dim=c(1,q) )
    y <- y.full[ind]
    
    res <- emul(test.par, test.regr, Design.par, Design.regr, y, beta, Cov.beta, d, nu, sigma2, string)
    M[i] <- res$Mean
    V[i] <- res$Variance
  }

  return( list("Mean"=M, "Variance"=V) )
}









