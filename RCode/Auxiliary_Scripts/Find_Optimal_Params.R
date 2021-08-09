Find.Optimal.Parameters <- function(Train.ActInp, Train.regr,
                                    Cross.ActInp, Cross.regr,
                                    y.train, y.cross,
                                    beta, Cov.beta = matrix(0, length(beta), length(beta)),
                                    kernel, d0, s2.tot0, nug_frac0, 
                                    fixsigma=F, same.corr.lengths=F){

  # Convert data frames into matrices if necessary
  Train.ActInp <- as.matrix(Train.ActInp)
  Train.regr <- as.matrix(Train.regr)
  Cross.ActInp <- as.matrix(Cross.ActInp)
  Cross.regr <- as.matrix(Cross.regr)

  # Define size variables
  n <- nrow(Train.ActInp)      # Size of Training Set
  N <- nrow(Cross.ActInp)      # Size of Cross-Validation Set
  
  # Prior mean at training points
  Prior.mean.Train <- Train.regr %*% beta           # n-dim vector
  # Prior mean at cross-validation points
  Prior.mean.Cross <- Cross.regr %*% beta           # N-dim vector
  # Regression residuals
  e <- y.train - Prior.mean.Train                   # n-dim vector
  
  # Regression component of prior covariance at training points
  Regress.cov.train <- Train.regr %*% Cov.beta %*% t(Train.regr)              # nxn = nxq qxq qxn
  # Regression component of prior covariance between cross-validation and training points
  Regress.cov.crosstrain <- Cross.regr %*% Cov.beta %*% t(Train.regr)         # Nxn = Nxq qxq qxn
  # Regression component of prior variance at cross-validation points
  Regress.var.cross <- diag(Cross.regr %*% Cov.beta %*% t(Cross.regr))        # N-dim vector
  

  ## Part dependent on sigma, d, nugget: define the function that will be maximised
  
  CV_objective <- function(sigma2.tot, nugget_frac, d){
    nu2 <- nugget_frac*sigma2.tot               # Variance of nugget term (noise due to inactive inputs)
    sig2 <- (1-nugget_frac)*sigma2.tot  
    
    # Prior covariance at training points
    A <- Regress.cov.train +
         sig2*Corr.fun(Train.ActInp, Train.ActInp, d, kernel) +  # nxn, covariance of zero-mean process eta
         nu2*diag(n)                                              # nxn, covariance of nugget term
    
    # Prior variance at CV points
    Prior.var.cross <- Regress.var.cross + sig2                  # N-dim vector
    
    # Prior covariance between training and CV points
    tx <- Regress.cov.crosstrain +                               # Nxn, covariance of regression component
          sig2*Corr.fun(Cross.ActInp, Train.ActInp, d, kernel)   # Nxn, covariance of eta term
    
    ## FINAL COMPUTATIONS FOR EMULATOR: POSTERIOR MEAN AND VARIANCE AT CV POINTS ##
    
    # Posterior mean at test points
    K <- t( solve(A, t(tx)) )                           # Nxn: tx*(A^-1)
    Post.mean <- Prior.mean.Cross + K%*%e               # N-dim vector
    
    # Posterior variance at test points
    # "a" is the "correction" term to be subtracted from the prior variance:
    # it is  a <- diag(K%*%t(tx)), computed as follows for speed gain
    a <- array(dim=c(N,1))                              # Lx1
    for (j in 1:N){
      a[j] <- K[j,] %*% tx[j,]                          # (1xn) x (nx1)
    }
    # Posterior variance, add nu to account for variability due to inactive inputs
    Post.var <- Prior.var.cross  - a + nu2   # Nx1 matrix
    
    densities <- dnorm(y.cross, mean = Post.mean, sd = sqrt(Post.var), log = T)
    return( sum(densities)/N )
  }
  
  ## FIND THE OPTIMAL PARAMETERS (maximise the function)
  
  # Define the function to be maximised, according to whether sigma is fixed or not
  # and according to whether the same correlation length should be used for all active inputs
  Nd <- length(d0)
  if (fixsigma){
    if (same.corr.lengths){ # same correl lengths, no sigma2 optimised
      f <- function(pars) {CV_objective(s2.tot0, pars[1], rep(exp(pars[2]),Nd) )}
      x0 <- c(nug_frac0, log(d0[1]))
      opt.par <- optim(par = x0, fn = f, method = "L-BFGS-B", 
                       lower = c(0.0001, -Inf), upper = c(1, Inf),
                       control = list(trace=6, fnscale=-1))
    } else { # different correl lengths, no sigma2 optimised
      f <- function(pars) {CV_objective(s2.tot0, pars[1], exp(pars[2:(Nd+1)]) )}
      x0 <- c(nug_frac0, log(d0))
      opt.par <- optim(par = x0, fn = f, method = "L-BFGS-B", 
                       lower = c(0.0001, -Inf), upper = c(1, Inf),
                       control = list(trace=6, fnscale=-1))
    }
    } else {
      if (same.corr.lengths){ # same correl lengths, but sigma2 is optimised
        f <- function(pars) {CV_objective(exp(pars[1]), pars[2], rep(exp(pars[3]),Nd) ) }
        x0 <- c(log(s2.tot0), nug_frac0, log(d0[1]))
        opt.par <- optim(par = x0, fn = f, method = "L-BFGS-B", 
                         lower = c(-Inf, 0.0001, rep(-Inf,Nd)), upper = c(Inf, 1, rep(Inf,Nd)),
                         control = list(trace=6, fnscale=-1))
      } else { # different correl lengths, sigma2 is optimised
        f <- function(pars) {CV_objective(exp(pars[1]), pars[2], exp(pars[3:(Nd+2)]) ) }
        x0 <- c(log(s2.tot0), nug_frac0, log(d0))
        opt.par <- optim(par = x0, fn = f, method = "L-BFGS-B", 
                         lower = c(-Inf, 0.0001, rep(-Inf,Nd)), upper = c(Inf, 1, rep(Inf,Nd)),
                         control = list(trace=6, fnscale=-1))
      }
    }
  
  # Return the optimised values
  
  if (same.corr.lengths){ # then append the last element of the optimised vector (correlation length) to itself Nd-1 times
    x <- opt.par$par
    opt.par$par <- c(x, rep(x[length(x)], Nd-1) )
  }
  
  if (fixsigma){
    s2.opt  <- s2.tot0*(1-opt.par$par[1])
    nu2.opt <- s2.tot0* opt.par$par[1]
    d.opt <- exp(opt.par$par[2:(Nd+1)]) 
  } else {
    s2.opt  <- exp(opt.par$par[1])*(1-opt.par$par[2])
    nu2.opt <- exp(opt.par$par[1])* opt.par$par[2]
    d.opt <- exp(opt.par$par[3:(Nd+2)])
  }
  val <- opt.par$value
  
  Opt.pars <- list(s2=s2.opt, nu2=nu2.opt, d=d.opt, obj=val)
  return(Opt.pars)
  
  }



