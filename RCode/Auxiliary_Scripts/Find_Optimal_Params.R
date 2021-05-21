Find.Optimal.Parameters <- function(Train.ActInp, Train.regr,
                                    Cross.ActInp, Cross.regr,
                                    y.train, y.cross,
                                    kernel, d0, s2.tot0, nug_frac0){

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
  

  ## Part dependent on sigma, d, nugget: define the function which will be maximised
  
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
  
  # FIND THE OPTIMAL PARAMETERS (maximise the function)
  Nd <- length(d0)
  f <- function(pars) {CV_objective(exp(pars[1]), pars[2], exp(pars[3:(Nd+2)]) ) }
  x0 <- c(log(s2.tot0), nug_frac0, log(d0))
  opt.par <- optim(par = x0, fn = f, method = "L-BFGS-B", 
                   lower = c(-Inf, 0.0001, rep(-Inf,Nd)), upper = c(Inf, 1, rep(Inf,Nd)),
                   control = list(trace=5, fnscale=-1)) 

  # Return the optimised values
  s2.opt  <- exp(opt.par$par[1])*(1-opt.par$par[2])
  nu2.opt <- exp(opt.par$par[1])*   opt.par$par[2]
  d.opt <- exp(opt.par$par[3:(Nd+2)])    
  val <- opt.par$value
  
  Opt.pars <- list(s2=s2.opt, nu2=nu2.opt, d=d.opt, obj=val)
  return(Opt.pars)
  
  }



