#' Function for estimating sigmau2 with REML.
#'
#' This function estimates sigmau2.
#'
#' @param interval interval for the algorithm.
#' @param direct direct estimator.
#' @param x matrix with explanatory variables.
#' @param vardir direct variance.
#' @param areanumber number of domains.
#' @return estimated sigmau2.
#' @keywords internal


Reml <- function(interval, direct, x, vardir, areanumber) {

  A.reml <- function(interval, direct, x, vardir, areanumber) {
    psi <- matrix(c(vardir), areanumber,1)
    Y <- matrix(c(direct),areanumber,1)
    X <- x
    Z.area <- diag(1,areanumber)
    sigma.u_log <- interval[1]
    I <- diag(1, areanumber)
    #V is the variance covariance matrix
    V <- sigma.u_log * Z.area%*%t(Z.area) + I*psi[,1]
    Vi <- solve(V)
    Xt <- t(X)
    XVi <- Xt%*%Vi
    Q <- solve(XVi%*%X)
    P <- Vi - (Vi%*%X%*%Q%*%XVi)
    b.s <- Q%*%XVi%*%Y

    ee = eigen(V)
    -(areanumber/2) * log(2*pi) - 0.5 * sum(log(ee$value)) - (0.5) * log(det(t(X)%*%Vi%*%X)) - (0.5) * t(Y)%*%P%*%Y
    #(2*pi)^(-(areanumber/2)) * exp(-0.5 * sum(log(ee$value))) * exp(-(0.5) * log(det(t(X)%*%Vi%*%X))) * exp(-(0.5) * t(Y)%*%P%*%Y)
  }
  ottimo <- optimize(A.reml, interval, maximum = TRUE,
                     vardir = vardir, areanumber = areanumber,
                     direct = direct, x = x)

  estsigma2u <- ottimo$maximum

  return(sigmau_reml = estsigma2u)
}


#' Function for estimating sigmau2 following Li and Lahiri.
#'
#' This function estimates sigmau2 using the adjusted maximum residual
#' likelihood.
#'
#' @param interval interval for the algorithm.
#' @param direct direct estimator.
#' @param x matrix with explanatory variables.
#' @param vardir direct variance.
#' @param areanumber number of domains.
#' @return estimated sigmau2.
#' @keywords internal

AMRL <- function(interval, direct, x, vardir, areanumber) {

  AR <- function(interval, direct, x, vardir, areanumber){
    psi <- matrix(c(vardir), areanumber, 1)
    Y <- matrix(c(direct), areanumber, 1)
    X <- x
    Z.area <- diag(1, areanumber)
    sigma.u_log <- interval[1]
    I <- diag(1, areanumber)
    #V is the variance covariance matrix
    V <- sigma.u_log * Z.area%*%t(Z.area) + I * psi[,1]
    Vi <- solve(V)
    Xt <- t(X)
    XVi <- Xt%*%Vi
    Q <- solve(XVi%*%X)
    P <- Vi - (Vi%*%X%*%Q%*%XVi)
    b.s <- Q%*%XVi%*%Y

    ee <- eigen(V)
    log(sigma.u_log) - (areanumber/2) * log(2*pi) - 0.5 * sum(log(ee$value)) - (0.5) * log(det(t(X)%*%Vi%*%X)) - (0.5) * t(Y)%*%P%*%Y
  }


  ottimo <- optimize(AR, interval, maximum = TRUE,
                     vardir = vardir, areanumber = areanumber,
                     direct = direct, x = x)

  estsigma2u <- ottimo$maximum

  return(sigmau_amrl = estsigma2u)
}


#' Function for estimating sigmau2 following Yoshimori and Lahiri.
#'
#' This function estimates sigmau2 using the adjusted maximum residual
#' likelihood.
#'
#' @param interval interval for the algorithm.
#' @param direct direct estimator.
#' @param x matrix with explanatory variables.
#' @param vardir direct variance.
#' @param areanumber number of domains.
#' @return estimated sigmau2.
#' @keywords internal

AMRL_YL <- function(interval, direct, x, vardir, areanumber) {

  AR_YL <- function(interval, direct, x, vardir, areanumber){
    psi <- matrix(c(vardir), areanumber, 1)
    Y <- matrix(c(direct), areanumber, 1)
    X <- x
    Z.area <- diag(1, areanumber)
    sigma.u_log <- interval[1]
    I <- diag(1, areanumber)
    #V is the variance covariance matrix
    V <- sigma.u_log * Z.area%*%t(Z.area) + I * psi[,1]
    Vi <- solve(V)
    Xt <- t(X)
    XVi <- Xt%*%Vi
    Q <- solve(XVi%*%X)
    P <- Vi - (Vi%*%X%*%Q%*%XVi)
    b.s <- Q%*%XVi%*%Y
    Bd <- diag(vardir/(sigma.u_log + vardir))

    ee <- eigen(V)
    #atan(sum(diag((I - Bd))))^(1/areanumber) * (2*pi)^(-(areanumber/2)) * exp(-0.5 * sum(log(ee$value))) * exp(-(0.5) * log(det(t(X)%*%Vi%*%X))) * exp(-(0.5) * t(Y)%*%P%*%Y)
    (1/areanumber) * log((atan(sum(diag(I - Bd)))^-1)) - (areanumber/2) * log(2*pi) - 0.5 * sum(log(ee$value)) - (0.5) * log(det(t(X)%*%Vi%*%X)) - (0.5) * t(Y)%*%P%*%Y
  }

  ottimo <- optimize(AR_YL, interval, maximum = TRUE,
                     vardir = vardir, areanumber = areanumber,
                     direct = direct, x = x)

  estsigma2u <- ottimo$maximum

  return(sigmau_amrl_yl = estsigma2u)
}


#' Function for estimating sigmau2 following Li and Lahiri.
#'
#' This function estimates sigmau2 using the adjusted maximum profile
#' likelihood.
#'
#' @param interval interval for the algorithm.
#' @param direct direct estimator.
#' @param x matrix with explanatory variables.
#' @param vardir direct variance.
#' @param areanumber number of domains.
#' @return estimated sigmau2.
#' @keywords internal

AMPL <- function(interval, direct, x, vardir, areanumber) {

  AP <- function(interval, direct,x, vardir, areanumber){
    psi <- matrix(c(vardir), areanumber, 1)
    Y <- matrix(c(direct), areanumber, 1)
    X <- x
    Z.area <- diag(1, areanumber)
    sigma.u_log <- interval[1]
    I <- diag(1, areanumber)
    # V is the variance covariance matrix
    V <- sigma.u_log * Z.area%*%t(Z.area) + I * psi[,1]
    Vi <- solve(V)
    Xt <- t(X)
    XVi <- Xt%*%Vi
    Q <- solve(XVi%*%X)
    P <- Vi - (Vi%*%X%*%Q%*%XVi)
    b.s <- Q%*%XVi%*%Y

    ee = eigen(V)
    log(sigma.u_log) - (areanumber/2) * log(2*pi) - 0.5 * sum(log(ee$value)) - (0.5) * t(Y)%*%P%*%Y
  }

  ottimo <- optimize(AP, interval, maximum = TRUE,
                     vardir = vardir, areanumber = areanumber,
                     direct = direct, x = x)

  estsigma2u <- ottimo$maximum

  return(sigmau_ampl = estsigma2u)
}


#' Function for estimating sigmau2 following Yoshimori and Lahiri.
#'
#' This function estimates sigmau2 using the adjusted maximum profile
#' likelihood.
#'
#' @param interval interval for the algorithm.
#' @param direct direct estimator.
#' @param x matrix with explanatory variables.
#' @param vardir direct variance.
#' @param areanumber number of domains.
#' @return estimated sigmau2.
#' @keywords internal

AMPL_YL <- function(interval, direct, x, vardir, areanumber) {

  AP_YL <- function(interval, direct, x, vardir, areanumber){
    psi <- matrix(c(vardir), areanumber, 1)
    Y <- matrix(c(direct), areanumber, 1)
    X <- x
    Z.area <- diag(1, areanumber)
    sigma.u_log <- interval[1]
    I <- diag(1, areanumber)
    # V is the variance covariance matrix
    V <- sigma.u_log * Z.area%*%t(Z.area) + I * psi[,1]
    Vi <- solve(V)
    Xt <- t(X)
    XVi <- Xt%*%Vi
    Q <- solve(XVi%*%X)
    P <- Vi - (Vi%*%X%*%Q%*%XVi)
    b.s <- Q%*%XVi%*%Y
    Bd <- diag(vardir/(sigma.u_log + vardir))

    ee = eigen(V)
    #(1/areanumber) * log((tan(sum(diag(I - Bd)))^-1)) - (areanumber/2) * log(2*pi) - 0.5 * sum(log(ee$value)) - (0.5) * t(Y)%*%P%*%Y
    (atan(sum(diag((I - Bd)))))^(1/areanumber) * (2*pi)^(-(areanumber/2)) * exp(-0.5 * sum(log(ee$value))) * exp(-(0.5) * t(Y)%*%P%*%Y)
    #sigma.u_log * (2*pi)^(-(areanumber/2)) * exp(-0.5 * sum(log(ee$value))) * exp(-(0.5) * t(Y)%*%P%*%Y)
  }

  ottimo <- optimize(AP_YL, interval, maximum = TRUE,
                     vardir = vardir, areanumber = areanumber,
                     direct = direct, x = x)
  estsigma2u <- ottimo$maximum

  return(sigmau_ampl_yl = estsigma2u)
}


#' Function for estimating sigmau2 using maximum likelihood.
#'
#' This function estimates sigmau2 using the maximum profile
#' likelihood.
#'
#' @param interval interval for the algorithm.
#' @param direct direct estimator.
#' @param x matrix with explanatory variables.
#' @param vardir direct variance.
#' @param areanumber number of domains.
#' @return estimated sigmau2.
#' @keywords internal

MPL <- function(interval, direct, x, vardir, areanumber) {

  ML <- function(interval, direct,x, vardir, areanumber){
    psi <- matrix(c(vardir), areanumber, 1)
    Y <- matrix(c(direct), areanumber, 1)
    X <- x
    Z.area <- diag(1, areanumber)
    sigma.u_log <- interval[1]
    I <- diag(1, areanumber)
    # V is the variance covariance matrix
    V <- sigma.u_log * Z.area%*%t(Z.area) + I * psi[,1]
    Vi <- solve(V)
    Xt <- t(X)
    XVi <- Xt%*%Vi
    Q <- solve(XVi%*%X)
    P <- Vi - (Vi%*%X%*%Q%*%XVi)
    b.s <- Q%*%XVi%*%Y

    ee = eigen(V)
    -(areanumber/2) * log(2*pi) - 0.5 * sum(log(ee$value)) - (0.5) * t(Y)%*%P%*%Y
    # (2*pi)^(-(areanumber/2)) * exp(- 0.5 * sum(log(ee$value))) * exp(-(0.5) * t(Y)%*%P%*%Y)
  }

  ottimo <- optimize(ML, interval, maximum = TRUE,
                     vardir = vardir, areanumber = areanumber,
                     direct = direct, x = x)

  estsigma2u <- ottimo$maximum

  return(sigmau_mpl = estsigma2u)
}

#' Function for estimating sigmau2 and beta following Ybarra and Lohr.
#'
#' @param direct direct estimator.
#' @param x matrix with explanatory variables.
#' @param vardir direct variance.
#' @param Ci mean squared error of x.
#' @param areanumber number of domains.
#' @param p number of covariates.
#' @param tol tolerance value for the convergence of weights.
#' @param maxit maximum number of iterations.
#' @return estimated sigmau2 and estimated beta coefficients.
#' @keywords internal

ybarralohr <- function(direct, x, vardir, Ci, areanumber,p, tol, maxit) {
  
  hatwi <- rep(1,areanumber) 
  eps <- 1
  it <- 0
  x <- t(x)
  
  sqrtinv.matrix <- function(mat,maxiter=50) {
    stopifnot(nrow(mat) == ncol(mat))
    niter <- 0
    direct <- mat
    z <- diag(rep(1,nrow(mat)))
    for (niter in 1:maxiter) {
      direct.temp <- 0.5*(direct + ginv(z))
      z <- 0.5*(z + ginv(direct))
      direct <- direct.temp
    }
    return(list(sqrt=direct, sqrt.inv=z))
  }#WORKS ONLY WITH SYMMETRIC AND POSITIVE DEFINITE MATRIX
  
  while(eps > tol & it < maxit){ # Loops end when tolerance value for the convergence
    # of weights is reached and max number of iterations
    
    hatwi.old <- hatwi 
    CC <- matrix(0,p,p) 
    for(i in 1:areanumber){ 
      CC <- CC + hatwi[i]*Ci[,,i]
    }
    
    G <- t(hatwi*t(x))%*%t(x) # G = Sum over m(w times X^T X)
    
    
    G.sqrtinv <- sqrtinv.matrix(G)$sqrt.inv 
    GG <- G.sqrtinv %*% CC %*% G.sqrtinv 
    
    decomp <- eigen(GG)
    P <- decomp$vectors
    Lambda <- diag(decomp$values)
    
    dd <- which((1-decomp$values)>1/areanumber)
    Lambdajj <- rep(0,p)
    Lambdajj[dd] <- 1/(1-decomp$values[dd])
    D <- diag(Lambdajj) 
    
    wXdirect <- t(t(as.matrix(hatwi*direct,areanumber,1))%*%t(x)) 
    
    # Beta estimate
    Beta.hat.w <- G.sqrtinv %*% P %*% D %*% t(P) %*% G.sqrtinv %*% wXdirect
    
    thbCihb <- NULL
    for(i in 1:areanumber){
      thbCihb[i] <- t(Beta.hat.w)%*%Ci[,,i]%*%Beta.hat.w 
    }
    
    # Estimate variance of the random effect
    sigmau2 <- (1/(areanumber - p))* sum((direct-t(x)%*%Beta.hat.w)^2 - vardir - thbCihb)
    
    # Truncation to zero
    if (sigmau2 <= 0) sigmau2<-0
    
    # Update hatwi
    hatwi <- 1/(sigmau2 + vardir + thbCihb)
    
    # Check convergence
    eps <- mean(abs(hatwi - hatwi.old))
    it <- it + 1
    
  }#End while
  
  estsigma2u <- sigmau2
  
  return(list(sigmau_YL = estsigma2u, betahatw = Beta.hat.w, thbCihb = thbCihb)) 
  
}




#' Wrapper function for the estmation of sigmau2
#'
#' This function wraps the different estimation methods for sigmau2.
#'
#' @param vardir direct variance.
#' @param precision precision criteria for the estimation of sigmau2.
#' @param maxiter maximum of iterations for the estimation of sigmau2.
#' @param interval interval for the algorithm.
#' @param direct direct estimator.
#' @param x matrix with explanatory variables.
#' @param areanumber number of domains.
#' @return estimated sigmau2.
#' @keywords internal

wrapper_estsigmau2 <- function(framework, method, interval) {

  sigmau2 <- if (method == "reml") {
    Reml(interval = interval, vardir = framework$vardir, x = framework$model_X,
                          direct = framework$direct, areanumber = framework$m)
  } else if (method == "amrl") {
    AMRL(interval = interval, vardir = framework$vardir, x = framework$model_X,
                    direct = framework$direct, areanumber = framework$m)
  } else if (method == "amrl_yl") {
    AMRL_YL(interval = interval, vardir = framework$vardir, x = framework$model_X,
         direct = framework$direct, areanumber = framework$m)
  } else if (method == "ampl") {
    AMPL(interval = interval, vardir = framework$vardir, x = framework$model_X,
         direct = framework$direct, areanumber = framework$m)
  } else if (method == "ampl_yl") {
    AMPL_YL(interval = interval, vardir = framework$vardir, x = framework$model_X,
                    direct = framework$direct, areanumber = framework$m)
  } else if (method == "ml") {
    MPL(interval = interval, vardir = framework$vardir, x = framework$model_X,
         direct = framework$direct, areanumber = framework$m)
  } else if (method == "moment") {
    ybarralohr(vardir = framework$vardir, direct = framework$direct, x = framework$model_X,
       Ci = framework$Ci, tol = framework$tol, maxit = framework$maxit, 
       p = framework$p, areanumber = framework$m)
  }

  return(sigmau2)
}
