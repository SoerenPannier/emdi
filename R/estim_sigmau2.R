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
  }

  return(sigmau2)
}
