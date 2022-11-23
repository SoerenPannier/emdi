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
#' @noRd


Reml <- function(interval, direct, x, vardir, areanumber) {
  A.reml <- function(interval, direct, x, vardir, areanumber) {
    psi <- matrix(c(vardir), areanumber, 1)
    Y <- matrix(c(direct), areanumber, 1)
    X <- x
    Z.area <- diag(1, areanumber)
    sigma.u_log <- interval[1]
    I <- diag(1, areanumber)
    # V is the variance covariance matrix
    V <- sigma.u_log * Z.area %*% t(Z.area) + I * psi[, 1]
    Vi <- solve(V)
    Xt <- t(X)
    XVi <- Xt %*% Vi
    Q <- solve(XVi %*% X)
    P <- Vi - (Vi %*% X %*% Q %*% XVi)

    ee <- eigen(V)
    -(areanumber / 2) * log(2 * pi) - 0.5 * sum(log(ee$value)) - (0.5) *
      log(det(t(X) %*% Vi %*% X)) - (0.5) * t(Y) %*% P %*% Y
  }
  ottimo <- optimize(A.reml, interval,
    maximum = TRUE,
    vardir = vardir, areanumber = areanumber,
    direct = direct, x = x
  )

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
#' @noRd

AMRL <- function(interval, direct, x, vardir, areanumber) {
  AR <- function(interval, direct, x, vardir, areanumber) {
    psi <- matrix(c(vardir), areanumber, 1)
    Y <- matrix(c(direct), areanumber, 1)
    X <- x
    Z.area <- diag(1, areanumber)
    sigma.u_log <- interval[1]
    I <- diag(1, areanumber)
    # V is the variance covariance matrix
    V <- sigma.u_log * Z.area %*% t(Z.area) + I * psi[, 1]
    Vi <- solve(V)
    Xt <- t(X)
    XVi <- Xt %*% Vi
    Q <- solve(XVi %*% X)
    P <- Vi - (Vi %*% X %*% Q %*% XVi)

    ee <- eigen(V)
    log(sigma.u_log) - (areanumber / 2) * log(2 * pi) - 0.5 *
      sum(log(ee$value)) - (0.5) * log(det(t(X) %*% Vi %*% X)) -
      (0.5) * t(Y) %*% P %*% Y
  }


  ottimo <- optimize(AR, interval,
    maximum = TRUE,
    vardir = vardir, areanumber = areanumber,
    direct = direct, x = x
  )

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
#' @noRd

AMRL_YL <- function(interval, direct, x, vardir, areanumber) {
  AR_YL <- function(interval, direct, x, vardir, areanumber) {
    psi <- matrix(c(vardir), areanumber, 1)
    Y <- matrix(c(direct), areanumber, 1)
    X <- x
    Z.area <- diag(1, areanumber)
    sigma.u_log <- interval[1]
    I <- diag(1, areanumber)
    # V is the variance covariance matrix
    V <- sigma.u_log * Z.area %*% t(Z.area) + I * psi[, 1]
    Vi <- solve(V)
    Xt <- t(X)
    XVi <- Xt %*% Vi
    Q <- solve(XVi %*% X)
    P <- Vi - (Vi %*% X %*% Q %*% XVi)
    Bd <- diag(vardir / (sigma.u_log + vardir))

    ee <- eigen(V)
    atan(sum(diag((I - Bd))))^(1 / areanumber) * (2 * pi)^(-(areanumber / 2)) *
      exp(-0.5 * sum(log(ee$value))) *
      exp(-(0.5) * log(det(t(X) %*% Vi %*% X))) * exp(-(0.5) * t(Y) %*% P %*% Y)
  }

  ottimo <- optimize(AR_YL, interval,
    maximum = TRUE,
    vardir = vardir, areanumber = areanumber,
    direct = direct, x = x
  )

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
#' @noRd

AMPL <- function(interval, direct, x, vardir, areanumber) {
  AP <- function(interval, direct, x, vardir, areanumber) {
    psi <- matrix(c(vardir), areanumber, 1)
    Y <- matrix(c(direct), areanumber, 1)
    X <- x
    Z.area <- diag(1, areanumber)
    sigma.u_log <- interval[1]
    I <- diag(1, areanumber)
    # V is the variance covariance matrix
    V <- sigma.u_log * Z.area %*% t(Z.area) + I * psi[, 1]
    Vi <- solve(V)
    Xt <- t(X)
    XVi <- Xt %*% Vi
    Q <- solve(XVi %*% X)
    P <- Vi - (Vi %*% X %*% Q %*% XVi)

    ee <- eigen(V)
    log(sigma.u_log) - (areanumber / 2) * log(2 * pi) -
      0.5 * sum(log(ee$value)) - (0.5) * t(Y) %*% P %*% Y
  }

  ottimo <- optimize(AP, interval,
    maximum = TRUE,
    vardir = vardir, areanumber = areanumber,
    direct = direct, x = x
  )

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
#' @noRd

AMPL_YL <- function(interval, direct, x, vardir, areanumber) {
  AP_YL <- function(interval, direct, x, vardir, areanumber) {
    psi <- matrix(c(vardir), areanumber, 1)
    Y <- matrix(c(direct), areanumber, 1)
    X <- x
    Z.area <- diag(1, areanumber)
    sigma.u_log <- interval[1]
    I <- diag(1, areanumber)
    # V is the variance covariance matrix
    V <- sigma.u_log * Z.area %*% t(Z.area) + I * psi[, 1]
    Vi <- solve(V)
    Xt <- t(X)
    XVi <- Xt %*% Vi
    Q <- solve(XVi %*% X)
    P <- Vi - (Vi %*% X %*% Q %*% XVi)
    Bd <- diag(vardir / (sigma.u_log + vardir))

    ee <- eigen(V)
    (atan(sum(diag((I - Bd)))))^(1 / areanumber) *
      (2 * pi)^(-(areanumber / 2)) * exp(-0.5 * sum(log(ee$value))) *
      exp(-(0.5) * t(Y) %*% P %*% Y)
  }

  ottimo <- optimize(AP_YL, interval,
    maximum = TRUE,
    vardir = vardir, areanumber = areanumber,
    direct = direct, x = x
  )
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
#' @noRd

MPL <- function(interval, direct, x, vardir, areanumber) {
  ML <- function(interval, direct, x, vardir, areanumber) {
    psi <- matrix(c(vardir), areanumber, 1)
    Y <- matrix(c(direct), areanumber, 1)
    X <- x
    Z.area <- diag(1, areanumber)
    sigma.u_log <- interval[1]
    I <- diag(1, areanumber)
    # V is the variance covariance matrix
    V <- sigma.u_log * Z.area %*% t(Z.area) + I * psi[, 1]
    Vi <- solve(V)
    Xt <- t(X)
    XVi <- Xt %*% Vi
    Q <- solve(XVi %*% X)
    P <- Vi - (Vi %*% X %*% Q %*% XVi)

    ee <- eigen(V)
    -(areanumber / 2) * log(2 * pi) - 0.5 * sum(log(ee$value)) -
      (0.5) * t(Y) %*% P %*% Y
  }

  ottimo <- optimize(ML, interval,
    maximum = TRUE,
    vardir = vardir, areanumber = areanumber,
    direct = direct, x = x
  )

  estsigma2u <- ottimo$maximum

  return(sigmau_mpl = estsigma2u)
}

#' Function for estimating sigmau2 and rho for the spatial FH model (ML).
#'
#' @param direct direct estimator.
#' @param x matrix with explanatory variables.
#' @param vardir direct variance.
#' @param areanumber number of domains.
#' @param W proximity matrix.
#' @param maxit maximum number of iterations for the estimation of the variance.
#' @param tol tolerance value for the estimation of the variance.
#' @return estimated sigmau2 and beta coefficients and convergence (TRUE/FALSE).
#' @noRd

SML <- function(direct, X, vardir, areanumber, W, maxit, tol) {
  Xt <- t(X)
  tdirect <- t(direct)
  Wt <- t(W)
  D <- diag(1, areanumber)

  # Variance and spatial correlation vectors
  est.param <- matrix(0, 2, 1)
  final.param <- matrix(0, 2, 1)

  # Scores vector and Fisher information matrix
  score.vec <- matrix(0, 2, 1)
  fisher <- matrix(0, 2, 2)

  # Set initial values
  est.sigma2 <- 0
  est.rho <- 0
  est.sigma2[1] <- median(vardir)
  est.rho[1] <- 0.5

  # Fisher-scoring algorithm
  iter <- 0
  conv <- tol + 1
  while ((conv > tol) & (iter < maxit)) {
    iter <- iter + 1

    # Derivative of covariance matrix V: variance
    der.sigma <- solve((D - est.rho[iter] * Wt) %*% (D - est.rho[iter] * W))

    # Derivative of covariance matrix V: spatial correlation parameter
    der.rho <- 2 * est.rho[iter] * Wt %*% W - W - Wt
    der.vrho <- (-1) * est.sigma2[iter] * (der.sigma %*% der.rho %*% der.sigma)

    # Covariance matrix
    V <- est.sigma2[iter] * der.sigma + D * vardir
    # Inverse of covariance matrix
    Vi <- solve(V)

    # Computation of the regression coefficients
    XVi <- Xt %*% Vi
    Q <- solve(XVi %*% X)
    P <- Vi - (Vi %*% X %*% Q %*% XVi)

    # Scores vector
    P.der.sigma <- P %*% der.sigma
    P.der.rho <- P %*% der.vrho
    P.direct <- P %*% direct
    Vi.der.sigma <- Vi %*% der.sigma
    Vi.der.vrho <- Vi %*% der.vrho

    # Scores vector
    score.vec[1, 1] <- (-0.5) * sum(diag(Vi.der.sigma)) +
      (0.5) * (tdirect %*% P.der.sigma %*% P.direct)
    score.vec[2, 1] <- (-0.5) * sum(diag(Vi.der.vrho)) +
      (0.5) * (tdirect %*% P.der.rho %*% P.direct)

    # Fisher information matrix
    fisher[1, 1] <- (0.5) * sum(diag(Vi.der.sigma %*% Vi.der.sigma))
    fisher[1, 2] <- (0.5) * sum(diag(Vi.der.sigma %*% Vi.der.vrho))
    fisher[2, 1] <- (0.5) * sum(diag(Vi.der.vrho %*% Vi.der.sigma))
    fisher[2, 2] <- (0.5) * sum(diag(Vi.der.vrho %*% Vi.der.vrho))

    # Updating equations
    est.param[1, 1] <- est.sigma2[iter]
    est.param[2, 1] <- est.rho[iter]
    final.param <- est.param + solve(fisher) %*% score.vec

    # Restricting the spatial correlation to (-0.999,0.999)
    if (final.param[2, 1] <= -1) {
      final.param[2, 1] <- -0.999
    }
    if (final.param[2, 1] >= 1) {
      final.param[2, 1] <- 0.999
    }

    est.sigma2[iter + 1] <- final.param[1, 1]
    est.rho[iter + 1] <- final.param[2, 1]
    conv <- max(abs(final.param - est.param) / est.param)
  }

  # Final estimators
  if (est.rho[iter + 1] == -0.999) {
    est.rho[iter + 1] <- -1
  } else if (est.rho[iter + 1] == 0.999) {
    est.rho[iter + 1] <- 1
  }
  rho <- est.rho[iter + 1]

  est.sigma2[iter + 1] <- max(est.sigma2[iter + 1], 0)
  sigma2u <- est.sigma2[iter + 1]

  # Convergence

  if (iter >= maxit && conv >= tol) {
    convergence <- FALSE
  } else {
    convergence <- TRUE
  }

  return(list(sigmau2 = sigma2u, rho = rho, convergence = convergence))
}
#' Function for estimating sigmau2 and rho for the spatial FH model (REML).
#'
#' @param direct direct estimator.
#' @param x matrix with explanatory variables.
#' @param vardir direct variance.
#' @param areanumber number of domains.
#' @param W proximity matrix.
#' @param maxit maximum number of iterations for the estimation of the variance.
#' @param tol tolerance value for the estimation of the variance.
#' @return estimated sigmau2 and beta coefficients and convergence (TRUE/FALSE).
#' @noRd

SREML <- function(direct, X, vardir, areanumber, W, maxit, tol) {
  Xt <- t(X)
  tdirect <- t(direct)
  Wt <- t(W)
  D <- diag(1, areanumber)

  # Variance and spatial correlation vectors
  est.param <- matrix(0, 2, 1)
  final.param <- matrix(0, 2, 1)

  # Scores vector and Fisher information matrix
  score.vec <- matrix(0, 2, 1)
  fisher <- matrix(0, 2, 2)

  # Set initial values
  est.sigma2 <- 0
  est.rho <- 0
  est.sigma2[1] <- median(vardir)
  est.rho[1] <- 0.5

  # Fisher-scoring algorithm
  iter <- 0
  conv <- tol + 1
  while ((conv > tol) & (iter < maxit)) {
    iter <- iter + 1

    # Derivative of covariance matrix V: variance
    der.sigma <- solve((D - est.rho[iter] * Wt) %*% (D - est.rho[iter] * W))

    # Derivative of covariance matrix V: spatial correlation parameter
    der.rho <- 2 * est.rho[iter] * Wt %*% W - W - Wt
    der.vrho <- (-1) * est.sigma2[iter] * (der.sigma %*% der.rho %*% der.sigma)

    # Covariance matrix
    V <- est.sigma2[iter] * der.sigma + D * vardir
    # Inverse of covariance matrix
    Vi <- solve(V)

    # Computation of the regression coefficients
    XVi <- Xt %*% Vi # Inverse of X'ViX
    Q <- solve(XVi %*% X)
    P <- Vi - (Vi %*% X %*% Q %*% XVi)

    # Scores vector
    P.der.sigma <- P %*% der.sigma
    P.der.rho <- P %*% der.vrho
    P.direct <- P %*% direct
    score.vec[1, 1] <- (-0.5) * sum(diag(P.der.sigma)) +
      (0.5) * (tdirect %*% P.der.sigma %*% P.direct)
    score.vec[2, 1] <- (-0.5) * sum(diag(P.der.rho)) +
      (0.5) * (tdirect %*% P.der.rho %*% P.direct)

    # Fisher information matrix
    fisher[1, 1] <- (0.5) * sum(diag((P.der.sigma %*% P.der.sigma)))
    fisher[1, 2] <- (0.5) * sum(diag((P.der.sigma %*% P.der.rho)))
    fisher[2, 1] <- (0.5) * sum(diag((P.der.rho %*% P.der.sigma)))
    fisher[2, 2] <- (0.5) * sum(diag((P.der.rho %*% P.der.rho)))

    # Updating equations
    est.param[1, 1] <- est.sigma2[iter]
    est.param[2, 1] <- est.rho[iter]
    final.param <- est.param + solve(fisher) %*% score.vec

    # Restricting the spatial correlation to (-0.999,0.999)
    if (final.param[2, 1] <= -1) {
      final.param[2, 1] <- -0.999
    }
    if (final.param[2, 1] >= 1) {
      final.param[2, 1] <- 0.999
    }


    est.sigma2[iter + 1] <- final.param[1, 1]
    est.rho[iter + 1] <- final.param[2, 1]
    conv <- max(abs(final.param - est.param) / est.param)
  }

  # Final estimators
  if (est.rho[iter + 1] == -0.999) {
    est.rho[iter + 1] <- -1
  } else if (est.rho[iter + 1] == 0.999) {
    est.rho[iter + 1] <- 1
  }
  rho <- est.rho[iter + 1]

  est.sigma2[iter + 1] <- max(est.sigma2[iter + 1], 0)
  sigma2u <- est.sigma2[iter + 1]

  # Convergence

  if (iter >= maxit && conv >= tol) {
    convergence <- FALSE
  } else {
    convergence <- TRUE
  }

  return(list(sigmau2 = sigma2u, rho = rho, convergence = convergence))
}
#' Function for estimating sigmau2 and beta following Ybarra and Lohr.
#'
#' @param direct direct estimator.
#' @param x matrix with explanatory variables.
#' @param vardir direct variance.
#' @param Ci mean squared error of x.
#' @param areanumber number of domains.
#' @param p number of covariates.
#' @param tol tolerance value.
#' @param maxit maximum number of iterations.
#' @return estimated sigmau2 and estimated beta coefficients.
#' @noRd

ybarralohr <- function(direct, x, vardir, Ci, areanumber, p, tol, maxit) {

  # Paper pages 923-924
  wi <- rep(1, areanumber)
  conv <- 1
  iter <- 0
  x <- as.matrix(x)
  x <- t(x)
  rownames(x) <- NULL

  # Function for inverse square root matrix of G
  inverse.square.root.mat <- function(mat, maxit) {
    stopifnot(nrow(mat) == ncol(mat))
    niter <- 0
    G.inv <- diag(1, nrow(mat))
    for (niter in seq_len(maxit)) {
      mat.tmp <- 0.5 * (mat + ginv(G.inv))
      G.inv <- 0.5 * (G.inv + ginv(mat))
      mat <- mat.tmp
    }
    return(list(G = mat, G.inv = G.inv))
  }

  while (conv > tol & iter < maxit) {
    wi.tmp <- wi

    # wC
    wC <- matrix(0, p, p)
    for (i in seq_len(areanumber)) {
      wC <- wC + wi[i] * Ci[, , i]
    }

    # G
    G <- t(wi * t(x)) %*% t(x)

    # Inverse square root matrix of G
    Gi <- inverse.square.root.mat(G, maxit = maxit)$G.inv

    # GiwCGi
    GiwCGi <- Gi %*% wC %*% Gi

    # P
    decomp <- eigen(GiwCGi)
    P <- decomp$vectors

    # D
    Djj <- rep(0, p)
    cond <- which((1 - decomp$values) > 1 / areanumber)
    Djj[cond] <- 1 / (1 - decomp$values[cond])
    D <- diag(Djj)

    # Beta estimate
    Beta.hat <- Gi %*% P %*% D %*% t(P) %*% Gi %*%
      t(t(as.matrix(wi * direct, areanumber, 1)) %*% t(x))

    Beta.hat.tCiBeta.hat <- NULL
    for (i in seq_len(areanumber)) {
      Beta.hat.tCiBeta.hat[i] <- t(Beta.hat) %*% Ci[, , i] %*% Beta.hat
    }

    # Estimate variance of the random effect
    sigmau2 <- (1 / (areanumber - p)) *
      sum((direct - t(x) %*% Beta.hat)^2 - vardir - Beta.hat.tCiBeta.hat)

    # Truncation to zero, if variance estimate is negative
    if (sigmau2 <= 0) {
      sigmau2 <- 0
    }

    # Weight (wi) estimate
    wi <- 1 / (sigmau2 + vardir + Beta.hat.tCiBeta.hat)

    # Convergence
    conv <- mean(abs(wi - wi.tmp))
    iter <- iter + 1
  }

  estsigma2u <- sigmau2

  return(list(
    sigmau_YL = estsigma2u, betahatw = Beta.hat,
    Beta.hat.tCiBeta.hat = Beta.hat.tCiBeta.hat
  ))
}




#' Wrapper function for the estmation of sigmau2
#'
#' This function wraps the different estimation methods for sigmau2.
#'
#' @param vardir direct variance.
#' @param tol precision criteria for the estimation of sigmau2.
#' @param maxit maximum of iterations for the estimation of sigmau2.
#' @param interval interval for the algorithm.
#' @param direct direct estimator.
#' @param x matrix with explanatory variables.
#' @param areanumber number of domains.
#' @return estimated sigmau2.
#' @noRd

wrapper_estsigmau2 <- function(framework, method, interval) {
  sigmau2 <- if (method == "reml" && framework$correlation == "no") {
    Reml(
      interval = interval, vardir = framework$vardir, x = framework$model_X,
      direct = framework$direct, areanumber = framework$m
    )
  } else if (method == "amrl") {
    AMRL(
      interval = interval, vardir = framework$vardir, x = framework$model_X,
      direct = framework$direct, areanumber = framework$m
    )
  } else if (method == "amrl_yl") {
    AMRL_YL(
      interval = interval, vardir = framework$vardir,
      x = framework$model_X, direct = framework$direct,
      areanumber = framework$m
    )
  } else if (method == "ampl") {
    AMPL(
      interval = interval, vardir = framework$vardir,
      x = framework$model_X, direct = framework$direct,
      areanumber = framework$m
    )
  } else if (method == "ampl_yl") {
    AMPL_YL(
      interval = interval, vardir = framework$vardir,
      x = framework$model_X, direct = framework$direct,
      areanumber = framework$m
    )
  } else if (method == "ml" && framework$correlation == "no") {
    MPL(
      interval = interval, vardir = framework$vardir,
      x = framework$model_X, direct = framework$direct,
      areanumber = framework$m
    )
  } else if (method == "ml" && framework$correlation == "spatial") {
    SML(
      direct = framework$direct, X = framework$model_X,
      vardir = framework$vardir, areanumber = framework$m, W = framework$W,
      tol = framework$tol, maxit = framework$maxit
    )
  } else if (method == "reml" && framework$correlation == "spatial") {
    SREML(
      direct = framework$direct, X = framework$model_X,
      vardir = framework$vardir, areanumber = framework$m, W = framework$W,
      tol = framework$tol, maxit = framework$maxit
    )
  } else if (method == "me") {
    ybarralohr(
      vardir = framework$vardir, direct = framework$direct,
      x = framework$model_X, Ci = framework$Ci, tol = framework$tol,
      maxit = framework$maxit, p = framework$p,
      areanumber = framework$m
    )
  }

  return(sigmau2)
}
