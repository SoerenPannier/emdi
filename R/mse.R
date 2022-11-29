prasad_rao <- function(framework, sigmau2, combined_data) {
  g1 <- rep(0, framework$m)
  g2 <- rep(0, framework$m)
  g3 <- rep(0, framework$m)
  mse <- rep(0, framework$m)
  # Inverse of total variance
  Vi <- 1 / (sigmau2 + framework$vardir)
  # Shrinkage factor
  Bd <- framework$vardir / (sigmau2 + framework$vardir)
  # Squared inverse of total variance
  SumAD2 <- sum(Vi^2)
  # X'Vi
  XtVi <- t(Vi * framework$model_X)
  # (X'ViX)^-1
  Q <- solve(XtVi %*% framework$model_X)

  # 2 divided by squared inverse of total variance
  VarA <- 2 / SumAD2

  for (d in seq_len(framework$m)) {
    # Variance due to random effects: vardir * gamma
    g1[d] <- framework$vardir[d] * (1 - Bd[d])
    # Covariate for single domain
    xd <- matrix(framework$model_X[d, ], nrow = 1, ncol = framework$p)
    # Variance due to the estimation of beta
    g2[d] <- (Bd[d]^2) * xd %*% Q %*% t(xd)
    # Variance due to the estimation of the variance of the random effects
    g3[d] <- (Bd[d]^2) * VarA / (sigmau2 + framework$vardir[d])
    # Prasad-Rao estimator
    mse[d] <- g1[d] + g2[d] + 2 * g3[d]
  }

  mse_data <- data.frame(Domain = framework$combined_data[[framework$domains]])
  mse_data$Direct <- NA
  mse_data$Direct[framework$obs_dom == TRUE] <- framework$vardir

  # Small area MSE
  mse_data$FH[framework$obs_dom == TRUE] <- mse
  mse_data$Out[framework$obs_dom == TRUE] <- 0


  if (!all(framework$obs_dom == TRUE)) {
    h <- rep(0, framework$M - framework$m)
    mse_out <- rep(0, framework$M - framework$m)
    # Covariates for out-of-sample domains
    pred_data_tmp <- combined_data[framework$obs_dom == FALSE, ]
    pred_data_tmp <- data.frame(pred_data_tmp, helper = rnorm(1, 0, 1))
    formula.tools::lhs(framework$formula) <- quote(helper)
    pred_data <- makeXY(formula = framework$formula, data = pred_data_tmp)
    pred_X <- pred_data$x

    for (d_out in seq_len((framework$M - framework$m))) {
      xd_out <- matrix(pred_X[d_out, ], nrow = 1, ncol = framework$p)
      h[d_out] <- xd_out %*% Q %*% t(xd_out)
      mse_out[d_out] <- sigmau2 + h[d_out]
    }

    mse_data$FH[framework$obs_dom == FALSE] <- mse_out
    mse_data$Out[framework$obs_dom == FALSE] <- 1
  }


  return(mse_data)
}


datta_lahiri <- function(framework, sigmau2, combined_data) {
  g1 <- rep(0, framework$m)
  g2 <- rep(0, framework$m)
  g3 <- rep(0, framework$m)
  mse <- rep(0, framework$m)
  Vi <- 1 / (sigmau2 + framework$vardir)
  Bd <- framework$vardir / (sigmau2 + framework$vardir)
  SumAD2 <- sum(Vi^2)
  XtVi <- t(Vi * framework$model_X)
  Q <- solve(XtVi %*% framework$model_X)

  VarA <- 2 / SumAD2
  b <- (-1) * sum(diag(Q %*% (t((Vi^2) * framework$model_X) %*%
    framework$model_X))) / SumAD2
  for (d in seq_len(framework$m)) {
    g1[d] <- framework$vardir[d] * (1 - Bd[d])
    xd <- matrix(framework$model_X[d, ], nrow = 1, ncol = framework$p)
    g2[d] <- (Bd[d]^2) * xd %*% Q %*% t(xd)
    g3[d] <- (Bd[d]^2) * VarA / (sigmau2 + framework$vardir[d])
    mse[d] <- g1[d] + g2[d] + 2 * g3[d] - b * (Bd[d]^2)
  }

  mse_data <- data.frame(Domain = framework$combined_data[[framework$domains]])
  mse_data$Direct <- NA
  mse_data$Direct[framework$obs_dom == TRUE] <- framework$vardir

  # Small area MSE
  mse_data$FH[framework$obs_dom == TRUE] <- mse
  mse_data$Out[framework$obs_dom == TRUE] <- 0

  if (!all(framework$obs_dom == TRUE)) {
    h <- rep(0, framework$M - framework$m)
    mse_out <- rep(0, framework$M - framework$m)
    # Covariates for out-of-sample domains
    pred_data_tmp <- combined_data[framework$obs_dom == FALSE, ]
    pred_data_tmp <- data.frame(pred_data_tmp, helper = rnorm(1, 0, 1))
    formula.tools::lhs(framework$formula) <- quote(helper)
    pred_data <- makeXY(formula = framework$formula, data = pred_data_tmp)
    pred_X <- pred_data$x

    for (d_out in seq_len((framework$M - framework$m))) {
      xd_out <- matrix(pred_X[d_out, ], nrow = 1, ncol = framework$p)
      h[d_out] <- xd_out %*% Q %*% t(xd_out)
      mse_out[d_out] <- sigmau2 + b + h[d_out]
    }

    mse_data$FH[framework$obs_dom == FALSE] <- mse_out
    mse_data$Out[framework$obs_dom == FALSE] <- 1
  }

  return(mse_data)
}


li_lahiri <- function(framework, sigmau2, combined_data, method) {
  prasad_rao <- prasad_rao(
    framework = framework, sigmau2 = sigmau2,
    combined_data = combined_data
  )
  mse <- prasad_rao$FH[framework$obs_dom == TRUE]
  X <- framework$model_X
  psi <- matrix(c(framework$vardir), framework$m, 1)
  Y <- matrix(c(framework$direct), framework$m, 1)
  Z.area <- diag(1, framework$m)
  I <- diag(1, framework$m)
  # Shrinkage factor
  Bd <- framework$vardir / (sigmau2 + framework$vardir)
  # V is the variance covariance matrix
  V <- sigmau2 * Z.area %*% t(Z.area) + I * psi[, 1]
  Vi <- solve(V)
  Xt <- t(X)
  XVi <- Xt %*% Vi
  Q <- solve(XVi %*% X)
  P <- Vi - (Vi %*% X %*% Q %*% XVi)

  if (method == "ampl") {
    Bias <- (sum(diag(P - Vi)) + (2 / sigmau2)) / sum(diag(Vi^2))
    for (d in seq_len(framework$m)) {
      # Adjusted mse
      mse[d] <- mse[d] - (Bd[d]^2) * Bias
    }
  } else if (method == "amrl") {
    Bias <- (2 / sigmau2) / sum(diag(Vi^2))
    for (d in seq_len(framework$m)) {
      # Adjusted mse
      mse[d] <- mse[d] - (Bd[d]^2) * Bias
    }
  }

  mse_data <- prasad_rao
  mse_data$FH[framework$obs_dom == TRUE] <- mse
  mse_data$Out[framework$obs_dom == TRUE] <- 0


  if (!all(framework$obs_dom == TRUE)) {
    mse_data$FH[framework$obs_dom == FALSE] <- NA
    mse_data$Out[framework$obs_dom == FALSE] <- 1

    message(strwrap(prefix = " ", initial = "",
                    "Please note that only for in-sample-domains a correction
                    following Li and Lahiri (2010) is implemented. For the
                    out-of-sample domains, no estimate for the MSE is returned.
                    For the reference see help(fh)."))
  }

  return(mse_data)
}


yoshimori_lahiri <- function(framework, sigmau2, combined_data, method) {
  prasad_rao <- prasad_rao(
    framework = framework, sigmau2 = sigmau2,
    combined_data = combined_data
  )
  mse <- prasad_rao$FH[framework$obs_dom == TRUE]
  X <- framework$model_X
  psi <- matrix(c(framework$vardir), framework$m, 1)
  Y <- matrix(c(framework$direct), framework$m, 1)
  Z.area <- diag(1, framework$m)
  I <- diag(1, framework$m)
  # Shrinkage factor
  Bd <- framework$vardir / (sigmau2 + framework$vardir)
  # V is the variance covariance matrix
  V <- sigmau2 * Z.area %*% t(Z.area) + I * psi[, 1]
  Vi <- solve(V)
  Xt <- t(X)
  XVi <- Xt %*% Vi
  Q <- solve(XVi %*% X)
  P <- Vi - (Vi %*% X %*% Q %*% XVi)

  if (method == "ampl_yl") {
    Bias <- (sum(diag(P - Vi))) / sum(diag(Vi^2))
    for (d in seq_len(framework$m)) {
      # Adjusted mse
      mse[d] <- mse[d] - (Bd[d]^2) * Bias
    }
  } else if (method == "amrl_yl") {
    mse <- mse
  }

  mse_data <- prasad_rao
  mse_data$FH[framework$obs_dom == TRUE] <- mse
  mse_data$Out[framework$obs_dom == TRUE] <- 0


  if (!all(framework$obs_dom == TRUE)) {
    mse_data$FH[framework$obs_dom == FALSE] <- NA
    mse_data$Out[framework$obs_dom == FALSE] <- 1

    message(strwrap(prefix = " ", initial = "",
                    "Please note that only for in-sample-domains a correction
                    following Yoshimori and Lahiri (2014) is implemented. For
                    the out-of-sample domains, no estimate for the MSE is
                    returned. For the reference see help(fh)."))
  }

  return(mse_data)
}

prasad_rao_spatial <- function(framework, sigmau2, combined_data, method) {

  # MSE components
  g1 <- rep(0, framework$m)
  g2 <- rep(0, framework$m)
  g3 <- rep(0, framework$m)
  g4 <- rep(0, framework$m)
  mse.help <- rep(0, framework$m)
  mse <- rep(0, framework$m)

  D <- diag(1, framework$m)
  Xt <- t(framework$model_X)
  Wt <- t(framework$W)
  DrhoWDrhoWt <- solve((D - sigmau2$rho * Wt) %*%
    (D - sigmau2$rho * framework$W))
  var.DrhoW <- sigmau2$sigmau2 * DrhoWDrhoWt
  # Total variance
  V.rho <- var.DrhoW + D * framework$vardir
  # Inverse of total variance
  V.rhoi <- solve(V.rho)
  # Inverse of X'V.rhoiX
  XtV.rhoi <- Xt %*% V.rhoi
  Q.rho <- solve(t(framework$model_X) %*% V.rhoi %*% framework$model_X)

  G1 <- var.DrhoW - var.DrhoW %*% V.rhoi %*% var.DrhoW
  G2 <- var.DrhoW %*% V.rhoi %*% framework$model_X
  Xcoef <- matrix(0, 1, framework$p)

  # Computation of g1 and g2
  for (d in seq_len(framework$m)) {
    g1[d] <- G1[d, d]
    Xcoef[1, ] <- framework$model_X[d, ] - G2[d, ]
    g2[d] <- Xcoef %*% Q.rho %*% t(Xcoef)
  }

  # Derivative of covariance matrix V: spatial correlation parameter
  der.rho <- 2 * sigmau2$rho * Wt %*% framework$W - framework$W - Wt
  DrhoWDrhoWtmat <- (-1) * sigmau2$sigmau2 *
    (DrhoWDrhoWt %*% der.rho %*% DrhoWDrhoWt)
  P <- V.rhoi - t(XtV.rhoi) %*% Q.rho %*% XtV.rhoi
  PDrhoWDrhoWt <- P %*% DrhoWDrhoWt
  PDrhoWDrhoWtmat <- P %*% DrhoWDrhoWtmat

  # Fisher information matrix
  fisher <- matrix(0, 2, 2)
  fisher[1, 1] <- (0.5) * sum(diag((PDrhoWDrhoWt %*% PDrhoWDrhoWt)))
  fisher[1, 2] <- (0.5) * sum(diag((PDrhoWDrhoWt %*% PDrhoWDrhoWtmat)))
  fisher[2, 1] <- fisher[1, 2]
  fisher[2, 2] <- (0.5) * sum(diag((PDrhoWDrhoWtmat %*% PDrhoWDrhoWtmat)))
  fisheri <- solve(fisher)

  V.rhoiDrhoWDrhoWt <- V.rhoi %*% DrhoWDrhoWt
  V.rhoiDrhoWDrhoWtmat <- V.rhoi %*% DrhoWDrhoWtmat

  # Computation of g3
  line1 <- V.rhoiDrhoWDrhoWt -
    sigmau2$sigmau2 * V.rhoiDrhoWDrhoWt %*% V.rhoiDrhoWDrhoWt
  line1t <- t(line1)
  line2 <- V.rhoiDrhoWDrhoWtmat -
    sigmau2$sigmau2 * V.rhoiDrhoWDrhoWtmat %*% V.rhoiDrhoWDrhoWt
  line2t <- t(line2)

  lines <- matrix(0, 2, framework$m)
  for (d in seq_len(framework$m)) {
    lines[1, ] <- line1t[d, ]
    lines[2, ] <- line2t[d, ]
    g3[d] <- sum(diag(lines %*% V.rho %*% t(lines) %*% fisheri))
  }

  mse.help <- g1 + g2 + 2 * g3

  ######## Bias correction of Singh et al.
  psi <- diag(c(framework$vardir), framework$m)
  D1help <- (-1) * (DrhoWDrhoWt %*% der.rho %*% DrhoWDrhoWt)
  D2help <- 2 * sigmau2$sigmau2 * DrhoWDrhoWt %*% der.rho %*%
    DrhoWDrhoWt %*% der.rho %*% DrhoWDrhoWt -
    2 * sigmau2$sigmau2 * DrhoWDrhoWt %*% Wt %*% framework$W %*% DrhoWDrhoWt
  D <- (psi %*% V.rhoi %*% D1help %*% V.rhoi %*% psi) *
    (fisheri[1, 2] + fisheri[2, 1]) +
    psi %*% V.rhoi %*% D2help %*% V.rhoi %*% psi * fisheri[2, 2]

  for (d in seq_len(framework$m)) {
    g4[d] <- (0.5) * D[d, d]
  }

  # Computation of estimated MSE of Singh et al.
  mse <- mse.help - g4

  if (method == "ml") {
    # Computation of bML
    Q.rhoXtV.rhoi <- Q.rho %*% XtV.rhoi
    V.rhoiX <- V.rhoi %*% framework$model_X
    h1 <- (-1) * sum(diag(Q.rhoXtV.rhoi %*% DrhoWDrhoWt %*% V.rhoiX))
    h2 <- (-1) * sum(diag(Q.rhoXtV.rhoi %*% DrhoWDrhoWtmat %*% V.rhoiX))
    h <- matrix(c(h1, h2), nrow = 2, ncol = 1)
    bML <- (fisheri %*% h) / 2
    tbML <- t(bML)

    # Gradient of g1d
    GV.rhoi <- var.DrhoW %*% V.rhoi
    GV.rhoiDrhoWDrhoWt <- GV.rhoi %*% DrhoWDrhoWt
    GV.rhoiDrhoWDrhoWtmat <- GV.rhoi %*% DrhoWDrhoWtmat
    V.rhoiDrhoWDrhoWt <- V.rhoi %*% DrhoWDrhoWt
    dg1_dDrhoWDrhoWt <- DrhoWDrhoWt - 2 * GV.rhoiDrhoWDrhoWt +
      sigmau2$sigmau2 * GV.rhoiDrhoWDrhoWt %*% V.rhoiDrhoWDrhoWt
    dg1_dp <- DrhoWDrhoWtmat - 2 * GV.rhoiDrhoWDrhoWtmat +
      sigmau2$sigmau2 * GV.rhoiDrhoWDrhoWtmat %*% V.rhoiDrhoWDrhoWt
    grad.g1d <- matrix(0, nrow = 2, ncol = 1)

    bMLgrad.g1 <- rep(0, framework$m)
    for (d in seq_len(framework$m)) {
      grad.g1d[1, 1] <- dg1_dDrhoWDrhoWt[d, d]
      grad.g1d[2, 1] <- dg1_dp[d, d]
      bMLgrad.g1[d] <- tbML %*% grad.g1d
    }
    mse <- mse - bMLgrad.g1
  }



  mse_data <- data.frame(Domain = framework$combined_data[[framework$domains]])
  mse_data$Direct <- NA
  mse_data$Direct[framework$obs_dom == TRUE] <- framework$vardir

  # Small area MSE
  mse_data$FH[framework$obs_dom == TRUE] <- mse
  mse_data$Out[framework$obs_dom == TRUE] <- 0

  if (!all(framework$obs_dom == TRUE)) {
    mse_data$FH[framework$obs_dom == FALSE] <- NA
    mse_data$Out[framework$obs_dom == FALSE] <- 1

    message(strwrap(prefix = " ", initial = "",
                    "Please note that only for in-sample-domains an analytical
                    MSE for the spatial FH model is implemented. For the
                    out-of-sample domains, no estimate for the MSE is returned.
                    For the reference, see help(fh)."))
  }
  return(mse_data)
}




slud_maiti <- function(framework, sigmau2, eblup, combined_data) {

  # MSE estimation
  nu <- framework$model_X %*% eblup$coefficients$coefficients
  gamma <- eblup$gamma
  tau <- sigmau2 + framework$vardir
  # Variance of beta
  X <- framework$model_X
  psi <- matrix(c(framework$vardir), framework$m, 1)
  Z.area <- diag(1, framework$m)
  I <- diag(1, framework$m)
  # V is the variance covariance matrix
  V <- sigmau2 * Z.area %*% t(Z.area) + I * psi[, 1]
  Vi <- solve(V)
  Xt <- t(X)
  XVi <- Xt %*% Vi
  Q <- solve(XVi %*% X)
  # Variance of sigmau2
  # Identity matrix mxm
  D <- diag(1, framework$m)
  Deriv1 <- solve((sigmau2 * D) + diag(c(framework$vardir), framework$m))
  # Inverse of fisher information matrix. That is var. sigma2u
  Var.sigma <- ((1 / 2) * sum(diag(Deriv1 %*% Deriv1)))^(-1)

  tmp <- NULL
  for (i in seq_len(framework$m)) {
    tmp[i] <- (t(framework$model_X)[, i] %*% Q %*%
      framework$model_X[i, ]) / (tau[i]^2)
  }

  mse <- NULL
  for (j in seq_len(framework$m)) {
    mse[j] <- exp(2 * (nu[j, 1] + sigmau2)) *
      (1 - exp(-gamma[j] * framework$vardir[j])) +
      ((framework$vardir[j]^2) / tau[j]^2) *
        exp(2 * nu[j, 1] + sigmau2 * (1 + gamma[j])) *
        (t(framework$model_X)[, j] %*% Q %*% framework$model_X[j, ]) +
      Var.sigma * ((framework$vardir[j]^2) / tau[j]^2) *
        exp(2 * nu[j, 1] + sigmau2 * (1 + gamma[j])) *
        ((1 / 4) * (1 + 3 * gamma[j])^2 + (1 / tau[j])) -
      exp(2 * (nu[j, 1] + sigmau2)) *
        (2 * (1 - exp(-gamma[j] * framework$vardir[j])) *
          (t(framework$model_X)[, j] %*% Q %*% framework$model_X[j, ]) -
          Var.sigma * (2 + (((framework$vardir[j]^2) / tau[j]^2) - 2) *
            exp(-gamma[j] * framework$vardir[j])) * sum(tmp) +
          Var.sigma * (2 + (((2 * (framework$vardir[j]^2)) / (tau[j]^2)) - 2) *
            exp(-gamma[j] * framework$vardir[j]) -
            ((framework$vardir[j]^2) / (tau[j]^3)) *
              exp(-gamma[j] * framework$vardir[j]) *
              (1 + ((framework$vardir[j]^2) / (2 * tau[j])))))
  }


  mse_data <- data.frame(Domain = framework$combined_data[[framework$domains]])
  mse_data$Direct <- NA
  mse_data$Direct[framework$obs_dom == TRUE] <- framework$vardir
  mse_data$FH[framework$obs_dom == TRUE] <- mse
  mse_data$Out[framework$obs_dom == TRUE] <- 0

  if (!all(framework$obs_dom == TRUE)) {
    mse_data$FH[framework$obs_dom == FALSE] <- NA
    mse_data$Out[framework$obs_dom == FALSE] <- 1

    message(strwrap(prefix = " ", initial = "",
                    "Please note that EBLUP and MSE results are only returned
                    for in-sample domains. For more information see help(fh)."))
  }

  return(mse_data)
}



analytical_mse <- function(framework, sigmau2, combined_data,
                           method) {
  if (framework$correlation == "spatial") {
    mse_data <- prasad_rao_spatial(
      framework = framework, sigmau2,
      combined_data, method
    )
    MSE_method <- "prasad-rao-Singh"
  } else if (method == "reml") {
    mse_data <- prasad_rao(
      framework = framework, sigmau2 = sigmau2,
      combined_data = combined_data
    )
    MSE_method <- "prasad-rao"
  } else if (method == "amrl" || method == "ampl") {
    mse_data <- li_lahiri(
      framework = framework, sigmau2, combined_data,
      method = method
    )
    MSE_method <- "li-lahiri"
  } else if (method == "ampl_yl") {
    mse_data <- yoshimori_lahiri(
      framework = framework, sigmau2,
      combined_data, method = method
    )
    MSE_method <- "yoshimori-lahiri"
  } else if (method == "amrl_yl") {
    mse_data <- yoshimori_lahiri(
      framework = framework, sigmau2,
      combined_data, method = method
    )
    MSE_method <- "prasad-rao"
  } else if (method == "ml") {
    mse_data <- datta_lahiri(framework = framework, sigmau2, combined_data)
    MSE_method <- "datta-lahiri"
  }

  mse_out <- list(
    mse_data = mse_data,
    MSE_method = MSE_method
  )

  return(mse_out)
}


boot_arcsin_2 <- function(sigmau2, vardir, combined_data, framework,
                          eblup, eblup_corr, B, method,
                          interval, backtransformation) {


  # Gonzales Bootstrap fuer arcsin
  M <- framework$M
  m <- framework$m
  vardir <- framework$vardir
  eff_smpsize <- framework$eff_smpsize
  x <- framework$model_X

  ### Bootstrap
  in_sample <- framework$obs_dom == TRUE
  out_sample <- framework$obs_dom == FALSE

  # Ergebnissmatrixen
  true_value_boot <- matrix(NA, ncol = B, nrow = M)
  est_value_boot <- matrix(NA, ncol = B, nrow = M)

  for (b in seq_len(B)) {
    v_boot <- rnorm(M, 0, sqrt(sigmau2))
    e_boot <- rnorm(m, 0, sqrt(vardir))

    # Get covariates for all domains
    pred_data_tmp <- framework$combined_data
    pred_data_tmp <- data.frame(pred_data_tmp, helper = rnorm(1, 0, 1))
    formula.tools::lhs(framework$formula) <- quote(helper)
    pred_data <- makeXY(formula = framework$formula, data = pred_data_tmp)

    pred_X <- pred_data$x

    Xbeta_boot <- pred_X %*% eblup$coefficients$coefficients

    ## True Value for bootstraps
    ## Truncation
    true_value_boot_trans <- Xbeta_boot + v_boot
    true_value_boot_trans[true_value_boot_trans < 0] <- 0
    true_value_boot_trans[true_value_boot_trans > (pi / 2)] <- (pi / 2)

    ## Back-transformation
    ## true values without correction
    true_value_boot[, b] <- (sin(true_value_boot_trans))^2

    ystar_trans <- Xbeta_boot[in_sample] + v_boot[in_sample] + e_boot

    ## Estimation of sigmau2_boot on transformed scale
    framework2 <- framework
    framework2$direct <- ystar_trans
    sigmau2_boot <- wrapper_estsigmau2(
      framework = framework2, method = method,
      interval = interval
    )

    ## Computation of the coefficients'estimator (Bstim)
    D <- diag(1, m)
    V <- sigmau2_boot * D %*% t(D) + diag(as.numeric(vardir))
    Vi <- solve(V)
    Q <- solve(t(x) %*% Vi %*% x)
    Beta.hat_boot <- Q %*% t(x) %*% Vi %*% ystar_trans

    ## Computation of the EBLUP
    res <- ystar_trans - c(x %*% Beta.hat_boot)
    Sigma.u <- sigmau2_boot * D
    u.hat <- Sigma.u %*% t(D) %*% Vi %*% res

    ## Estimated Small area mean on transformed scale for the out and in sample
    # values
    est_mean_boot_trans <- x %*% Beta.hat_boot + D %*% u.hat
    pred_out_boot_trans <- pred_X %*% Beta.hat_boot

    est_value_boot_trans <- rep(NA, M)
    est_value_boot_trans[in_sample] <- est_mean_boot_trans
    est_value_boot_trans[out_sample] <- pred_out_boot_trans[out_sample]

    gamma_trans <- as.numeric(vardir) / (sigmau2_boot + as.numeric(vardir))
    est_value_boot_trans_var <- sigmau2_boot * gamma_trans
    est_value_boot_trans_var_ <- rep(0, M)
    est_value_boot_trans_var_[in_sample] <- est_value_boot_trans_var

    # backtransformation
    if (backtransformation == "bc") {
      int_value <- NULL
      for (i in seq_len(M)) {
        if (in_sample[i] == T) {
          mu_dri <- est_value_boot_trans
          # Get value of first domain
          mu_dri <- mu_dri[i]

          Var_dri <- est_value_boot_trans_var_
          Var_dri <- as.numeric(Var_dri[i])

          integrand <- function(x, mean, sd) {
            sin(x)^2 * dnorm(x, mean = mu_dri, sd = sqrt(Var_dri))
          }

          upper_bound <- min(
            mean(framework$direct) + 10 * sd(framework$direct),
            mu_dri + 100 * sqrt(Var_dri)
          )
          lower_bound <- max(
            mean(framework$direct) - 10 * sd(framework$direct),
            mu_dri - 100 * sqrt(Var_dri)
          )

          int_value <- c(int_value, integrate(integrand,
            lower = 0, upper = pi / 2
          )$value)
        } else {
          int_value <- c(int_value, (sin(est_value_boot_trans[i]))^2)
        }
      }
    } else if (backtransformation == "naive") {
      int_value <- NULL
      for (i in seq_len(M)) {
        int_value <- c(int_value, (sin(est_value_boot_trans[i]))^2)
      }
    }

    est_value_boot[, b] <- int_value

    message("b =", b, "\n")
  } # End of bootstrap runs

  # KI
  Li <- rep(NA, M)
  Ui <- rep(NA, M)

  for (ii in seq_len(M)) {
    Li[ii] <- eblup_corr[ii] +
      quantile(est_value_boot[ii, ] - true_value_boot[ii, ], 0.025)
    Ui[ii] <- eblup_corr[ii] +
      quantile(est_value_boot[ii, ] - true_value_boot[ii, ], 0.975)
  }

  conf_int <- data.frame(Li = Li, Ui = Ui)

  Quality_MSE <- function(estimator, TrueVal, B) {
    RMSE <- rep(NA, dim(estimator)[1])
    for (ii in seq_len(dim(estimator)[1])) {
      RMSE[ii] <- (1 / B * sum(((estimator[ii, ] - TrueVal[ii, ]))^2))
    }
    RMSE
  }

  mse <- Quality_MSE(est_value_boot, true_value_boot, B)

  mse_data <- data.frame(Domain = framework$combined_data[[framework$domains]])
  mse_data$Direct <- NA
  mse_data$Direct[framework$obs_dom == TRUE] <- framework$vardir

  # Small area MSE
  mse_data$MSE <- mse
  mse_data$Out[framework$obs_dom == TRUE] <- 0
  mse_data$Out[framework$obs_dom == FALSE] <- 1

  return(list(conf_int, mse_data))
}

nonparametricboot_spatial <- function(sigmau2, combined_data, framework,
                                      vardir, eblup, B, transformation,
                                      method, mse_type) {

  # MSE components
  g1 <- rep(0, framework$m)
  g2 <- rep(0, framework$m)
  mse <- rep(0, framework$m)

  # True values
  BCoef.boot <- eblup$coefficients$coefficients
  rho.boot <- sigmau2$rho
  sigma2.boot <- sigmau2$sigmau2

  D <- diag(1, framework$m)
  Xt <- t(framework$model_X)
  Wt <- t(framework$W)
  W <- framework$W
  DrhoW <- D - rho.boot * W
  DrhoWt <- t(DrhoW)
  DrhoWDrhoWt <- solve(DrhoWt %*% DrhoW)
  var.DrhoW <- sigmau2$sigmau2 * DrhoWDrhoWt
  V.rho <- var.DrhoW + D * framework$vardir
  V.rhoi <- solve(V.rho)
  Q.rho <- solve(t(framework$model_X) %*% V.rhoi %*% framework$model_X)

  # Residual vectors
  res <- framework$direct - framework$model_X %*% BCoef.boot
  u.hat <- var.DrhoW %*% V.rhoi %*% res

  # Computation of covariance matrices of residual vectors
  VG <- V.rho - var.DrhoW
  P <- V.rhoi - V.rhoi %*% framework$model_X %*% Q.rho %*% Xt %*% V.rhoi
  Ve <- VG %*% P %*% VG
  Vu <- DrhoW %*% var.DrhoW %*% P %*% var.DrhoW %*% DrhoWt

  XtV.rhoi <- Xt %*% V.rhoi
  Q.rho <- solve(XtV.rhoi %*% framework$model_X)

  # Computation of g1 and g2

  G1 <- var.DrhoW - var.DrhoW %*% V.rhoi %*% var.DrhoW
  G2 <- var.DrhoW %*% t(XtV.rhoi)
  Xcoef <- matrix(0, 1, framework$p)

  for (d in seq_len(framework$m)) {
    g1[d] <- G1[d, d]
    Xcoef[1, ] <- framework$model_X[d, ] - G2[d, ]
    g2[d] <- Xcoef %*% Q.rho %*% t(Xcoef)
  }

  # Square roots of covariance matrices
  eigenVecVe0 <- eigen(Ve)$vectors
  eigenVecVe <- eigenVecVe0[, seq_len((framework$m - framework$p))]
  eigenValVe0 <- eigen(Ve)$values
  eigenValVe <- diag(sqrt(1 / eigenValVe0[seq_len((framework$m -
    framework$p))]))
  Vei <- eigenVecVe %*% eigenValVe %*% t(eigenVecVe)

  eigenVecVu0 <- eigen(Vu)$vectors
  eigenVecVu <- eigenVecVu0[, seq_len((framework$m - framework$p))]
  eigenValVu0 <- 1 / (eigen(Vu)$values)
  eigenValVu <- diag(sqrt(eigenValVu0[seq_len((framework$m - framework$p))]))
  Vui <- eigenVecVu %*% eigenValVu %*% t(eigenVecVu)

  # Standardize residual vectors
  est.u <- as.vector(Vui %*% ((DrhoW) %*% u.hat))
  est.e <- as.vector(Vei %*% (res - u.hat))
  sdu <- sqrt(sigma2.boot)

  std.u <- rep(0, framework$m)
  for (d in seq_len(framework$m)) {
    std.u[d] <- (sdu * (est.u[d] - mean(est.u))) /
      sqrt(mean((est.u - mean(est.u))^2))
  }

  std.e <- rep(0, framework$m)
  for (d in seq_len(framework$m)) {
    std.e[d] <- (est.e[d] - mean(est.e)) / sqrt(mean((est.e - mean(est.e))^2))
  }

  # Create results matrices
  bootstrapMSE <- matrix(0, framework$m, B)
  estTheta <- matrix(0, framework$m, B)
  trueTheta <- matrix(0, framework$m, B)
  difmse.npb <- matrix(0, framework$m, 1)
  g1.help <- matrix(0, framework$m, 1)
  g2.help <- matrix(0, framework$m, 1)
  difg1.npb <- matrix(0, framework$m, 1)
  difg2.npb <- matrix(0, framework$m, 1)
  difg3.npb <- matrix(0, framework$m, 1)
  difmse.npbBC <- matrix(0, framework$m, 1)
  # Successfull bootstraps
  notSuc <- matrix(0, B, 1)

  # Bootstrap algorithm
  for (b in seq_len(B)) {

    # Bootstrap data
    u.boot <- sample(std.u, framework$m, replace = TRUE)
    e.samp <- sample(std.e, framework$m, replace = TRUE)
    e.boot <- sqrt(framework$vardir) * e.samp
    v.boot <- solve(D - rho.boot * W) %*% u.boot


    data_tmp <- data.frame(
      Domain =
        framework$combined_data[[framework$domains]]
    )
    data_tmp$theta.boot[framework$obs_dom == TRUE] <-
      as.numeric(framework$model_X %*% BCoef.boot + v.boot)

    direct.boot <- as.numeric(data_tmp$theta.boot[framework$obs_dom == TRUE] +
      e.boot)
    combined_data$direct.boot[framework$obs_dom == TRUE] <- direct.boot
    combined_data$direct.boot <- as.numeric(combined_data$direct.boot)

    # Estimation procedure

    # Terms <- terms(framework$formula)
    formula.tmp <- update.formula(framework$formula, direct.boot ~ .)
    framework.boot <- framework_FH(
      combined_data = combined_data,
      fixed = formula.tmp,
      vardir = vardir,
      domains = framework$domains,
      transformation = transformation,
      eff_smpsize = framework$eff_smpsize,
      correlation = framework$correlation,
      corMatrix = framework$W,
      Ci = framework$Ci,
      tol = framework$tol,
      maxit = framework$maxit
    )

    sigmau2.boot <- wrapper_estsigmau2(
      framework = framework.boot,
      method = method
    )

    eblupSFH.boot <- eblup_SFH(
      framework = framework.boot,
      sigmau2 = sigmau2.boot,
      combined_data = combined_data
    )

    eblupSFH.boot$eblup_data$FH[eblupSFH.boot$eblup_data$Out == 1] <- NA
    # New sample if estimated parameters are not acceptable
    if (sigmau2.boot$convergence == FALSE || sigmau2.boot$sigmau2 < 0 ||
      sigmau2.boot$rho < (-1) || sigmau2.boot$rho > 1) {
      notSuc[b, ] <- 1
      next
    }

    message("b =", b, "\n")

    # Bootstrap values
    rho.tmp.boot <- sigmau2.boot$rho
    sigma2.tmp.boot <- sigmau2.boot$sigmau2
    thetaSFH.boot <- eblupSFH.boot$eblup_data$FH[framework$obs_dom == TRUE]

    # Computation of nonparametric bootstrap estimator of g3
    est.coef.sblup <- Q.rho %*% XtV.rhoi %*% direct.boot


    data_tmp$thetaSFH.sblup.boot[framework$obs_dom == TRUE] <-
      framework$model_X %*% est.coef.sblup + var.DrhoW %*% V.rhoi %*%
      (direct.boot - framework$model_X %*% est.coef.sblup)

    thetaSFH.boot.in <-
      eblupSFH.boot$eblup_data$FH[eblupSFH.boot$eblup_data$Out == 0]
    difg3.npb[, 1] <- difg3.npb[, 1] +
      (thetaSFH.boot.in -
        data_tmp$thetaSFH.sblup.boot[framework$obs_dom == TRUE])^2

    data_tmp <- data_tmp[complete.cases(data_tmp$theta.boot), ]
    # Naive nonparametric bootstrap MSE
    estTheta[, b] <- thetaSFH.boot # estimated values
    trueTheta[, b] <- data_tmp$theta.boot # true values
    bootstrapMSE[, b] <- (thetaSFH.boot - data_tmp$theta.boot)^2
    difmse.npb[, 1] <- difmse.npb[, 1] +
      (thetaSFH.boot - data_tmp$theta.boot)^2

    # Computation of g1 and g2
    A <- solve((D - rho.tmp.boot * Wt) %*% (D - rho.tmp.boot * W))
    var.DrhoW.boot <- sigma2.tmp.boot * A
    V.rho.boot <- var.DrhoW.boot + D * framework$vardir
    Vi.rho.boot <- solve(V.rho.boot)
    XtVi.rho.boot <- Xt %*% Vi.rho.boot
    Q.rho.boot <- solve(XtVi.rho.boot %*% framework$model_X)

    G1 <- var.DrhoW.boot - var.DrhoW.boot %*% Vi.rho.boot %*% var.DrhoW.boot
    G2 <- var.DrhoW.boot %*% Vi.rho.boot %*% framework$model_X
    Xcoef <- matrix(0, 1, framework$p)

    for (d in seq_len(framework$m)) {
      g1.help[d] <- G1[d, d]
      Xcoef[1, ] <- framework$model_X[d, ] - G2[d, ]
      g2.help[d] <- Xcoef %*% Q.rho.boot %*% t(Xcoef)
    }

    difg1.npb <- difg1.npb + g1.help
    difg2.npb <- difg2.npb + g2.help
  }
  # Number of successful bootstrap iterations
  NoSuc <- (B - sum(notSuc[, 1]))

  # Naive nonparametric bootstrap MSE estimator
  mse.npb <- difmse.npb[, 1] / NoSuc

  # Bias-corrected nonparametric bootstrap MSE estimator
  g3.npb <- difg3.npb / NoSuc
  g1.npb <- difg1.npb / NoSuc
  g2.npb <- difg2.npb / NoSuc
  mse.npbBC <- 2 * (g1 + g2) - difg1.npb[, 1] / NoSuc - difg2.npb[, 1] / NoSuc +
    difg3.npb[, 1] / NoSuc

  mse_data <- data.frame(Domain = framework$combined_data[[framework$domains]])
  mse_data$Direct <- NA
  mse_data$Direct[framework$obs_dom == TRUE] <- framework$vardir

  notSuc <- paste(NoSuc, "out of", B)

  # Small area MSE
  if (mse_type == "spatialnonparboot") {
    mse_data$FH <- NA
    mse_data$FH[framework$obs_dom == TRUE] <- mse.npb
    mse_data$Out[framework$obs_dom == TRUE] <- 0
    mse_data$Out[framework$obs_dom == FALSE] <- 1
    mse_data <- list(
      mse_data = mse_data,
      MSE_method = "naive non-parametric bootstrap",
      successful_bootstraps = notSuc
    )
    if (!all(framework$obs_dom == TRUE)) {
      message(strwrap(prefix = " ", initial = "",
                      "Please note that only for in-sample-domains the
                      non-parametric bootstrap MSE estimator for the spatial FH
                      model is implemented. For the out-of-sample domains, no
                      estimate for the MSE is returned. For the reference,
                      see help(fh)."))
    }
  }
  if (mse_type == "spatialnonparbootbc") {
    mse_data$FH <- NA
    mse_data$FH[framework$obs_dom == TRUE] <- mse.npbBC
    mse_data$Out[framework$obs_dom == TRUE] <- 0
    mse_data$Out[framework$obs_dom == FALSE] <- 1
    mse_data <- list(
      mse_data = mse_data,
      MSE_method = "bias corrected non-parametric bootstrap",
      successful_bootstraps = notSuc
    )
    if (!all(framework$obs_dom == TRUE)) {
      message(strwrap(prefix = " ", initial = "",
                      "Please note that only for in-sample-domains the bias
                      corrected non-parametric bootstrap MSE estimator for the
                      spatial FH model is implemented. For the out-of-sample
                      domains, no estimate for the MSE is returned. For the
                      reference, see help(fh)."))
    }
  }
  return(mse_data)
}

parametricboot_spatial <- function(sigmau2, combined_data, framework, vardir,
                                   eblup, B, transformation, method,
                                   mse_type) {

  # MSE components
  g1 <- rep(0, framework$m)
  g2 <- rep(0, framework$m)
  mse <- rep(0, framework$m)

  # True values
  BCoef.boot <- eblup$coefficients$coefficients
  rho.boot <- sigmau2$rho
  sigma2.boot <- sigmau2$sigmau2

  D <- diag(1, framework$m)
  Xt <- t(framework$model_X)
  Wt <- t(framework$W)
  W <- framework$W
  DrhoW <- D - rho.boot * W
  DrhoWt <- t(DrhoW)
  DrhoWDrhoWt <- solve(DrhoWt %*% DrhoW)
  var.DrhoW <- sigmau2$sigmau2 * DrhoWDrhoWt
  V.rho <- var.DrhoW + D * framework$vardir
  V.rhoi <- solve(V.rho)
  Q.rho <- solve(t(framework$model_X) %*% V.rhoi %*% framework$model_X)

  # Residual vectors
  res <- framework$direct - framework$model_X %*% BCoef.boot
  u.hat <- var.DrhoW %*% V.rhoi %*% res

  XtV.rhoi <- Xt %*% V.rhoi
  Q.rho <- solve(XtV.rhoi %*% framework$model_X)

  # Computation of g1 and g2

  G1 <- var.DrhoW - var.DrhoW %*% V.rhoi %*% var.DrhoW
  G2 <- var.DrhoW %*% t(XtV.rhoi)
  Xcoef <- matrix(0, 1, framework$p)

  for (d in seq_len(framework$m)) {
    g1[d] <- G1[d, d]
    Xcoef[1, ] <- framework$model_X[d, ] - G2[d, ]
    g2[d] <- Xcoef %*% Q.rho %*% t(Xcoef)
  }

  # Create result matrices

  difmse.pb <- matrix(0, framework$m, 1)
  g1.help <- matrix(0, framework$m, 1)
  g2.help <- matrix(0, framework$m, 1)
  difg1.pb <- matrix(0, framework$m, 1)
  difg2.pb <- matrix(0, framework$m, 1)
  difg3.pb <- matrix(0, framework$m, 1)
  difmse.pbBC <- matrix(0, framework$m, 1)
  # Successfull bootstraps
  notSuc <- matrix(0, B, 1)


  # Bootstrap algorithm
  for (b in seq_len(B)) {

    # Bootstrap data
    u.boot <- rnorm(framework$m, 0, sqrt(sigma2.boot))
    v.boot <- solve(D - rho.boot * W) %*% u.boot
    theta.boot <- framework$model_X %*% BCoef.boot + v.boot
    e.boot <- rnorm(framework$m, 0, sqrt(framework$vardir))
    direct.boot <- theta.boot + e.boot
    combined_data$direct.boot[framework$obs_dom == TRUE] <- direct.boot

    # Estimation procedure
    formula.tmp <- update.formula(framework$formula, direct.boot ~ .)
    framework.boot <- framework_FH(
      combined_data = combined_data,
      fixed = formula.tmp,
      vardir = vardir,
      domains = framework$domains,
      transformation = transformation,
      eff_smpsize = framework$eff_smpsize,
      correlation = framework$correlation,
      corMatrix = framework$W,
      Ci = framework$Ci,
      tol = framework$tol,
      maxit = framework$maxit
    )

    sigmau2.boot <- wrapper_estsigmau2(
      framework = framework.boot,
      method = method
    )

    eblupSFH.boot <- eblup_SFH(
      framework = framework.boot,
      sigmau2 = sigmau2.boot,
      combined_data = combined_data
    )
    eblupSFH.boot$eblup_data$FH[eblupSFH.boot$eblup_data$Out == 1] <- NA

    # New sample if estimated parameters are not acceptable
    if (sigmau2.boot$convergence == FALSE || sigmau2.boot$sigmau2 < 0 |
      sigmau2.boot$rho < (-1) || sigmau2.boot$rho > 1) {
      notSuc[b, ] <- 1
      next
    }


    message("b =", b, "\n")

    rho.tmp.boot <- sigmau2.boot$rho
    sigma2.tmp.boot <- sigmau2.boot$sigmau2
    BCoef.tmp.boot <- eblupSFH.boot$coefficients$coefficients
    thetaSFH.boot <- eblupSFH.boot$eblup_data$FH[framework$obs_dom == TRUE]

    # Parametric bootstrap estimator of g3
    est.coef.sblup <- Q.rho %*% XtV.rhoi %*% direct.boot
    thetaSFH.sblup.boot <- framework$model_X %*% est.coef.sblup +
      var.DrhoW %*% V.rhoi %*% (direct.boot -
        framework$model_X %*% est.coef.sblup)

    difg3.pb[, 1] <- difg3.pb[, 1] + (thetaSFH.boot - thetaSFH.sblup.boot)^2

    # Naive parametric bootstrap MSE
    difmse.pb[, 1] <- difmse.pb[, 1] + (thetaSFH.boot - theta.boot)^2

    # Computation of g1 and g2
    A <- solve((D - rho.tmp.boot * Wt) %*% (D - rho.tmp.boot * W))
    var.DrhoW.boot <- sigma2.tmp.boot * A
    V.rho.boot <- var.DrhoW.boot + D * framework$vardir
    Vi.rho.boot <- solve(V.rho.boot)
    XtVi.rho.boot <- Xt %*% Vi.rho.boot
    Q.rho.boot <- solve(XtVi.rho.boot %*% framework$model_X)

    G1 <- var.DrhoW.boot - var.DrhoW.boot %*% Vi.rho.boot %*% var.DrhoW.boot
    G2 <- var.DrhoW.boot %*% Vi.rho.boot %*% framework$model_X
    Xcoef <- matrix(0, 1, framework$p)

    for (d in seq_len(framework$m)) {
      g1.help[d] <- G1[d, d]
      Xcoef[1, ] <- framework$model_X[d, ] - G2[d, ]
      g2.help[d] <- Xcoef %*% Q.rho.boot %*% t(Xcoef)
    }

    difg1.pb <- difg1.pb + g1.help
    difg2.pb <- difg2.pb + g2.help
  }

  # Number of successful bootstrap iterations
  NoSuc <- (B - sum(notSuc[, 1]))

  # Naive parametric bootstrap MSE estimator
  mse.pb <- difmse.pb[, 1] / NoSuc

  # Bias-corrected parametric bootstrap MSE estimator
  g1.pb <- difg1.pb / NoSuc
  g2.pb <- difg2.pb / NoSuc
  g3.pb <- difg3.pb / NoSuc

  mse.pbBC <- 2 * (g1 + g2) - difg1.pb[, 1] / NoSuc -
    difg2.pb[, 1] / NoSuc + difg3.pb[, 1] / NoSuc



  mse_data <- data.frame(Domain = framework$combined_data[[framework$domains]])
  mse_data$Direct <- NA
  mse_data$Direct[framework$obs_dom == TRUE] <- framework$vardir

  notSuc <- paste(NoSuc, "out of", B)

  # Small area MSE
  if (mse_type == "spatialparboot") {
    mse_data$FH <- NA
    mse_data$FH[framework$obs_dom == TRUE] <- mse.pb
    mse_data$Out[framework$obs_dom == TRUE] <- 0
    mse_data$Out[framework$obs_dom == FALSE] <- 1

    mse_data <- list(
      mse_data = mse_data,
      MSE_method = "naive parametric bootstrap",
      successful_bootstraps = notSuc
    )
    if (!all(framework$obs_dom == TRUE)) {
      message(strwrap(prefix = " ", initial = "",
                      "Please note that only for in-sample-domains the naive
                      parametric bootstrap MSE estimator for the spatial FH
                      model is implemented. For the out-of-sample domains, no
                      estimate for the MSE is returned. For the reference,
                      see help(fh)."))
    }
  }
  if (mse_type == "spatialparbootbc") {
    mse_data$FH <- NA
    mse_data$FH[framework$obs_dom == TRUE] <- mse.pbBC
    mse_data$Out[framework$obs_dom == TRUE] <- 0
    mse_data$Out[framework$obs_dom == FALSE] <- 1

    mse_data <- list(
      mse_data = mse_data,
      MSE_method = "bias corrected parametric bootstrap",
      successful_bootstraps = notSuc
    )
    if (!all(framework$obs_dom == TRUE)) {
      message(strwrap(prefix = " ", initial = "",
                      "Please note that only for in-sample-domains the bias
                      corrected parametric bootstrap MSE estimator for the
                      spatial FH model is implemented. For the out-of-sample
                      domains, no estimate for the MSE is returned. For the
                      reference, see help(fh)."))
    }
  }
  return(mse_data)
}

jiang_jackknife <- function(framework, combined_data, sigmau2, eblup,
                            transformation, vardir, method, interval) {


  # this MSE estimator can leed to negative values
  m <- framework$m
  jack_sigmau2 <- vector(length = m)
  diff_jack_eblups <- data.frame(row.names = seq_len(m))
  diff_jack_g1 <- data.frame(row.names = seq_len(m))

  g1 <- rep(0, framework$m)
  jack_mse <- rep(0, framework$m)
  # Inverse of total variance
  Vi <- 1 / (sigmau2 + framework$vardir)
  # Shrinkage factor
  Bd <- framework$vardir / (sigmau2 + framework$vardir)


  for (d in seq_len(framework$m)) {
    # Variance due to random effects: vardir * gamma
    g1[d] <- framework$vardir[d] * (1 - Bd[d])
  }

  for (domain in seq_len(m)) {
    message("domain =", domain, "\n")

    data_insample <- combined_data[framework$obs_dom, ]
    data_tmp <- data_insample[-domain, ]

    # Framework with temporary data
    framework_tmp <- framework_FH(
      combined_data = data_tmp,
      fixed = framework$formula,
      vardir = vardir, domains = framework$domains,
      transformation = transformation,
      correlation = framework$correlation,
      corMatrix = framework$corMatrix,
      eff_smpsize = framework$eff_smpsize,
      Ci = NULL, tol = NULL, maxit = NULL
    )
    # Estimate sigma u
    sigmau2_tmp <- wrapper_estsigmau2(
      framework = framework_tmp,
      method = method, interval = interval
    )
    jack_sigmau2[domain] <- sigmau2_tmp

    Vi_tmp <- 1 / (sigmau2_tmp + framework$vardir)
    # Shrinkage factor
    Bd_tmp <- framework$vardir / (sigmau2_tmp + framework$vardir)

    g1_tmp <- rep(0, framework$m)
    for (d_tmp in seq_len(framework$m)) {
      g1_tmp[d_tmp] <- framework$vardir[d_tmp] * (1 - Bd_tmp[d_tmp])
    }

    # G1
    diff_jack_g1[, paste0(domain)] <- g1_tmp - g1

    # Standard EBLUP
    framework_insample <- framework_FH(
      combined_data = data_insample,
      fixed = framework$formula,
      vardir = vardir,
      domains = framework$domains,
      transformation = transformation,
      correlation = framework$correlation,
      corMatrix = framework$corMatrix,
      eff_smpsize = framework$eff_smpsize,
      Ci = NULL, tol = NULL, maxit = NULL
    )
    eblup_tmp <- eblup_FH(
      framework = framework_insample, sigmau2 = sigmau2_tmp,
      combined_data = data_insample
    )
    diff_jack_eblups[, paste0(domain)] <- eblup_tmp$eblup_data$FH -
      eblup$eblup_data$FH[eblup$eblup_data$Out == 0]
  }

  jack_mse <- g1 - ((m - 1) / m) * rowSums(diff_jack_g1) +
    ((m - 1) / m) * rowSums(diff_jack_eblups^2)


  mse_data <- data.frame(Domain = framework$combined_data[[framework$domains]])
  mse_data$Direct <- NA
  mse_data$Direct[framework$obs_dom == TRUE] <- framework$vardir

  # Jackknife MSE
  mse_data$FH[framework$obs_dom == TRUE] <- jack_mse
  mse_data$Out[framework$obs_dom == TRUE] <- 0


  if (!all(framework$obs_dom == TRUE)) {
    mse_data$FH[framework$obs_dom == FALSE] <- NA
    mse_data$Out[framework$obs_dom == FALSE] <- 1

    message(strwrap(prefix = " ", initial = "",
                    "Please note that the jackknife MSE is only available for
                    in-sample domains."))
  }

  mse_out <- list(
    mse_data = mse_data,
    MSE_method = "jackknife"
  )

  return(mse_out)
}

chen_weighted_jackknife <- function(framework, combined_data, sigmau2, eblup,
                                    transformation, vardir, method, interval) {

  # implementiert nach:
  # Chen S., Lahiri P. (2002), A Weighted Jackknife MSPE Estimator in Small-Area Estimation,
  # "Proceeding of the Section on Survey Research Methods", American Statistical Association,
  # pp. 473-477.

  # hier wurden erstmal die weights aus d-1/d festgelegt, es gibt auch andere Optionen

  m <- framework$m
  jack_sigmau2 <- vector(length = m)
  diff_jack_eblups <- data.frame(row.names = seq_len(m))
  diff_jack_g1 <- data.frame(row.names = seq_len(m))
  diff_jack_g2 <- data.frame(row.names = seq_len(m))

  g1 <- rep(0, framework$m)
  jack_mse <- rep(0, framework$m)
  jack_mse_weighted <- rep(0, framework$m)
  # Inverse of total variance
  Vi <- 1 / (sigmau2 + framework$vardir)
  # Shrinkage factor
  Bd <- framework$vardir / (sigmau2 + framework$vardir)


  for (d in seq_len(framework$m)) {
    # Variance due to random effects: vardir * gamma
    g1[d] <- framework$vardir[d] * (1 - Bd[d])
  }


  Nenner <- rep(0, framework$m)
  for (d in seq_len(framework$m)) {
    Nenner[d] <- (framework$model_X[d, ] %*% framework$model_X[d, ]) /
      (framework$vardir[d] + sigmau2)
  }


  g2 <- rep(0, framework$m)
  for (d in seq_len(framework$m)) {
    # Variance due to beta estimation
    g2[d] <- (Bd[d])^2 * framework$model_X[d, ] %*%
      framework$model_X[d, ] * (sum(Nenner))^(-1)
  }

  for (domain in seq_len(m)) {
    message("domain =", domain, "\n")


    data_insample <- combined_data[framework$obs_dom, ]
    data_tmp <- data_insample[-domain, ]

    # Framework with temporary data
    framework_tmp <- framework_FH(
      combined_data = data_tmp,
      fixed = framework$formula,
      vardir = vardir, domains = framework$domains,
      transformation = transformation,
      correlation = framework$correlation,
      corMatrix = framework$corMatrix,
      eff_smpsize = framework$eff_smpsize,
      Ci = NULL, tol = NULL, maxit = NULL
    )
    # Estimate sigma u
    sigmau2_tmp <- wrapper_estsigmau2(
      framework = framework_tmp,
      method = method, interval = interval
    )
    jack_sigmau2[domain] <- sigmau2_tmp

    Vi_tmp <- 1 / (sigmau2_tmp + framework$vardir)
    # Shrinkage factor
    Bd_tmp <- framework$vardir / (sigmau2_tmp + framework$vardir)

    g1_tmp <- rep(0, framework$m)
    for (d_tmp in seq_len(framework$m)) {
      g1_tmp[d_tmp] <- framework$vardir[d_tmp] * (1 - Bd_tmp[d_tmp])
    }

    Nenner_tmp <- rep(0, framework$m)
    for (d_tmp in seq_len(framework$m)) {
      Nenner_tmp[d_tmp] <- (framework$model_X[d_tmp, ] %*%
        framework$model_X[d_tmp, ]) /
        (framework$vardir[d_tmp] + sigmau2_tmp)
    }

    g2_tmp <- rep(0, framework$m)
    for (d_tmp in seq_len(framework$m)) {
      g2_tmp[d_tmp] <- (Bd_tmp[d_tmp])^2 *
        framework$model_X[d_tmp, ] %*% framework$model_X[d_tmp, ] *
        (sum(Nenner_tmp))^(-1)
    }

    # G1
    diff_jack_g1[, paste0(domain)] <- g1_tmp - g1
    # negative Werte koennen erhalten werden, wenn diff_jack_g1 sehr gross ist

    # G1
    diff_jack_g2[, paste0(domain)] <- g1_tmp + g2_tmp - (g1 + g2)

    # Standard EBLUP
    framework_insample <- framework_FH(
      combined_data = data_insample,
      fixed = framework$formula,
      vardir = vardir,
      domains = framework$domains,
      transformation = transformation,
      eff_smpsize = framework$eff_smpsize,
      correlation = framework$correlation,
      corMatrix = framework$corMatrix,
      Ci = NULL, tol = NULL, maxit = NULL
    )
    eblup_tmp <- eblup_FH(
      framework = framework_insample, sigmau2 = sigmau2_tmp,
      combined_data = data_insample
    )
    diff_jack_eblups[, paste0(domain)] <- eblup_tmp$eblup_data$FH -
      eblup$eblup_data$FH[eblup$eblup_data$Out == 0]
  }

  w_u <- c()
  v_wj <- c()

  for (i in seq_len(nrow(framework$model_X))) {
    w_u[i] <- 1 - t(framework$model_X[i, ]) %*%
      solve(t(framework$model_X) %*%
        framework$model_X) %*% framework$model_X[i, ]
    v_wj[i] <- w_u[i] * (jack_sigmau2[i] - sigmau2) *
      (jack_sigmau2[i] - sigmau2)
  }

  jack_mse <- g1 - ((m - 1) / m) * rowSums(diff_jack_g1) +
    ((m - 1) / m) * rowSums(diff_jack_eblups^2)
  jack_mse_weighted <- g1 + g2 - ((m - 1) / m) *
    rowSums(diff_jack_g2) + ((m - 1) / m) * rowSums(diff_jack_eblups^2)

  jack_mse <- g1 - w_u * rowSums(diff_jack_g1) + w_u *
    rowSums(diff_jack_eblups^2)
  jack_mse_weighted <- g1 + g2 - w_u * rowSums(diff_jack_g2) + w_u *
    rowSums(diff_jack_eblups^2)


  # bias correction for negative values:

  neg_values <- which(jack_mse_weighted < 0)

  jack_mse_weighted_neg <- c()

  if (length(neg_values) > 0) {
    Sig_d <- (sigmau2 + framework$vardir) * diag(m)
    Sig_d_inv <- solve(Sig_d)
    L_d <- framework$vardir / ((sigmau2 + framework$vardir)^2) * diag(m)
    b_wj <- w_u * (jack_sigmau2 - sigmau2)

    # bias for REML is NULL (only for REML feasible)
    app_bias_correction <-
      sum(b_wj) * (framework$vardir^2 / (framework$vardir + sigmau2)^2) -
      diag(L_d %*% Sig_d %*% t(L_d) * sum(v_wj))

    jack_mse_weighted_neg <- g1 + g2 + w_u *
      rowSums(diff_jack_eblups^2) - app_bias_correction
  }

  for (i in seq_len(m)) {
    if (i %in% neg_values) {
      jack_mse_weighted[i] <- jack_mse_weighted_neg[i]
    }
  }

  jack_mse_weighted

  mse_data <- data.frame(Domain = framework$combined_data[[framework$domains]])
  mse_data$Direct <- NA
  mse_data$Direct[framework$obs_dom == TRUE] <- framework$vardir

  # Jackknife MSE
  mse_data$FH[framework$obs_dom == TRUE] <- jack_mse_weighted
  mse_data$Out[framework$obs_dom == TRUE] <- 0


  if (!all(framework$obs_dom == TRUE)) {
    mse_data$FH[framework$obs_dom == FALSE] <- NA
    mse_data$Out[framework$obs_dom == FALSE] <- 1

    message(strwrap(prefix = " ", initial = "",
                    "Please note that the jackknife MSE is only available for
                    in-sample domains."))
  }

  mse_out <- list(
    mse_data = mse_data,
    MSE_method = "weighted jackknife"
  )

  return(mse_out)
}

### Jackknife MSE estimator (Ybarra-Lohr model)

jiang_jackknife_yl <- function(framework, combined_data, sigmau2, eblup,
                               method, transformation, vardir, Ci) {


  # this MSE estimator can leed to negative values
  m <- framework$m
  jack_sigmau2 <- vector(length = m)
  diff_jack_eblups <- data.frame(row.names = seq_len(m))
  diff_jack_g1 <- data.frame(row.names = seq_len(m))
  g1 <- rep(0, framework$m)
  jack_mse <- rep(0, framework$m)
  gamma_tmp <- matrix(0, framework$m, framework$m)
  sigmau2_tmp <- matrix(0, framework$m)
  jack_sigmau2 <- matrix(0, framework$m)
  # Shrinkage factor
  Bd <- 1 - eblup$gamma

  g1_tmp <- matrix(0, framework$m, framework$m)
  for (d in seq_len(framework$m)) {
    # Variance due to random effects: vardir * gamma
    g1[d] <- framework$vardir[d] * (1 - Bd[d])
  }

  for (domain in seq_len(m)) {
    message("domain =", domain, "\n")


    data_insample <- framework$combined_data[framework$obs_dom, ]
    data_tmp <- data_insample[-domain, ]
    Ci_tmp <- Ci[, , -domain]

    # Framework with temporary data
    framework_tmp <- framework_FH(
      combined_data = data_tmp,
      fixed = framework$formula,
      vardir = vardir,
      domains = framework$domains,
      transformation = "no",
      eff_smpsize = framework$eff_smpsize,
      correlation = framework$correlation,
      corMatrix = framework$corMatrix,
      Ci = Ci_tmp,
      tol = framework$tol,
      maxit = framework$maxit
    )


    # Estimate sigma u
    sigmau2_tmp <- wrapper_estsigmau2(
      framework = framework_tmp,
      method = method
    )
    jack_sigmau2[domain] <- sigmau2_tmp$sigmau_YL

    framework_insample <- framework_FH(
      combined_data = data_insample,
      fixed = framework$formula,
      vardir = vardir,
      domains = framework$domains,
      transformation = "no",
      eff_smpsize = framework$eff_smpsize,
      correlation = framework$correlation,
      corMatrix = framework$corMatrix,
      Ci = Ci,
      tol = framework$tol,
      maxit = framework$maxit
    )


    Beta.hat.tCiBeta.hat <- NULL
    for (i in seq_len(framework_insample$m)) {
      Beta.hat.tCiBeta.hat[i] <-
        t(sigmau2_tmp$betahatw) %*%
        framework_insample$Ci[, , i] %*% sigmau2_tmp$betahatw
    }

    gamma_tmp[, domain] <- (sigmau2_tmp$sigmau_YL + Beta.hat.tCiBeta.hat) /
      (sigmau2_tmp$sigmau_YL + Beta.hat.tCiBeta.hat + framework_insample$vardir)

    for (d_tmp in seq_len(framework$m)) {
      g1_tmp[d_tmp, ] <- framework$vardir[d_tmp] * gamma_tmp[d_tmp, ]
    }


    # G1
    diff_jack_g1[, paste0(domain)] <- g1_tmp[, domain] - g1

    # Standard EBLUP

    eblup_tmp <- eblup_YL(
      framework = framework_insample, sigmau2 = sigmau2_tmp,
      combined_data = data_insample
    )

    diff_jack_eblups[, paste0(domain)] <- eblup_tmp$eblup_data$FH -
      eblup$eblup_data$FH[eblup$eblup_data$Out == 0]
  }

  jack_mse <- g1 - ((m - 1) / m) * rowSums(diff_jack_g1) +
    ((m - 1) / m) * rowSums(diff_jack_eblups^2)


  mse_data <- data.frame(Domain = framework$combined_data[[framework$domains]])
  mse_data$Direct <- NA
  mse_data$Direct[framework$obs_dom == TRUE] <- framework$vardir

  # Jackknife MSE
  mse_data$FH[framework$obs_dom == TRUE] <- jack_mse
  mse_data$Out[framework$obs_dom == TRUE] <- 0


  if (!all(framework$obs_dom == TRUE)) {
    mse_data$FH[framework$obs_dom == FALSE] <- NA
    mse_data$Out[framework$obs_dom == FALSE] <- 1

    message(strwrap(prefix = " ", initial = "",
                    "Please note that the jackknife MSE is only available for
                    in-sample domains."))
  }

  mse_out <- list(
    mse_data = mse_data,
    MSE_method = "jackknife",
    g1 = g1,
    diff_jack_g1 = jack_sigmau2,
    diff_jack_eblups = diff_jack_eblups
  )

  return(mse_out)
}


################################################################################
### MSE estimators taken from saeRobust
pseudo <- function(framework, combined_data, eblup, mse_type, method) {
  MSE <- saeRobust::mse(
    object = eblup$eblupobject, type = mse_type,
    predType = method
  )
  mse_data <- data.frame(Domain = framework$combined_data[[framework$domains]])
  mse_data$Direct <- NA
  mse_data$Direct[framework$obs_dom == TRUE] <- framework$vardir
  if (is.element("reblup", method)) {
    mse_data$FH[framework$obs_dom == TRUE] <- MSE$pseudo
  }
  if (is.element("reblupbc", method)) {
    mse_data$FH[framework$obs_dom == TRUE] <- MSE$pseudobc
  }
  mse_data$Out[framework$obs_dom == TRUE] <- 0
  if (!all(framework$obs_dom == TRUE)) {
    mse_data$FH[framework$obs_dom == FALSE] <- NA
    mse_data$Out[framework$obs_dom == FALSE] <- 1
  }

  mse_data <- list(
    mse_data = mse_data,
    MSE_method = "pseudo linearization"
  )
}

robustboot <- function(framework, combined_data, eblup, mse_type, B, method) {
  MSE <- saeRobust::mse(
    object = eblup$eblupobject, type = mse_type,
    predType = method, B = B
  )
  mse_data <- data.frame(Domain = framework$combined_data[[framework$domains]])
  mse_data$Direct <- NA
  mse_data$Direct[framework$obs_dom == TRUE] <- framework$vardir
  if (is.element("reblup", method)) {
    mse_data$FH[framework$obs_dom == TRUE] <- MSE$boot
  }
  if (is.element("reblupbc", method)) {
    mse_data$FH[framework$obs_dom == TRUE] <- MSE$bootbc
  }
  mse_data$Out[framework$obs_dom == TRUE] <- 0
  if (!all(framework$obs_dom == TRUE)) {
    mse_data$FH[framework$obs_dom == FALSE] <- NA
    mse_data$Out[framework$obs_dom == FALSE] <- 1
  }

  mse_data <- list(
    mse_data = mse_data,
    MSE_method = "bootstrap"
  )
}




wrapper_MSE <- function(framework, combined_data, sigmau2, vardir, Ci, eblup,
                        transformation, method, interval, mse_type,
                        B = NULL) {
  mse_data <- if (mse_type == "analytical") {
    analytical_mse(
      framework = framework, sigmau2 = sigmau2,
      combined_data = combined_data, method = method
    )
  } else if (mse_type == "jackknife") {
    if (method == "me") {
      jiang_jackknife_yl(
        framework = framework, combined_data = combined_data,
        sigmau2 = sigmau2, vardir = vardir, Ci = Ci,
        eblup = eblup, transformation = transformation,
        method = method
      )
    } else {
      jiang_jackknife(
        framework = framework, combined_data = combined_data,
        sigmau2 = sigmau2, vardir = vardir, eblup = eblup,
        transformation = transformation, method = method,
        interval = interval
      )
    }
  } else if (mse_type == "weighted_jackknife") {
    chen_weighted_jackknife(
      framework = framework,
      combined_data = combined_data,
      sigmau2 = sigmau2, eblup = eblup, vardir = vardir,
      transformation = transformation, method = method,
      interval = interval
    )
  } else if (mse_type == "pseudo") {
    pseudo(
      framework = framework, combined_data = combined_data, eblup = eblup,
      mse_type = "pseudo", method = method
    )
  } else if (mse_type == "boot") {
    robustboot(
      framework = framework, combined_data = combined_data,
      eblup = eblup,
      mse_type = "boot", method = method, B = B
    )
  } else if (mse_type == "spatialnonparboot" ||
    mse_type == "spatialnonparbootbc") {
    nonparametricboot_spatial(
      framework = framework,
      combined_data = combined_data,
      sigmau2 = sigmau2, eblup = eblup, B = B,
      method = method, vardir = vardir,
      transformation = transformation,
      mse_type = mse_type
    )
  } else if (mse_type == "spatialparboot" || mse_type == "spatialparbootbc") {
    parametricboot_spatial(
      framework = framework,
      combined_data = combined_data,
      sigmau2 = sigmau2, eblup = eblup, B = B,
      method = method, vardir = vardir,
      transformation = transformation,
      mse_type = mse_type
    )
  }

  return(mse_data)
}
