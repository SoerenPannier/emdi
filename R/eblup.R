eblup_FH <- function(framework, sigmau2, combined_data) {

  # Estimation of the regression coefficients
  # Identity matrix mxm
  D <- diag(1, framework$m)
  # Total variance-covariance matrix - only values on the diagonal due to
  # independence of error terms
  V <- sigmau2 * D %*% t(D) + diag(as.numeric(framework$vardir))
  # Inverse of the total variance
  Vi <- solve(V)
  # Inverse of X'ViX
  Q <- solve(t(framework$model_X) %*% Vi %*% framework$model_X)
  # Beta by (X'ViX)^-1 X'Viy
  beta_hat <- Q %*% t(framework$model_X) %*% Vi %*% framework$direct

  # Inference for coefficients
  std_errorbeta <- sqrt(diag(Q))
  tvalue <- beta_hat / std_errorbeta
  pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)

  eblup_coef <- data.frame(
    coefficients = beta_hat,
    std.error = std_errorbeta,
    t.value = tvalue,
    p.value = pvalue
  )

  # Computation of the EBLUP
  res <- framework$direct - c(framework$model_X %*% beta_hat)
  sigmau2Diag <- sigmau2 * D
  u_hat <- sigmau2Diag %*% t(D) %*% Vi %*% res

  # Computation of shrinkage factor
  gamma <- sigmau2 / (sigmau2 + framework$vardir)
  real_res <- framework$direct - (framework$model_X %*% beta_hat + D %*% u_hat)
  std_real_res <- real_res / sqrt(framework$vardir)
  fitted <- as.vector(framework$model_X %*% beta_hat)

  eblup_data <- data.frame(
    Domain =
      framework$combined_data[[framework$domains]]
  )
  eblup_data$Direct <- NA
  eblup_data$Direct[framework$obs_dom == TRUE] <- framework$direct

  if (all(framework$obs_dom == TRUE)) {
    eblup_data$FH[framework$obs_dom == TRUE] <- framework$model_X %*% beta_hat +
      D %*% u_hat
    eblup_data$Out[framework$obs_dom == TRUE] <- 0
  } else {
    # Prediction

    pred_X <- get_covariates(framework = framework)$X
    #pred_y <- pred_X %*% beta_hat

    # Small area mean
    eblup_data$FH[framework$obs_dom == TRUE] <- framework$model_X %*% beta_hat +
      D %*% u_hat
    eblup_data$FH[framework$obs_dom == FALSE] <- pred_X %*% beta_hat
    eblup_data$Out[framework$obs_dom == TRUE] <- 0
    eblup_data$Out[framework$obs_dom == FALSE] <- 1
  }

  eblup_out <- list(
    eblup_data = eblup_data,
    fitted = fitted,
    gamma = gamma,
    coefficients = eblup_coef,
    real_res = real_res,
    std_real_res = std_real_res,
    random_effects = u_hat,
    beta_vcov = Q
  )

  return(eblup_out)
}

# Get covariates for all domains
get_covariates <- function(framework, only_out = TRUE) {

  if (only_out == TRUE) {
    pred_data_tmp <- framework$combined_data[framework$obs_dom == FALSE, ]
  } else {
    pred_data_tmp <- framework$combined_data
  }

  pred_data_tmp <- data.frame(pred_data_tmp, helper = rnorm(1,0,1))
  formula.tools::lhs(framework$formula) <- quote(helper)
  pred_data <- makeXY(formula = framework$formula, data = pred_data_tmp)

  return(list(y = pred_data$y, X = pred_data$x))
}

eblup_SFH <- function(framework, sigmau2, combined_data) {

  # Estimation of the regression coefficients
  # Identity matrix mxm
  D <- diag(1, framework$m)
  Wt <- t(framework$W)
  A <- solve((D - sigmau2$rho * Wt) %*% (D - sigmau2$rho * framework$W))
  G <- sigmau2$sigmau2 * A
  # Total variance-covariance matrix
  V <- G + D * framework$vardir
  # Inverse of the total variance
  Vi <- solve(V)
  # Inverse of X'ViX
  Q <- solve(t(framework$model_X) %*% Vi %*% framework$model_X)
  # Beta by (X'ViX)^-1 X'Viy
  beta_hat <- Q %*% t(framework$model_X) %*% Vi %*% framework$direct

  # Inference for coefficients
  std_errorbeta <- sqrt(diag(Q))
  tvalue <- beta_hat / std_errorbeta
  pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)

  eblup_coef <- data.frame(
    coefficients = beta_hat,
    std.error = std_errorbeta,
    t.value = tvalue,
    p.value = pvalue
  )

  # Computation of the EBLUP
  res <- framework$direct - c(framework$model_X %*% beta_hat)

  # Computation of the random effect
  u_hat <- G %*% Vi %*% res

  # Computation of the residuals
  real_res <- framework$direct - (framework$model_X %*% beta_hat +
    G %*% Vi %*% res)
  std_real_res <- real_res / sqrt(framework$vardir) # Achtung nochmal prüfen
  fitted <- as.vector(framework$model_X %*% beta_hat)

  eblup_data <- data.frame(
    Domain =
      framework$combined_data[[framework$domains]]
  )
  eblup_data$Direct <- NA
  eblup_data$Direct[framework$obs_dom == TRUE] <- framework$direct

  if (all(framework$obs_dom == TRUE)) {
    eblup_data$FH[framework$obs_dom == TRUE] <- (framework$model_X %*%
      beta_hat +
      G %*% Vi %*% res)
    eblup_data$Out[framework$obs_dom == TRUE] <- 0
  } else {
    # Prediction
    pred_data_tmp <- combined_data[framework$obs_dom == FALSE, ]

    pred_data_tmp <- data.frame(pred_data_tmp, helper = rnorm(1, 0, 1))
    formula.tools::lhs(framework$formula) <- quote(helper)
    pred_data <- makeXY(formula = framework$formula, data = pred_data_tmp)

    pred_X <- pred_data$x
    pred_y <- pred_X %*% beta_hat

    # Small area mean
    eblup_data$FH[framework$obs_dom == TRUE] <-
      (framework$model_X %*% beta_hat + G %*% Vi %*% res)
    eblup_data$FH[framework$obs_dom == FALSE] <- pred_y
    eblup_data$Out[framework$obs_dom == TRUE] <- 0
    eblup_data$Out[framework$obs_dom == FALSE] <- 1
  }

  eblup_out <- list(
    eblup_data = eblup_data,
    fitted = fitted,
    coefficients = eblup_coef,
    real_res = real_res,
    V = V,
    std_real_res = std_real_res,
    random_effects = u_hat,
    beta_vcov = Q
  )

  return(eblup_out)
}


eblup_YL <- function(framework, sigmau2, combined_data) {

  # Identity matrix mxm
  D <- diag(1, framework$m)

  # Total variance-covariance matrix - only values on the diagonal due to
  # independence of error terms
  beta_hat_tCi_beta_hat <- NULL
  for (i in seq_len(framework$m)) {
    beta_hat_tCi_beta_hat[i] <-
      t(sigmau2$betahatw) %*% framework$Ci[, , i] %*% sigmau2$betahatw
  }
  V <- sigmau2$sigmau_YL * D %*% t(D) + diag(as.numeric(framework$vardir)) +
    diag(as.numeric(beta_hat_tCi_beta_hat))

  # Inverse of the total variance
  Vi <- solve(V)

  # Inverse of X'ViX
  Q <- solve(t(framework$model_X) %*% Vi %*% framework$model_X)

  # Inference for coefficients
  std_errorbeta <- sqrt(diag(Q))
  tvalue <- sigmau2$betahatw / std_errorbeta
  pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)

  eblup_coef <- data.frame(
    coefficients = sigmau2$betahatw,
    std.error = std_errorbeta,
    t.value = tvalue,
    p.value = pvalue
  )

  # Computation of the EBLUP

  # Computation of shrinkage factor

  gamma <- (sigmau2$sigmau_YL + beta_hat_tCi_beta_hat) /
    (sigmau2$sigmau_YL + beta_hat_tCi_beta_hat + framework$vardir)

  res <- framework$direct - c(framework$model_X %*% sigmau2$betahatw)
  u_hat <- gamma * res

  real_res <- framework$direct - (framework$model_X %*% sigmau2$betahatw +
    u_hat)
  std_real_res <- real_res / sqrt(framework$vardir)
  fitted <- as.vector(framework$model_X %*% sigmau2$betahatw[, 1])

  eblup_data <- data.frame(
    Domain =
      framework$combined_data[[framework$domains]]
  )
  eblup_data$Direct <- NA
  eblup_data$Direct[framework$obs_dom == TRUE] <- framework$direct

  if (all(framework$obs_dom == TRUE)) {
    eblup_data$FH[framework$obs_dom == TRUE] <- gamma * framework$direct +
      (1 - gamma) * framework$model_X %*% sigmau2$betahatw[, 1]
    eblup_data$Out[framework$obs_dom == TRUE] <- 0
  } else {
    # Prediction
    pred_data_tmp <- combined_data[framework$obs_dom == FALSE, ]

    pred_data_tmp <- data.frame(pred_data_tmp, helper = rnorm(1, 0, 1))
    formula.tools::lhs(framework$formula) <- quote(helper)
    pred_data <- makeXY(formula = framework$formula, data = pred_data_tmp)
    pred_X <- pred_data$x
    pred_y <- pred_X %*% sigmau2$betahatw

    # Small area mean
    eblup_data$FH[framework$obs_dom == TRUE] <- gamma * framework$direct +
      (1 - gamma) * framework$model_X %*% sigmau2$betahatw[, 1]
    eblup_data$FH[framework$obs_dom == FALSE] <- pred_y
    eblup_data$Out[framework$obs_dom == TRUE] <- 0
    eblup_data$Out[framework$obs_dom == FALSE] <- 1
  }

  eblup_out <- list(
    eblup_data = eblup_data,
    fitted = fitted,
    gamma = gamma,
    coefficients = eblup_coef,
    real_res = real_res,
    std_real_res = std_real_res,
    random_effects = u_hat,
    beta_vcov = Q
  )

  return(eblup_out)
}
