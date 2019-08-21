eblup_FH <- function(framework, sigmau2, combined_data) {

  # Estimation of the regression coefficients
  # Identity matrix mxm
  D <- diag(1, framework$m)
  # Total variance-covariance matrix - only values on the diagonal due to
  # independence of error terms
  V <- sigmau2 * D%*%t(D) + diag(as.numeric(framework$vardir))
  # Inverse of the total variance
  Vi <- solve(V)
  # Inverse of X'ViX
  Q <- solve(t(framework$model_X)%*%Vi%*%framework$model_X)
  # Beta by (X'ViX)^-1 X'Viy
  Beta.hat <- Q%*%t(framework$model_X)%*%Vi%*%framework$direct

  # Inference for coefficients
  std.errorbeta <- sqrt(diag(Q))
  tvalue <- Beta.hat/std.errorbeta
  pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)

  eblup_coef <- data.frame(coefficients = Beta.hat,
                             std.error = std.errorbeta,
                             t.value = tvalue,
                             p.value = pvalue)

  # Computation of the EBLUP
  res <- framework$direct - c(framework$model_X%*%Beta.hat)
  sigmau2Diag <- sigmau2*D
  u.hat <- sigmau2Diag%*%t(D)%*%Vi%*%res

  # Computation of shrinkage factor
  gamma <- sigmau2 / (sigmau2 + framework$vardir)
  real_res <- framework$direct - (framework$model_X%*%Beta.hat + D%*%u.hat)
  std_real_res <- real_res / sqrt(framework$vardir)

  EBLUP_data <- data.frame(Domain = combined_data[[framework$domains]])
  EBLUP_data$Direct <- NA
  EBLUP_data$Direct[framework$obs_dom == TRUE] <- framework$direct

  if (all(framework$obs_dom == TRUE)) {
    EBLUP_data$FH[framework$obs_dom == TRUE] <- framework$model_X%*%Beta.hat + D%*%u.hat
    EBLUP_data$ind[framework$obs_dom == TRUE] <- 0
  } else {
    # Prediction
    pred_data_tmp <- combined_data[framework$obs_dom == FALSE,]

    pred_data_tmp <- data.frame(pred_data_tmp, helper = rnorm(1,0,1))
    lhs(framework$formula) <- quote(helper)
    pred_data <- makeXY(formula = framework$formula, data = pred_data_tmp)

    pred_X <- pred_data$x
    pred_y <- pred_X %*% Beta.hat

    # Small area mean
    EBLUP_data$FH[framework$obs_dom == TRUE] <- framework$model_X%*%Beta.hat + D%*%u.hat
    EBLUP_data$FH[framework$obs_dom == FALSE] <- pred_y
    EBLUP_data$ind[framework$obs_dom == TRUE] <- 0
    EBLUP_data$ind[framework$obs_dom == FALSE] <- 1
  }

  eblup_out <- list(EBLUP_data = EBLUP_data,
                    gamma = gamma,
                    coefficients = eblup_coef,
                    real_res = real_res,
                    std_real_res = std_real_res,
                    random_effects = u.hat)

  return(eblup_out)
  }
