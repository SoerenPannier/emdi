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
    EBLUP_data$Out[framework$obs_dom == TRUE] <- 0
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
    EBLUP_data$Out[framework$obs_dom == TRUE] <- 0
    EBLUP_data$Out[framework$obs_dom == FALSE] <- 1
  }

  eblup_out <- list(EBLUP_data = EBLUP_data,
                    gamma = gamma,
                    coefficients = eblup_coef,
                    real_res = real_res,
                    std_real_res = std_real_res,
                    random_effects = u.hat)

  return(eblup_out)
}

eblup_SFH <- function(framework, sigmau2, combined_data) {
  
  # Estimation of the regression coefficients
  # Identity matrix mxm
  D <- diag(1, framework$m)
  Wt <- t(framework$W)
  A <- solve((D - sigmau2$rho*Wt)%*%(D - sigmau2$rho*framework$W))
  G <- sigmau2$sigmau2*A
  # Total variance-covariance matrix 
  V <- G + D*framework$vardir
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
  #sigmau2Diag <- sigmau2$sigmau2*D
  
  # Computation of the random effect
  u.hat <- G%*%Vi%*%res
  
  # Computation of the residuals
  real_res <- framework$direct - (framework$model_X%*%Beta.hat + G%*%Vi%*%res)
  std_real_res <- real_res / sqrt(framework$vardir) # Achtung nochmal prüfen
  
  EBLUP_data <- data.frame(Domain = combined_data[[framework$domains]])
  EBLUP_data$Direct <- NA
  EBLUP_data$Direct[framework$obs_dom == TRUE] <- framework$direct
  
  if (all(framework$obs_dom == TRUE)) {
    EBLUP_data$FH[framework$obs_dom == TRUE] <- (framework$model_X%*%Beta.hat +
                                                   G%*%Vi%*%res)
    EBLUP_data$Out[framework$obs_dom == TRUE] <- 0
  } else {
    # Prediction
    pred_data_tmp <- combined_data[framework$obs_dom == FALSE,]
    
    pred_data_tmp <- data.frame(pred_data_tmp, helper = rnorm(1,0,1))
    lhs(framework$formula) <- quote(helper)
    pred_data <- makeXY(formula = framework$formula, data = pred_data_tmp)
    
    pred_X <- pred_data$x
    pred_y <- pred_X %*% Beta.hat
    
    # Small area mean
    EBLUP_data$FH[framework$obs_dom == TRUE] <- (framework$model_X%*%Beta.hat + 
                                                   G%*%Vi%*%res)
    EBLUP_data$FH[framework$obs_dom == FALSE] <- pred_y
    EBLUP_data$Out[framework$obs_dom == TRUE] <- 0
    EBLUP_data$Out[framework$obs_dom == FALSE] <- 1
  }
  
  eblup_out <- list(EBLUP_data = EBLUP_data,
                    coefficients = eblup_coef,
                    real_res = real_res,
                    V = V,
                    std_real_res = std_real_res,
                    random_effects = u.hat)
  
  return(eblup_out)
}


eblup_YL <- function(framework, sigmau2, combined_data) {
  
  # Identity matrix mxm
  D <- diag(1, framework$m)
  
  # Total variance-covariance matrix - only values on the diagonal due to
  # independence of error terms
  Beta.hat.tCiBeta.hat <- NULL
  for(i in 1:framework$m){
    Beta.hat.tCiBeta.hat[i] <- 
      t(sigmau2$betahatw)%*%framework$Ci[,,i]%*%sigmau2$betahatw
  }
  V <- sigmau2$sigmau_YL * D%*%t(D) + diag(as.numeric(framework$vardir)) + 
    diag(as.numeric(Beta.hat.tCiBeta.hat))
  
  # Inverse of the total variance
  Vi <- solve(V)
  
  # Inverse of X'ViX
  Q <- solve(t(framework$model_X)%*%Vi%*%framework$model_X)
  
  # Inference for coefficients
  std.errorbeta <- sqrt(diag(Q))
  tvalue <- sigmau2$betahatw/std.errorbeta
  pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)
  
  eblup_coef <- data.frame(coefficients = sigmau2$betahatw,  
                           std.error = std.errorbeta,
                           t.value = tvalue,
                           p.value = pvalue)
  
  # Computation of the EBLUP
  
  # Computation of shrinkage factor
  
  gamma <- (sigmau2$sigmau_YL + Beta.hat.tCiBeta.hat)/
    (sigmau2$sigmau_YL + Beta.hat.tCiBeta.hat + framework$vardir)	
  
  res <- framework$direct - c(framework$model_X%*%sigmau2$betahatw)
  # sigmau2Diag <- sigmau2*D
  u.hat <- gamma*res
  
  real_res <- framework$direct - (framework$model_X%*%sigmau2$betahatw + u.hat)
  std_real_res <- real_res / sqrt(framework$vardir)
  
  EBLUP_data <- data.frame(Domain = combined_data[[framework$domains]])
  EBLUP_data$Direct <- NA
  EBLUP_data$Direct[framework$obs_dom == TRUE] <- framework$direct
  
  if (all(framework$obs_dom == TRUE)) {
  EBLUP_data$FH[framework$obs_dom == TRUE] <- gamma*framework$direct + 
    (1-gamma)* framework$model_X%*%sigmau2$betahatw[,1] #t(framework$model_X)
  EBLUP_data$Out[framework$obs_dom == TRUE] <- 0
   } else {
  # Prediction
     pred_data_tmp <- combined_data[framework$obs_dom == FALSE,]
  
     pred_data_tmp <- data.frame(pred_data_tmp, helper = rnorm(1,0,1))
     lhs(framework$formula) <- quote(helper)
     pred_data <- makeXY(formula = framework$formula, data = pred_data_tmp)
     pred_X <- pred_data$x
     pred_y <- pred_X%*%sigmau2$betahatw
  
  # Small area mean
  EBLUP_data$EBLUP[framework$obs_dom == TRUE] <- gamma*framework$direct + 
    (1-gamma)* framework$model_X%*%sigmau2$betahatw[,1]
  EBLUP_data$EBLUP[framework$obs_dom == FALSE] <- pred_y
  EBLUP_data$Out[framework$obs_dom == TRUE] <- 0
  EBLUP_data$Out[framework$obs_dom == FALSE] <- 1
  }
  
  eblup_out <- list(EBLUP_data = EBLUP_data,
                    gamma = gamma,
                    coefficients = eblup_coef,
                    real_res = real_res,
                    std_real_res = std_real_res,
                    random_effects = u.hat)
  
  return(eblup_out)
}
