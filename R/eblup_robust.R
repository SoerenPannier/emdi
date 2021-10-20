eblup_robust <- function(framework, combined_data, method, k = 1.345, vardir,
                         mult_constant, correlation, corMatrix) {

  if (correlation == "no") {
    eblupobject <- saeRobust::rfh(framework$formula, data = framework$data,
                       samplingVar = vardir,
                       tol = framework$tol, maxIter = framework$maxit)

    # Identity matrix mxm
    D <- diag(1, framework$m)
    model_X <- as.matrix(eblupobject$x)
    # Total variance-covariance matrix - only values on the diagonal due to
    # independence of error terms
    V <- eblupobject$variance * D %*% t(D) + diag(as.numeric(framework$vardir))
    # Inverse of the total variance
    Vi <- solve(V)
    # Inverse of X'ViX
    Q <- solve(t(model_X) %*% Vi %*% model_X)

    # Inference for coefficients
    std.errorbeta <- sqrt(diag(Q))
    tvalue <- eblupobject$coefficients/std.errorbeta
    pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)

    eblup_coef <- data.frame(coefficients = eblupobject$coefficients,
                             std.error = std.errorbeta,
                             t.value = tvalue,
                             p.value = pvalue)

  } else if (correlation == "spatial") {
    if (is.matrix(corMatrix) == FALSE) {corMatrix <- as.matrix(corMatrix)}
    eblupobject <- saeRobust::rfh(framework$formula, data = framework$data,
                       samplingVar = vardir, saeRobust::corSAR1(corMatrix),
                       k = k, tol = framework$tol, maxIter = framework$maxit)

    # Identity matrix mxm
    D <- diag(1, framework$m)
    model_X <- as.matrix(eblupobject$x)
    Wt <- t(framework$W)
    A <- solve((D - eblupobject$variance[1] *
        Wt) %*% (D - eblupobject$variance[1]*framework$W))
    G <- eblupobject$variance[2] * A
    # Total variance-covariance matrix
    V <- G + D * framework$vardir
    # Inverse of the total variance
    Vi <- solve(V)
    # Inverse of X'ViX
    Q <- solve(t(model_X) %*% Vi %*% model_X)

    # Inference for coefficients
    std.errorbeta <- sqrt(diag(Q))
    tvalue <- eblupobject$coefficients/std.errorbeta
    pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)

    eblup_coef <- data.frame(coefficients = eblupobject$coefficients,
                             std.error = std.errorbeta,
                             t.value = tvalue,
                             p.value = pvalue)
  }

  eblupobject$linear <- stats::predict(eblupobject, type = "linear")$linear
  eblupobject$reblupbc <- stats::predict(eblupobject, type = "reblupbc",
                                         c = mult_constant)$reblupbc

  EBLUP_data <- data.frame(Domain =
                             framework$combined_data[[framework$domains]])
  # direct
  EBLUP_data$Direct <- NA
  EBLUP_data$Direct[framework$obs_dom == TRUE] <- framework$direct
  # eblup
  # bias corrected prediction
  if (is.element("linear", method))
    EBLUP_data$FH[framework$obs_dom == TRUE] <- eblupobject$linear
  if (is.element("reblup", method))
    EBLUP_data$FH[framework$obs_dom == TRUE] <- eblupobject$reblup
  if (is.element("reblupbc", method))
    EBLUP_data$FH[framework$obs_dom == TRUE] <- eblupobject$reblupbc
  EBLUP_data$Out[framework$obs_dom == TRUE] <- 0
  EBLUP_data$Out[framework$obs_dom == FALSE] <- 1

  if (!all(framework$obs_dom == TRUE)) {
    message("Please note that the results are only returned for in-sample
            domains. For more information see help(fh).")
  }

  eblup_out <- list(EBLUP_data = EBLUP_data,
                    fitted = eblupobject$fitted,
                    coefficients = eblup_coef,
                    real_res = eblupobject$residuals,
                    std_real_res =
                      eblupobject$residuals / sqrt(eblupobject$samplingVar),
                    random_effects = eblupobject$re,
                    eblupobject = eblupobject,
                    variance = eblupobject$variance,
                    beta_vcov = Q)

  return(eblup_out)
}
