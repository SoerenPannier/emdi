eblup_robust <- function(framework, combined_data, method, k = 1.345, vardir, c,
                         correlation, corMatrix) {
  
  if (correlation == "no"){
    eblupobject <- saeRobust::rfh(framework$formula, data = framework$data,
                       samplingVar = vardir,
                       tol = framework$tol, maxIter = framework$maxit)
  } else if (correlation == "spatial"){
    if (is.matrix(corMatrix) == FALSE){corMatrix <- as.matrix(corMatrix)}
    eblupobject <- saeRobust::rfh(framework$formula, data = framework$data,
                       samplingVar = vardir, saeRobust::corSAR1(corMatrix), k = k,
                       tol = framework$tol, maxIter = framework$maxit)
  } 
  
  eblupobject$linear <- stats::predict(eblupobject, type = "linear")$linear
  eblupobject$reblupbc <- stats::predict(eblupobject, type = "reblupbc", c = c)$reblupbc
  # Inference for coefficients
  eblup_coef <- data.frame(coefficients = eblupobject$coefficients)
  
  EBLUP_data <- data.frame(Domain = combined_data[[framework$domains]])
  #direct
  EBLUP_data$Direct <- NA
  EBLUP_data$Direct[framework$obs_dom == TRUE] <- framework$direct
  #eblup
  # bias corrected prediction
  if (is.element("linear", method)) EBLUP_data$FH[framework$obs_dom == TRUE] <- eblupobject$linear
  if (is.element("reblup", method)) EBLUP_data$FH[framework$obs_dom == TRUE] <- eblupobject$reblup
  if (is.element("reblupbc", method)) EBLUP_data$FH[framework$obs_dom == TRUE] <- eblupobject$reblupbc
  EBLUP_data$Out[framework$obs_dom == TRUE] <- 0
 
  # Scores
 # eblupobject$score <- saeRobust::score(eblupobject)
 # names(eblupobject$score) <- c("coefficients", "variance", "re")
 # names(eblupobject$score$coefficients) <- names(coefficients(eblupobject))
 # names(eblupobject$score$variance) <- names(eblupobject$variance)
  
  # Iterations:
 # eblupobject$iter <- c(
  #  "model parameter" = NROW(eblupobject$iterations$coefficients),
  #  "random effects" = NROW(eblupobject$iterations$re)
 # )
  
  
  eblup_out <- list(EBLUP_data = EBLUP_data,
                    fitted <- eblupobject$fitted,
                    coefficients = eblup_coef,
                    real_res = eblupobject$residuals,
                    std_real_res = eblupobject$residuals/sqrt(eblupobject$samplingVar),
                    random_effects = eblupobject$re,
                    eblupobject = eblupobject,
                    variance = eblupobject$variance) #,
                   # W = eblupobject$W,
                   # scores = eblupobject$score,
                   # iterations = eblupobject$iter,
                   # maxIter = eblupobject$maxIter,
                   # maxIterParam = eblupobject$maxIterParam,
                   # maxIterRe = eblupobject$maxIterRe)
  
  return(eblup_out)
}
