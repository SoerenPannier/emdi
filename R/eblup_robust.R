eblup_robust <- function(framework, combined_data, predType, k = 1.345, vardir, c,
                         correlation, corMatrix, time) {
  
  if (correlation == "no"){
    eblupobject <- rfh(framework$formula, data = framework$data,
                       samplingVar = vardir)
  } else if (correlation == "spatial"){
    if (is.matrix(corMatrix) == FALSE){corMatrix <- as.matrix(corMatrix)}
    eblupobject <- rfh(framework$formula, data = framework$data,
                       samplingVar = vardir, corSAR1(corMatrix), k = k)
  } else if (correlation == "temporal"){
    nTime <- length(unique(framework$data[[time]]))
    eblupobject <- rfh(framework$formula, data = framework$data,
                       samplingVar = vardir, corAR1(nTime = nTime), k = k)
  } else if (correlation == "spatio-temporal"){
    if (is.matrix(corMatrix) == FALSE){corMatrix <- as.matrix(corMatrix)}
    nTime <- length(unique(framework$data[[time]]))
    eblupobject <- rfh(framework$formula, data = framework$data,
                       samplingVar = vardir, corSAR1AR1(W = corMatrix,nTime = nTime),
                       k = k)
  }
  
  eblupobject$linear <- predict(eblupobject, type = "linear")$linear
  eblupobject$reblupbc <- predict(eblupobject, type = "reblupbc", c = c)$reblupbc
  # Inference for coefficients
  eblup_coef <- data.frame(coefficients = eblupobject$coefficients)
  
  EBLUP_data <- data.frame(Domain = combined_data[[framework$domains]])
  #direct
  EBLUP_data$Direct <- NA
  EBLUP_data$Direct[framework$obs_dom == TRUE] <- framework$direct
  #eblup
  # bias corrected prediction
  if (is.element("linear", predType)) EBLUP_data$FH[framework$obs_dom == TRUE] <- eblupobject$linear
  if (is.element("reblup", predType)) EBLUP_data$FH[framework$obs_dom == TRUE] <- eblupobject$reblup
  if (is.element("reblupbc", predType)) EBLUP_data$FH[framework$obs_dom == TRUE] <- eblupobject$reblupbc
  EBLUP_data$Out[framework$obs_dom == TRUE] <- 0
 
  # Scores
  eblupobject$score <- score(eblupobject)
  names(eblupobject$score) <- c("coefficients", "variance", "re")
  names(eblupobject$score$coefficients) <- names(coefficients(eblupobject))
  names(eblupobject$score$variance) <- names(eblupobject$variance)
  
  # Iterations:
  eblupobject$iter <- c(
    "model parameter" = NROW(eblupobject$iterations$coefficients),
    "random effects" = NROW(eblupobject$iterations$re)
  )
  
  
  eblup_out <- list(EBLUP_data = EBLUP_data,
                    fitted <- eblupobject$fitted,
                    coefficients = eblup_coef,
                    real_res = eblupobject$residuals,
                    std_real_res = eblupobject$residuals/sqrt(eblupobject$samplingVar),
                    random_effects = eblupobject$re,
                    eblupobject = eblupobject,
                    variance = eblupobject$variance,
                    W = eblupobject$W,
                    nTime = eblupobject$nTime,
                    scores = eblupobject$score,
                    iterations = eblupobject$iter,
                    maxIter = eblupobject$maxIter,
                    maxIterParam = eblupobject$maxIterParam,
                    maxIterRe = eblupobject$maxIterRe)
  
  return(eblup_out)
}
