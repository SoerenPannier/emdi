
mse_emdi <- function(object, indicator = "all", CV = FALSE) {

  if (is.null(object$MSE) && CV == TRUE) {
    stop('No MSE estimates in emdi object: arguments MSE and CV have to be FALSE
          or a new emdi object with variance/MSE needs to be generated.')
  }
  if ((ncol(object$ind) == 11) && any(indicator == "Custom" || indicator == "custom")) {
    stop('No individual indicators are defined. Either select other indicators or
         define custom indicators and generate a new emdi object. See also help(ebp).')
  }

  # Calculation of CVs
  if(inherits(object, "fh")) {
    object$MSE <- object$MSE[, c("Domain", "Direct", "FH")]
    object$ind <- object$ind[, c("Domain", "Direct", "FH")]
  }
  all_cv <- sqrt(object$MSE[,-1]) / object$ind[,-1]

  if (any(indicator == "Quantiles") || any(indicator == "quantiles")) {
    indicator = c(indicator[!(indicator == "Quantiles" || indicator == "quantiles")], "Quantile_10", "Quantile_25", "Median", "Quantile_75", "Quantile_90")
  }
  if (any(indicator == "poverty") || any(indicator == "Poverty")) {
    indicator = c(indicator[!(indicator == "poverty" || indicator == "Poverty")], "Head_Count", "Poverty_Gap")
  }
  if (any(indicator == "inequality") || any(indicator == "Inequality")) {
    indicator = c(indicator[!(indicator == "inequality" || indicator == "Inequality")], "Gini", "Quintile_Share")
  }
  if (any(indicator == "custom") || any(indicator == "Custom")) {
     indicator = c(indicator[!(indicator == "custom" || indicator == "Custom")],
                   colnames(object$ind[-c(1,2,3,4,5,6,7,8,9,10,11)]))
  }

  if (any(indicator == "all") || any(indicator == "All" )) {
    ind <- object$MSE
    ind_cv <- cbind(Domain = object$MSE[,1], all_cv)
    ind_name <- "All indicators"
  } else if (any(indicator == "fh") || any(indicator == "FH" )) {
    ind <- object$MSE[, c("Domain", "FH")]
    ind_cv <- cbind(Domain = object$MSE[,1], all_cv)
    ind_name <- "Fay-Herriot estimates"
  } else if (any(indicator == "Direct") || any(indicator == "direct" )) {
    ind <- object$MSE[, c("Domain", "Direct")]
    ind_cv <- cbind(Domain = object$MSE[,1], all_cv)
    ind_name <- "Direct estimates used in Fay-Herriot approach"
  } else {
    selection = colnames(object$MSE[-1]) %in% indicator
    ind <- object$MSE[,c(TRUE, selection)]
    ind_cv <- data.frame(Domain = object$MSE[,1], all_cv[, selection])
    colnames(ind_cv) = colnames(ind)
    ind_name <- paste(unique(indicator), collapse = ", ")
  }

  if (CV == FALSE) {
  mse_emdi <- list(ind = ind, ind_name = ind_name)
  } else {
  mse_emdi <- list(ind = ind, ind_cv = ind_cv, ind_name = ind_name)
  }

  class(mse_emdi) <- "mse.emdi"

  return(mse_emdi)
}


