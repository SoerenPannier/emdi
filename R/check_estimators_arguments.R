estimators_check <- function(object, 
                             indicator, 
                             MSE, 
                             CV){
  if (!inherits(object, "emdi")) {
    stop('First object needs to be of class emdi.')
  }
  if (is.null(object$MSE) && (MSE == TRUE || CV == TRUE)) {
    stop('No MSE estimates in emdi object: arguments MSE and CV have to be FALSE
          or a new emdi object with variance/MSE needs to be generated.')
  }
  if (!(inherits(MSE, "logical") && length(MSE) == 1)) {
    stop("MSE must be a logical value. Set MSE to TRUE or FALSE.")
  }
  if (!(inherits(CV, "logical") && length(CV) == 1)) {
    stop("CV must be a logical value. Set CV to TRUE or FALSE.")
  }
  if (is.null(indicator) || !(indicator == "all" || indicator == "Quantiles" 
       || indicator == "quantiles"
       || indicator == "Poverty" || indicator == "poverty" 
       || indicator == "Inequality" || indicator == "inequality" 
       || indicator == "Custom" || indicator == "custom" 
       || indicator %in% names(object$ind[-1]))) {
    stop("indicator is a character vector that can only contain the names
         of estimated indicators or 'all' or indicator groups as described in 
         help(estimators.emdi).")
  }
}