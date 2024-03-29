estimators_check <- function(object,
                             indicator,
                             MSE,
                             CV) {
  if (is.null(object$MSE) && (MSE == TRUE || CV == TRUE)) {
    stop(strwrap(prefix = " ", initial = "",
                 "No MSE estimates in emdi object: arguments MSE and CV have to
                 be FALSE or a new emdi object with variance/MSE needs to be
                 generated."))
  }
  if (!(inherits(MSE, "logical") && length(MSE) == 1)) {
    stop("MSE must be a logical value. Set MSE to TRUE or FALSE.")
  }
  if (!(inherits(CV, "logical") && length(CV) == 1)) {
    stop("CV must be a logical value. Set CV to TRUE or FALSE.")
  }
  if (inherits(object, "fh")) {
    if (is.null(indicator) || !all(indicator == "all" | indicator == "All" |
      indicator == "FH" |
      indicator == "FH_Bench" |
      indicator == "Direct")) {
      stop(strwrap(prefix = " ", initial = "",
                   paste0("The argument indicator is set to ", indicator, ".
                   The argument only allows to be set to all, FH, Direct or
                   FH_Bench (if benchmark function is used before).")))
    }
  } else {
    if (is.null(indicator) || !all(indicator == "all" | indicator == "All" |
      indicator == "Quantiles" |
      indicator == "quantiles" |
      indicator == "Poverty" |
      indicator == "poverty" |
      indicator == "Inequality" |
      indicator == "inequality" |
      indicator == "Custom" |
      indicator == "custom" |
      indicator %in% names(object$ind[-1]))) {
      stop(strwrap(prefix = " ", initial = "",
                   paste0("The argument indicator is set to ", indicator, ".
                          The argument only allows to be set to all, a name of
                          estimated indicators or indicator groups as described
                          in help(estimators.emdi).")))
    }
  }
}
