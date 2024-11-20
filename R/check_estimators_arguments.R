estimators_check <- function(object,
                             indicator,
                             MSE,
                             CV,
                             level) {

  #____________________________Rachael__________________________________________
  if (any(inherits(object, which = TRUE, c("ebp_tf", "fh_tf")))) {
    if (is.null(level)) {
      stop("For 'ebp_tf' or 'fh_tf' models, please specify the 'level' argument.
           'level' can take either 'domain' or 'subdomain' arguments")
    }else if (!level %in% c("domain", "subdomain")) {
      stop("'level' can take either 'domain' or 'subdomain' arguments.")
    }
  } else {
    level <- NULL
  }
#_______________________________________________________________________________
 #_________________________Rachael added________________________________________
  if (any(inherits(object, which = TRUE, c("ebp_tf", "fh_tf")))) {
    if (is.null(object$MSE_Domain) || is.null(object$MSE_Subdomain) &&
        (MSE == TRUE || CV == TRUE)) {
      stop(strwrap(prefix = " ", initial = "",
                   "No MSE estimates in emdi object: arguments MSE and CV have to
                 be FALSE or a new emdi object with variance/MSE needs to be
                 generated."))
    }
  }else {
    if (is.null(object$MSE) && (MSE == TRUE || CV == TRUE)) {
      stop(strwrap(prefix = " ", initial = "",
                   "No MSE estimates in emdi object: arguments MSE and CV have to
                 be FALSE or a new emdi object with variance/MSE needs to be
                 generated."))
    }
  }
#_________________________Rachael Added_________________________________________
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
  } else if (inherits(object, "ebp_tf")) {
    if (is.null(indicator) || !all(indicator == "all" | indicator == "All" |
                                   indicator == "Quantiles" |
                                   indicator == "quantiles" |
                                   indicator == "Poverty" |
                                   indicator == "poverty" |
                                   indicator == "Inequality" |
                                   indicator == "inequality" |
                                   indicator == "Custom" |
                                   indicator == "custom" |
                                   indicator %in% names(object$ind_Subdomain[-1])|
                                   indicator %in% names(object$ind_Domain[-1]))) {
      stop(strwrap(prefix = " ", initial = "",
                   paste0("The argument indicator is set to ", indicator, ".
                          The argument only allows to be set to all, a name of
                          estimated indicators or indicator groups as described
                          in help(estimators.emdi).")))
    }
  } else if (inherits(object, "fh_tf") && level == "domain") {
      if (is.null(indicator) || !all(indicator == "all" | indicator == "All" |
                                     indicator == "FH_TF")) {
        stop(strwrap(prefix = " ", initial = "",
                     paste0("The argument indicator is set to ", indicator, ".
                   The argument only allows to be set to all or FH_TF for
                            the fh_tf object at domain level.")))
      }} else if(inherits(object, "fh_tf") && level == "subdomain"){
        if (is.null(indicator) || !all(indicator == "all" | indicator == "All" |
                                       indicator == "FH_TF" |
                                       indicator == "Direct")) {
          stop(strwrap(prefix = " ", initial = "",
                       paste0("The argument indicator is set to ", indicator, ".
                   The argument only allows to be set to all, FH_TF or Direct
                              for the fh_tf object at subdomain level.")))
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
