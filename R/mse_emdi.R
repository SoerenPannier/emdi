mse_emdi <- function(object, indicator = "all", CV = FALSE) {
  #___________________________________Rachael___________________________________
  if (any(inherits(object, which = T, c("ebp_tf", "fh_tf")))) {
    if (is.null(object$MSE_Domain) || is.null(object$MSE_Subdomain) && CV == TRUE) {
      stop(strwrap(prefix = " ", initial = "",
                   "No MSE estimates in emdi object: arguments MSE and CV have to
                 be FALSE or a new emdi object with variance/MSE needs to be
                 generated."))
    }
    if ((ncol(object$ind_Domain) == 11) && any(indicator == "Custom" |
                                        indicator == "custom")||
        (ncol(object$ind_Subdomain) == 11) && any(indicator == "Custom" |
                                               indicator == "custom")) {
      stop(strwrap(prefix = " ", initial = "",
                   "No individual indicators are defined. Either select other
                 indicators or define custom indicators and generate a new emdi
                 object. See also help(ebp)."))
    }
    all_cv_Domain <- sqrt(object$MSE_Domain[, -1, drop = FALSE]) / object$ind_Domain[, -1, drop = FALSE]
    all_cv_Subdomain <- sqrt(object$MSE_Subdomain[, -1]) / object$ind_Subdomain[, -1]

  }else{
    if (is.null(object$MSE) && CV == TRUE) {
      stop(strwrap(prefix = " ", initial = "",
                   "No MSE estimates in emdi object: arguments MSE and CV have to
                 be FALSE or a new emdi object with variance/MSE needs to be
                 generated."))
    }
    if ((ncol(object$ind) == 11) && any(indicator == "Custom" |
                                        indicator == "custom")) {
      stop(strwrap(prefix = " ", initial = "",
                   "No individual indicators are defined. Either select other
                 indicators or define custom indicators and generate a new emdi
                 object. See also help(ebp)."))
    }
    all_cv <- sqrt(object$MSE[, -1]) / object$ind[, -1]

  }

#_______________________________________________________________________________
  # Calculation of CVs
  if (inherits(object, "fh")) {
    object$MSE <- object$MSE[, c("Domain", "Direct", "FH")]
    object$ind <- object$ind[, c("Domain", "Direct", "FH")]

    all_cv <- sqrt(object$MSE[, -1]) / object$ind[, -1]
  }

  if (any(indicator == "Quantiles") || any(indicator == "quantiles")) {
    indicator <- c(
      indicator[!(indicator == "Quantiles" ||
        indicator == "quantiles")],
      "Quantile_10", "Quantile_25", "Median",
      "Quantile_75", "Quantile_90"
    )
  }
  if (any(indicator == "poverty") || any(indicator == "Poverty")) {
    indicator <- c(
      indicator[!(indicator == "poverty" ||
        indicator == "Poverty")],
      "Head_Count", "Poverty_Gap"
    )
  }
  if (any(indicator == "inequality") || any(indicator == "Inequality")) {
    indicator <- c(
      indicator[!(indicator == "inequality" ||
        indicator == "Inequality")],
      "Gini", "Quintile_Share"
    )
  }
#_______________________________Rachael added___________________________________
  if (inherits(object, "ebp_tf")) {
    if (any(indicator == "custom") || any(indicator == "Custom")) {
      indicator <- c(
        indicator[!(indicator == "custom" | indicator == "Custom")],
        colnames(object$ind_Domain[-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]),
        colnames(object$ind_Subdomain[-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)])
      )
    }
  }else{
    if (any(indicator == "custom") || any(indicator == "Custom")) {
      indicator <- c(
        indicator[!(indicator == "custom" | indicator == "Custom")],
        colnames(object$ind[-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)])
      )
    }
  }

  if(inherits(object, "fh_tf")){
    if (any(indicator == "all") || any(indicator == "All")) {
      ind_sub <- object$MSE_Subdomain
      ind_cv_sub <- cbind(Subomain = object$MSE_Subdomain[, 1], all_cv_Subdomain)
      ind_name_sub <- "All indicators"

      ind <- object$MSE_Domain
      ind_cv <- cbind(Domain = object$MSE_Domain[, 1], all_cv_Domain)
      ind_name <- "All indicators"
    } else if (any(indicator == "fh_tf") || any(indicator == "FH_tf") ||
               any(indicator == "FH_TF") || any(indicator == "fh_TF")) {
      ind_sub <- object$MSE_Subdomain[, c("Subdomain", "FH_TF")]
      ind_cv_sub <- cbind(Subdomain = object$MSE_Subdomain[, 1], all_cv_Subdomain)
      ind_name_sub <- "FH-TF estimates"

      ind <- object$MSE_Domain[, c("Domain", "FH_TF")]
      ind_cv <- cbind(Domain = object$MSE_Domain[, 1], all_cv_Domain)
      ind_name <- "FH-TF estimates"
    } else if (any(indicator == "Direct") || any(indicator == "direct")) {
      ind_sub <- object$MSE_Subdomain[, c("Subdomain", "Direct")]
      ind_cv_sub <- cbind(Subdomain = object$MSE_Subdomain[, 1], all_cv_Subdomain)
      ind_name_sub <- "Direct estimates used in FH-TF model"

      ind <- NULL
      ind_cv <- NULL
      ind_name <- NULL
    }
    if (CV == FALSE) {
      mse_emdi <- list(ind_Domain = ind, ind_Subdomain = ind_sub, ind_name = ind_name,
                       ind_name_sub = ind_name_sub)
    } else {
      mse_emdi <- list(ind_Domain = ind, ind_Subdomain = ind_sub,
                       ind_cv_Domain = ind_cv,
                       ind_cv_Subdomain = ind_cv_sub,ind_name = ind_name,
                       ind_name_sub = ind_name_sub)
    }
  }
  else if (inherits(object, "ebp_tf")){
    if (any(indicator == "all") || any(indicator == "All")) {
      ind_Domain <- object$MSE_Domain
      ind_cv_Domain <- cbind(Domain = object$MSE_Domain[, 1], all_cv_Domain)
      ind_Subdomain <- object$MSE_Subdomain
      ind_cv_Subdomain <- cbind(Domain = object$MSE_Subdomain[, 1], all_cv_Subdomain)
      ind_name <- "All indicators"
    }else {
      selection_dom <- colnames(object$MSE_Domain[-1]) %in% indicator
      selection_subdom <- colnames(object$MSE_Subdomain[-1]) %in% indicator
      ind_Domain <- object$MSE_Domain[, c(TRUE, selection_dom)]
      ind_Subdomain <- object$MSE_Subdomain[, c(TRUE, selection_subdom)]
      ind_cv_Domain <- data.frame(Domain = object$MSE_Domain[, 1],
                                  all_cv_Domain[, selection_dom])
      ind_cv_Subdomain <- data.frame(Domain = object$MSE_Subdomain[, 1],
                                  all_cv_Subdomain[, selection_subdom])
      colnames(ind_cv_Domain) <- colnames(ind_Domain)
      colnames(ind_cv_Subdomain) <- colnames(ind_Subdomain)
      ind_name <- paste(unique(indicator), collapse = ", ")
    }

    if (CV == FALSE) {
      mse_emdi <- list(ind_Domain = ind_Domain, ind_Subdomain = ind_Subdomain, ind_name = ind_name)
    } else {
      mse_emdi <- list(ind_Domain = ind_Domain, ind_Subdomain = ind_Subdomain,
                       ind_cv_Domain = ind_cv_Domain,
                       ind_cv_Subdomain = ind_cv_Subdomain,ind_name = ind_name)
    }
  } else{
    if (any(indicator == "all") || any(indicator == "All")) {
      ind <- object$MSE
      ind_cv <- cbind(Domain = object$MSE[, 1], all_cv)
      ind_name <- "All indicators"
    } else if (any(indicator == "fh") || any(indicator == "FH")) {
      ind <- object$MSE[, c("Domain", "FH")]
      ind_cv <- cbind(Domain = object$MSE[, 1], all_cv)
      ind_name <- "Fay-Herriot estimates"
    } else if (any(indicator == "Direct") || any(indicator == "direct")) {
      ind <- object$MSE[, c("Domain", "Direct")]
      ind_cv <- cbind(Domain = object$MSE[, 1], all_cv)
      ind_name <- "Direct estimates used in Fay-Herriot approach"
    } else {
      selection <- colnames(object$MSE[-1]) %in% indicator
      ind <- object$MSE[, c(TRUE, selection)]
      ind_cv <- data.frame(Domain = object$MSE[, 1], all_cv[, selection])
      colnames(ind_cv) <- colnames(ind)
      ind_name <- paste(unique(indicator), collapse = ", ")
    }

    if (CV == FALSE) {
      mse_emdi <- list(ind = ind, ind_name = ind_name)
    } else {
      mse_emdi <- list(ind = ind, ind_cv = ind_cv, ind_name = ind_name)
    }

  }



  class(mse_emdi) <- "mse.emdi"

  return(mse_emdi)
}
