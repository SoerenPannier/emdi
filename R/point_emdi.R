point_emdi <- function(object, indicator = "all") {
  if (is.null(object$ind)) {
    stop("No estimates in object: method point not applicable")
  }
  if ((ncol(object$ind) == 11) && any(indicator == "Custom" |
    indicator == "custom")) {
    stop("No individual indicators are defined. Either select other indicators
         or define custom indicators and generate a new emdi object. See also
         help(direct) or help(ebp).")
  }

  if (any(indicator == "Quantiles") || any(indicator == "quantiles")) {
    indicator <- c(
      indicator[!(indicator == "Quantiles" | indicator == "quantiles")],
      "Quantile_10", "Quantile_25", "Median", "Quantile_75", "Quantile_90"
    )
  }
  if (any(indicator == "poverty") || any(indicator == "Poverty")) {
    indicator <- c(
      indicator[!(indicator == "poverty" |
        indicator == "Poverty")],
      "Head_Count", "Poverty_Gap"
    )
  }

  if (any(indicator == "inequality") || any(indicator == "Inequality")) {
    indicator <- c(
      indicator[!(indicator == "inequality" | indicator == "Inequality")],
      "Gini", "Quintile_Share"
    )
  }

  if (any(indicator == "custom") || any(indicator == "Custom")) {
    indicator <- c(
      indicator[!(indicator == "custom" | indicator == "Custom")],
      colnames(object$ind[-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)])
    )
  }

  if (inherits(object, "fh")) {
    object$ind["Out"] <- NULL
  }



  if (any(indicator == "all") || any(indicator == "All")) {
    ind <- object$ind
    ind_name <- "All indicators"
  } else if (any(indicator == "fh") || any(indicator == "FH")) {
    ind <- object$ind[, c("Domain", "FH")]
    ind_name <- "Fay-Herriot estimates"
  } else if (any(indicator == "fh_bench") || any(indicator == "FH_Bench")) {
    ind <- object$ind[, c("Domain", "FH_Bench")]
    ind_name <- "Benchmarked Fay-Herriot estimates"
  } else if (any(indicator == "Direct") || any(indicator == "direct")) {
    ind <- object$ind[, c("Domain", "Direct")]
    ind_name <- "Direct estimates used in Fay-Herriot approach"
  } else {
    selection <- colnames(object$ind[-1]) %in% indicator
    ind <- object$ind[, c(TRUE, selection)]
    ind_name <- paste(unique(indicator), collapse = ", ")
  }

  point_emdi <- list(ind = ind, ind_name = ind_name)
  class(point_emdi) <- "point.emdi"

  return(point_emdi)
}
