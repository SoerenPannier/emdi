#' @rdname write.excel
#' @importFrom readODS write_ods
#' @export

write.ods <- function(object,
                      file = "ods_output.ods",
                      indicator = "all",
                      MSE = FALSE,
                      CV = FALSE,
                      split = FALSE) {
  writeods_check(
    object = object,
    file = file,
    split = split
  )

  wb <- gsub(".ods", "", file)

  if (inherits(object, "direct")) {
    add_summary_direct_ods(
      object = object,
      wb = wb
    )
  } else if (inherits(object, "ebp")) {
    add_summary_ods_ebp(object = object, wb = wb)
  } else if (inherits(object, "fh")) {
    add_summary_ods_fh(object = object, wb = wb)
  }

  if (!split && (MSE || CV)) {
    add_estims_ods(
      object = object,
      indicator = indicator,
      wb = wb,
      MSE = MSE,
      CV = CV
    )
  } else {
    add_pointests_ods(
      wb = wb,
      object = object,
      indicator = indicator
    )
    if (MSE || CV) {
      add_precisions_ods(
        object = object,
        indicator = indicator,
        MSE = MSE,
        wb = wb,
        CV = CV
      )
    }
  }
}

add_summary_ods_ebp <- function(object, wb, headlines_cs) {
  su <- summary(object)

  df_nobs <- data.frame(Count = c(
    su$out_of_smp,
    su$in_smp, su$size_pop,
    su$size_smp
  ))
  rownames(df_nobs) <- c(
    "out of sample domains",
    "in sample domains",
    "out of sample observations",
    "in sample observations"
  )
  df_nobs <- cbind.data.frame(rownames(df_nobs), df_nobs)
  readODS::write_ods(x = df_nobs, path = paste0(wb, "_sumObs", ".ods"))

  df_size_dom <- as.data.frame(su$size_dom)
  df_size_dom <- cbind.data.frame(rownames(df_size_dom), df_size_dom)
  readODS::write_ods(x = df_size_dom, path = paste0(wb, "_sumDom", ".ods"))

  if (!is.null(su$transform)) {
    readODS::write_ods(
      x = su$transform,
      path = paste0(wb, "_sumTrafo", ".ods")
    )
  }
  su$normality <- cbind.data.frame(rownames(su$normality), su$normality)
  readODS::write_ods(x = su$normality, path = paste0(wb, "_sumNorm", ".ods"))

  su$coeff_determ <- cbind.data.frame(
    "Coefficients of determination",
    su$coeff_determ
  )
  readODS::write_ods(x = su$coeff_determ, path = paste0(
    wb, "_sumCoefDet",
    ".ods"
  ))

  return(NULL)
}

add_summary_ods_fh <- function(object, wb, headlines_cs) {
  su <- summary(object)

  df_nobs <- data.frame(Count = c(
    su$out_of_smp,
    su$in_smp
  ))
  rownames(df_nobs) <- c(
    "out of sample domains",
    "in sample domains"
  )

  df_nobs <- cbind.data.frame(rownames(df_nobs), df_nobs)
  readODS::write_ods(x = df_nobs, path = paste0(wb, "_sumObs", ".ods"))

  if (su$model$correlation == "no") {
    estimMethods <- data.frame(su$method$method, su$model$variance,
      su$method$MSE_method,
      row.names = ""
    )
    names(estimMethods) <- c(
      "Variance estimation", "Estimated variance",
      "MSE estimation"
    )
    if (su$method$method == "reblup") {
      estimMethods$k <- su$model$k
      estimMethods <- estimMethods[, c(
        "Variance estimation", "Estimated variance",
        "k", "MSE estimation"
      )]
    } else if (su$method$method == "reblupbc") {
      estimMethods$k <- su$model$k
      estimMethods$c <- su$model$c
      estimMethods <- estimMethods[, c(
        "Variance estimation", "Estimated variance",
        "k", "c", "MSE estimation"
      )]
    }
  } else if (su$model$correlation == "spatial") {
    estimMethods <- data.frame(su$method$method, su$model$variance["variance"],
      su$model$variance["correlation"], su$method$MSE_method,
      row.names = ""
    )
    names(estimMethods) <- c(
      "Variance estimation", "Estimated variance",
      "Spatial correlation", "MSE estimation"
    )
    if (su$method$method == "reblup") {
      estimMethods$k <- su$model$k
      estimMethods <- estimMethods[, c(
        "Variance estimation", "Estimated variance",
        "k", "Spatial correlation", "MSE estimation"
      )]
    } else if (su$method$method == "reblupbc") {
      estimMethods$k <- su$model$k
      estimMethods$mult_constant <- su$model$mult_constant
      estimMethods <- estimMethods[, c(
        "Variance estimation", "Estimated variance",
        "k", "mult_constant", "Spatial correlation", "MSE estimation"
      )]
    }
  }
  readODS::write_ods(
    x = estimMethods,
    path = paste0(wb, "_sumEstimMethods", ".ods")
  )

  if (!is.null(su$transform)) {
    readODS::write_ods(
      x = su$transform,
      path = paste0(wb, "_sumTrafo", ".ods")
    )
  }
  su$normality <- cbind.data.frame(rownames(su$normality), su$normality)
  readODS::write_ods(x = su$normality, path = paste0(wb, "_sumNorm", ".ods"))

  if (su$model$correlation == "no" && !(su$method$method %in% c(
    "reblup",
    "reblupbc"
  ) |
    su$method$method == "me")) {
    readODS::write_ods(
      x = su$model$model_select,
      path = paste0(wb, "_sumModelSelect", ".ods")
    )
  }

  return(NULL)
}


add_summary_direct_ods <- function(object, wb, headlines_cs) {
  su <- summary(object)

  df_nobs <- data.frame(Count = c(su$in_smp, su$size_smp))
  rownames(df_nobs) <- c(
    "in sample domains",
    "in sample observations"
  )
  df_nobs <- cbind.data.frame(rownames(df_nobs), df_nobs)
  readODS::write_ods(x = df_nobs, path = paste0(wb, "_sumObs", ".ods"))

  df_size_dom <- as.data.frame(su$size_dom)
  df_size_dom <- cbind.data.frame(rownames(df_size_dom), df_size_dom)
  readODS::write_ods(x = df_size_dom, path = paste0(wb, "_sumDom", ".ods"))

  df_smp_sizes <- as.data.frame(su$smp_size_tab)
  colnames(df_smp_sizes) <- c("Domain", "Frequency")
  df_smp_sizes <- cbind.data.frame(rownames(df_smp_sizes), df_smp_sizes)
  readODS::write_ods(
    x = df_smp_sizes,
    path = paste0(wb, "_sumSmpsize", ".ods")
  )

  return(NULL)
}

add_pointests_ods <- function(object, indicator, wb, headlines_cs) {
  if (is.null(indicator) || !all(indicator == "all" | indicator == "Quantiles" |
    indicator == "quantiles" |
    indicator == "Poverty" | indicator == "poverty" |
    indicator == "Inequality" | indicator == "inequality" |
    indicator == "Custom" | indicator == "custom" |
    indicator %in% names(object$ind[-1]))) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0("The argument indicator is set to ", indicator, ". The
                        argument only allows to be set to all, a name of
                        estimated indicators or indicator groups as described
                        in help(estimators.emdi).")))
  }

  data <- point_emdi(object = object, indicator = indicator)$ind
  data[, 1] <- iconv(x = data[, 1], from = "", to = "UTF-8")
  readODS::write_ods(x = data, path = paste0(wb, "_pointEstim", ".ods"))

  return(NULL)
}


add_precisions_ods <- function(object, indicator, MSE, wb, headlines_cs, CV) {
  precisions <- mse_emdi(object = object, indicator = indicator, CV = TRUE)
  if (MSE) {
    precisions$ind[, 1] <-
      iconv(x <- precisions$ind[, 1], from = "", to = "UTF-8")
    readODS::write_ods(
      x = precisions$ind,
      path = paste0(wb, "_precMSE", ".ods")
    )
  }
  if (CV) {
    precisions$ind_cv[, 1] <-
      iconv(x <- precisions$ind_cv[, 1], from = "", to = "UTF-8")
    readODS::write_ods(
      x = precisions$ind_cv,
      path = paste0(wb, "_precCV", ".ods")
    )
  }
  return(NULL)
}


add_estims_ods <- function(object, indicator, wb, headlines_cs, MSE, CV) {
  data <- estimators(
    object = object, indicator = indicator,
    MSE = MSE, CV = CV
  )$ind
  data[, 1] <-
    iconv(x <- data[, 1], from = "", to = "UTF-8")

  readODS::write_ods(x = data, path = paste0(wb, "_estim", ".ods"))
  return(NULL)
}
