#' @export
print.direct <- function(x, ...) {
  throw_class_error(x, "direct")

  cat("Direct estimation\n")
  cat("\n")
  cat("In-sample domains: ", x$framework$N_dom_smp, "\n")
  cat("\n")
  cat("Units in each Domain:")
  print(table(x$framework$smp_domains_vec))
}

#' @export
print.ebp <- function(x, ...) {
  throw_class_error(x, "ebp")

  if (is.null(x$call$weights)) {
    cat("Empirical Best Prediction\n")
  } else {
    cat("Empirical Best Prediction with sampling weights\n")
  }

  cat("\n")
  cat("Out-of-sample domains: ", x$framework$N_dom_unobs, "\n")
  cat("In-sample domains: ", x$framework$N_dom_smp, "\n")

  if (x$transformation == "box.cox") {
    transform_method <- data.frame(
      Transformation = x$transformation,
      Method = x$method,
      Optimal_lambda =
        x$transform_param$optimal_lambda,
      Shift_parameter =
        round(x$transform_param$shift_par, 3),
      row.names = ""
    )
  } else if (x$transformation == "dual") {
    transform_method <- data.frame(
      Transformation = x$transformation,
      Method = x$method,
      Optimal_lambda =
        x$transform_param$optimal_lambda,
      row.names = ""
    )
  } else if (x$transformation == "log") {
    transform_method <- data.frame(
      Transformation = x$transformation,
      Shift_parameter =
        round(x$transform_param$shift_par, 3),
      row.names = ""
    )
  } else if (x$transformation == "no") {
    transform_method <- NULL
  }

  cat("\n")
  if (is.null(transform_method)) {
    cat("Transformation: No transformation \n")
  } else {
    cat("Transformation:\n")
    print(transform_method)
  }
  cat("\n")
  cat("Model fit:\n")
  cat("For model fit lme methods are applicable to emdiObject$model \n")
  cat("where transformed_data equals smp_data transformed by function \n")
  cat("data_transformation using above given transformation and lambda \n")
  cat("and where fixed/list(fixed) equals ")
  print(x$fixed)
  cat("\n")
}


#' @export
print.fh <- function(x, ...) {
  throw_class_error(x, "fh")
  cat("Empirical Best Linear Unbiased Prediction (Fay-Herriot)\n")
  cat("\n")
  cat("Out-of-sample domains: ", x$framework$N_dom_unobs, "\n")
  if (x$model$correlation == "temporal" ||
    x$model$correlation == "spatio-temporal") {
    cat("In-sample domains: ", x$framework$N_dom_smp / x$model$n_time, "\n")
    cat("Number of time periods: ", x$model$n_time, "\n")
    cat("\n")
  } else {
    cat("In-sample domains: ", x$framework$N_dom_smp, "\n")
    cat("\n")
  }
  cat("Variance and MSE estimation:\n")
  if (x$method$method == "reblup" || x$method$method == "reblupbc") {
    cat("Variance estimation method: robustified ml,", x$method$method, "\n")

    if (x$method$method == "reblup") {
      cat("k = ", x$model$k, "\n")
    } else if (x$method$method == "reblupbc") {
      cat("k = ", x$model$k, ", mult_constant = ", x$model$mult_constant, "\n")
    }
  } else {
    cat("Variance estimation method: ", x$method$method, "\n")
  }

  if (x$model$correlation == "no") {
    cat("Variance of random effects: ", x$model$variance, "\n")
  } else {
    cat(
      "Estimated variance component(s): ", x$model$correlation,
      "correlation assumed\n"
    )
    cat("Variance of random effects: ", x$model$variance$variance, "\n")
    cat("Spatial correlation parameter: ", x$model$variance$correlation, "\n")
  }
  cat("MSE method: ", x$method$MSE_method, "\n")
  cat("\n")
  if (x$transformation$transformation == "no") {
    transform_data <- NULL
  } else {
    if (x$transformation$backtransformation == "sm") {
      backtransformation <- "slud-maiti"
    } else {
      backtransformation <- x$transformation$backtransformation
    }
    transform_data <- data.frame(
      Transformation =
        x$transformation$transformation,
      Back_transformation = backtransformation,
      row.names = ""
    )
  }
  if (is.null(transform_data)) {
    cat("Transformation: No transformation \n")
  } else {
    cat("Transformation:\n")
    print(transform_data)
  }
}
