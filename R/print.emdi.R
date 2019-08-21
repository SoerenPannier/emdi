#' Prints an emdiObject
#'
#' Basic information of an emdi object is printed.
#' @param x an x of type "emdi", representing point and MSE
#' estimates obtained by direct estimation (see also \code{\link{direct}}),
#' the Fay-Herriot model and a range of extensions (see also \code{\link{fh}}),
#' or Empirical Best Prediction (see also \code{\link{ebp}}).
#' @param ... optional arguments passed to \code{\link{print.default}}.
#' @seealso \code{\link{emdiObject}}, \code{\link{fh}}, \code{\link{ebp}}
#' @export

print.emdi <- function(x, ...) {

  if(!inherits(x, "emdi")) {
    stop('First object needs to be of class emdi.')
  }

  if(inherits(x, "ebp")) {
    cat("Empirical Best Prediction\n")
    cat("\n")
    cat("Out-of-sample domains: ", x$framework$N_dom_unobs, "\n")
    cat("In-sample domains: ", x$framework$N_dom_smp, "\n")

    if (x$transformation == "box.cox") {
      transform_method <- data.frame(Transformation  = x$transformation,
                                     Method          = x$method,
                                     Optimal_lambda  = x$transform_param$optimal_lambda,
                                     Shift_parameter = round(x$transform_param$shift_par,3),
                                     row.names       = ""
      )
    } else if (x$transformation == "log") {
      transform_method <- data.frame(Transformation  = x$transformation,
                                     Shift_parameter = round(x$transform_param$shift_par,3),
                                     row.names       = ""
      )
    }
    else if (x$transformation == "no") {
      transform_method <- NULL
    }

    cat("\n")
    if(is.null(transform_method)){
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
  } else if(inherits(x, "direct")) {
    cat("Direct estimation\n")
    cat("\n")
    cat("In-sample domains: ", x$framework$N_dom_smp, "\n")
    cat("\n")
    cat("Units in each Domain:")
    print(table(x$framework$smp_domains_vec))
  } else if (inherits(x, "fh")) {
      cat("Empirical Best Linear Unbiased Prediction (Fay-Herriot)\n")
      cat("\n")
      cat("Out-of-sample domains: ", x$framework$N_dom_unobs, "\n")
      cat("In-sample domains: ", x$framework$N_dom_smp, "\n")
      cat("\n")
      cat("Variance and MSE estimation:\n")
      cat("Variance estimation method: ", x$method$method,
          "\n")
      cat("Estimated variance of random effects: ", x$model$sigmau2,
          "\n")
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
        transform_data <- data.frame(Transformation  = x$transformation$transformation,
                                     Back_transformation = backtransformation,
                                     row.names       = ""
        )
      }
      if(is.null(transform_data)){
        cat("Transformation: No transformation \n")
      } else {
        cat("Transformation:\n")
        print(transform_data)
      }
    }
}





