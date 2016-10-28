#' Prints an emdiObject
#'
#' Basic information of an emdi object is printed.
#' @param x an x of type "emdi", representing point and MSE
#' estimates following the Empirical Best Prediction by
#' \cite{Molina and Rao (2010)}.
#' @param ... optional arguments passed to \code{\link{print.default}}.
# #' @param quote	not applicable.
# #' @param max.levels	not applicable.
# #' @param width not applicable.
# #' @param digits	not applicable.
# #' @param na.print	not applicable.
# #' @param zero.print	not applicable.
# #' @param justify not applicable.
# #' @param useSource not applicable.	
#' @seealso \code{\link{emdiObject}}, \code{\link{ebp}}
#' @export

print.emdi <- function(x, ...) {

  cat("Empirical Best Prediction\n")
  cat("\n")
  cat("Call:\n ")
  print(x$call)
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
    #                      data.frame(Transformation  = x$transformation,
    #                                 Method          = "NULL",
    #                                 Optimal_lambda  = "NULL",
    #                                 Shift_parameter = "NULL",
    #                                 row.names       = ""
    #                                 )
  }
  
  cat("\n")
  if(is.null(x$transform)){
    cat("Transformation: No transformation \n")
  } else {
    cat("Transformation:\n")
    print(x$transform)
  }
  cat("\n")
  cat("Model fit:\n")
  cat("For model fit lme methods are applicable to emdix$model \n")
  cat("where transformed_data equals smp_data transformed by function \n")
  cat("data_transformation using above given transformation and lambda \n")
  cat("and where fixed/list(fixed) equals ")
  print(x$fixed)
  cat("\n")

}




