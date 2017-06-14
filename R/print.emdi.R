#' Prints an emdiObject
#'
#' Basic information of an emdi object is printed.
#' @param x an x of type "emdi", representing point and MSE
#' estimates obtained by direct estimation (see also \code{\link{direct}})
#' or Empirical Best Prediction (see also \code{\link{ebp}}). 
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

  if(!inherits(x, "emdi")) {
    stop('First object needs to be of class emdi.')
  }
  
  if(inherits(x, "model")) {
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
      #                      data.frame(Transformation  = x$transformation,
      #                                 Method          = "NULL",
      #                                 Optimal_lambda  = "NULL",
      #                                 Shift_parameter = "NULL",
      #                                 row.names       = ""
      #                                 )
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
  }
  if(inherits(x, "direct")) {
    cat("Direct estimation\n")
    cat("\n")
    cat("In-sample domains: ", x$framework$N_dom_smp, "\n")
    cat("\n")
    cat("Units in each Domain:")
    print(table(x$framework$smp_domains_vec))
  }
  

}




