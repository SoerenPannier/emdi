#' Summarizes an emdi fh object
#'
#' Additional information about the data and model in small area estimation
#' methods and components of an emdi object are extracted. The returned object
#' is suitable for printing  with the \code{print.summary.fh} method.
#' @param object an object of type "fh", representing point and MSE
#' estimates. 
#' @param ... additional arguments that are not used in this method.
#' @return an object of type "summary.fh" with information about the 
#' sample and population data, the usage of transformation, normality 
#' tests and information of the model fit.
#' @references 
#' Lahiri, P. and Suntornchost, J. (2015), Variable selection for linear mixed
#' models with applications in small area estimation, The Indian Journal of 
#' Statistics 77-B(2), 312-320. \cr \cr
#' Marhuenda, Y., Morales, D. and Pardo, M.C. (2014). Information criteria for 
#' Fay-Herriot model selection. Computational Statistics and Data Analysis 70, 
#' 268-280. \cr \cr
#' Nakagawa S, Schielzeth H (2013). A general and simple method for obtaining R2 
#' from generalized linear mixed-effects models. Methods in Ecology and Evolution, 
#' 4(2), 133-142.
#' @seealso \code{\link{emdiObject}}, \code{\link{direct}}, \code{\link{ebp}},
#' \code{\link{fh}}, \code{\link[MuMIn]{r.squaredGLMM}}, \code{\link[moments]{skewness}},
#' \code{\link[moments]{kurtosis}}, \code{\link[stats]{shapiro.test}}
#' @examples
#' \donttest{
#' # Example for models of type fh
#' 
#' # Loading data - population and sample data
#' data("eusilcA_popAgg")
#' data("eusilcA_smpAgg")
#'
#' # Combine sample and population data
#' combined_data <- combine_data(pop_data = eusilcA_popAgg, pop_domains = "Domain",
#'                               smp_data = eusilcA_smpAgg, smp_domains = "Domain")
#'
#' # Generation of the emdi object
#' fh_std <- fh(fixed = Mean ~ cash + self_empl, vardir = "Var_Mean",
#'              combined_data = combined_data, domains = "Domain", method = "ml", 
#'              MSE = TRUE)
#'              
#' # Example: Receive first overview
#' summary(fh_std)
#' }
#' @export
#' @importFrom moments skewness kurtosis
#' @importFrom MuMIn r.squaredGLMM

summary.fh <- function(object, ...) {
  throw_class_error(object, "fh")

  # Normaly checks for standardized realized residuals
  skewness_stdres <- skewness(object$model$std_real_residuals, na.rm = TRUE)
  kurtosis_stdres <- kurtosis(object$model$std_real_residuals, na.rm = TRUE)
  if (length(object$model$std_real_residuals) >= 3 & length(object$model$std_real_residuals) <
      5000) {
    shapiro_stdres_W <- shapiro.test(object$model$std_real_residuals)[[1]]
    shapiro_stdres_p <- shapiro.test(object$model$std_real_residuals)[[2]]
  }
  else {
    warning("Number of domains must be between 3 and 5000, otherwise the\n Shapiro-Wilk test is not applicable.")
    shapiro_stdres_W <- NA
    shapiro_stdres_p <- NA
  }
  
  # Normality checks for random effects
  skewness_random <- skewness(object$model$random_effects, na.rm = TRUE)
  kurtosis_random <- kurtosis(object$model$random_effects, na.rm = TRUE)
  if (length(object$model$random_effects) >= 3 & length(object$model$random_effects) <
      5000) {
    shapiro_random_W <- shapiro.test(object$model$random_effects)[[1]]
    shapiro_random_p <- shapiro.test(object$model$random_effects)[[2]]
  }
  else {
    shapiro_random_W <- NA
    shapiro_random_p <- NA
  }
  
  normality <- data.frame(Skewness = c(skewness_stdres, skewness_random),
                          Kurtosis = c(kurtosis_stdres, kurtosis_random),
                          Shapiro_W = c(shapiro_stdres_W,
                                        shapiro_random_W),
                          Shapiro_p = c(shapiro_stdres_p,
                                        shapiro_random_p),
                          row.names = c("Standardized_Residuals",
                                        "Random_effects"))
  
  if (object$transformation$transformation == "no") {
    transform_data <- NULL
  } else {
    if (object$transformation$backtransformation == "sm") {
      backtransformation <- "slud-maiti"
    } else {
      backtransformation <- object$transformation$backtransformation
    }
    transform_data <- data.frame(Transformation  = object$transformation$transformation,
                                 Back_transformation = backtransformation,
                                 row.names       = ""
    )
  }
  
  sum_emdi <- list(out_of_smp = object$framework$N_dom_unobs,
                   in_smp = object$framework$N_dom_smp,
                   size_smp     = NULL,
                   size_pop     = NULL,
                   size_dom     = NULL,
                   smp_size_tab = NULL,
                   transform    = transform_data,
                   normality    = normality,
                   icc          = NULL,
                   coeff_determ = NULL,
                   model        = object$model,
                   method       = object$method,
                   call = object$call)

  class(sum_emdi) <- c("summary.fh", "emdi")
  sum_emdi
}


#' @export
print.summary.fh <- function(x, ...) {
  throw_class_error(x, "fh")
  cat("Call:\n ")
  print(x$call)
  cat("\n")
  cat("Out-of-sample domains: ", x$out_of_smp, "\n")
  cat("In-sample domains: ", x$in_smp, "\n")
  cat("\n")
  cat("Variance and MSE estimation:\n")
  if (x$method$method == "reblup" | x$method$method == "reblupbc") {
    cat("Variance estimation method: robustified ml,", x$method$method, "\n")
    
    if (x$method$method == "reblup") {
      cat("k = ", x$model$k, "\n")
    } else if (x$method$method == "reblupbc") {
      cat("k = ", x$model$k, ", c = ", x$model$c, "\n")
    }
    
  } else {
    cat("Variance estimation method: ", x$method$method, "\n")
  }
  if (x$model$correlation == "no") {
    cat("Estimated variance component(s): ", x$model$variance, "\n")
  } else {
    cat("Estimated variance component(s): ", x$model$correlation, "correlation assumed\n")
    print(x$model$variance) 
  }
  cat("MSE method: ", x$method$MSE_method, "\n")
  cat("\n")
  cat("Coefficients:\n")
  printCoefmat(as.matrix(x$model$coefficients), has.Pvalue = TRUE)

  cat("\n")
  cat("Explanatory measures:\n")
  if (is.null(x$model$model_select)) {
    cat("No explanatory measures provided \n")
  } else {
    print(x$model$model_select)
  }
  cat("\n")
  cat("Residual diagnostics:\n")
  print(x$normality)
  cat("\n")
  if (is.null(x$transform)) {
    cat("Transformation: No transformation \n")
  } else {
    cat("Transformation:\n")
    print(x$transform)
  }
}