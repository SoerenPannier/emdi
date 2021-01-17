#' Summarizes an emdi direct object
#'
#' Additional information about the data and model in small area estimation
#' methods and components of an emdi object are extracted. The returned object
#' is suitable for printing  with the \code{print.summary.direct} method.
#' @param object an object of type "emdi", representing point and MSE
#' estimates. 
#' @param ... additional arguments that are not used in this method.
#' @return an object of type "summary.direct" with information about the 
#' sample and population data, the usage of transformation, normality 
#' tests and information of the model fit.
#' @examples
#' \donttest{
#' # Loading sample data
#' data("eusilcA_smp")
#'
#' # Generation of the emdi object
#' emdi_direct <- direct(y = "eqIncome", smp_data = eusilcA_smp, 
#' smp_domains = "district", weights = "weight", threshold = 11064.82, var = TRUE, 
#' boot_type = "naive", B = 50, seed = 123, X_calib = NULL, totals = NULL, 
#' na.rm = TRUE)
#' 
#' # Example: Receive first overview
#' summary(emdi_direct)
#' 
#' }
#' @export
#' @importFrom moments skewness kurtosis
#' @importFrom MuMIn r.squaredGLMM

summary.direct <- function(object, ...) {
  throw_class_error(object, "direct")
  
  call_emdi <- object$call
  
  N_dom_smp <-   object$framework$N_dom_smp
  
  smp_size <- object$framework$N_smp
  
  smp_size_tab <- table(object$framework$smp_domains_vec)
  
  smp_size_dom <-
    rbind(Sample_domains = summary(as.numeric(smp_size_tab)))
  
  sum_emdi <- list(out_of_smp   = NULL,
                   in_smp       = N_dom_smp,
                   size_smp     = smp_size,
                   size_pop     = NULL,
                   size_dom     = smp_size_dom,
                   smp_size_tab = smp_size_tab,
                   transform    = NULL,
                   normality    = NULL,
                   icc          = NULL,
                   coeff_determ = NULL,
                   model        = NULL,
                   call         = call_emdi
  )

  class(sum_emdi) <- c("summary.direct", "emdi")
  sum_emdi
}


#' @export
print.summary.direct <- function(x,...) {
  throw_class_error(x, "direct")
  
  cat("Direct estimation\n")
  cat("\n")
  cat("Call:\n ")
  print(x$call)
  cat("\n")
  cat("In-sample domains: ", x$in_smp, "\n")
  cat("\n")
  cat("Sample sizes:\n")
  cat("Units in sample: ", x$size_smp, "\n")
  print(x$size_dom)
  cat("\n")
  cat("Units in each Domain:")
  print(x$smp_size_tab)
}