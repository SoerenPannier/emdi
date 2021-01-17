# Summarizes an emdi direct object

#' @export
#' @importFrom moments skewness kurtosis
#' @importFrom MuMIn r.squaredGLMM
#' @rdname emdi_summaries

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