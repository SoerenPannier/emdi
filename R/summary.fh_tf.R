# Summarizes an emdi fh_tf Object

#' @export
#' @importFrom moments skewness kurtosis
#' @importFrom MuMIn r.squaredGLMM
#' @rdname emdi_summaries

summary.fh_tf <- function(object, ...) {
  throw_class_error(object, "fh_tf")

  # Normality checks for residuals
  skewness_res <- skewness(object$model$std_real_resid, na.rm = TRUE)
  kurtosis_res <- kurtosis(object$model$std_real_resid, na.rm = TRUE)
  if (length(object$model$std_real_resid) >= 3 &&
    length(object$model$std_real_resid) < 5000) {
    shapiro_res_W <- shapiro.test(object$model$std_real_resid)[[1]]
    shapiro_res_p <- shapiro.test(object$model$std_real_resid)[[2]]
  } else {
    warning(strwrap(prefix = " ", initial = "",
                    "Number of subdomains must be between 3 and 5000, otherwise
                    the Shapiro-Wilk test is not applicable."))
    shapiro_res_W <- NA
    shapiro_res_p <- NA
  }

  # Normality checks for random effects at domain level
  skewness_random_Dom <- skewness(object$model$random_effects_Domain, na.rm = TRUE)
  kurtosis_random_Dom <- kurtosis(object$model$random_effects_Domain, na.rm = TRUE)
  if (length(object$model$random_effects_Domain) >= 3 &&
    length(object$model$random_effects_Domain) < 5000) {
    shapiro_random_W_Dom <- shapiro.test(object$model$random_effects_Domain)[[1]]
    shapiro_random_p_Dom <- shapiro.test(object$model$random_effects_Domain)[[2]]
  } else {
    shapiro_random_W_Dom <- NA
    shapiro_random_p_Dom <- NA
  }

  # Normality checks for random effects at subdomain level
  skewness_random_Sub <- skewness(object$model$random_effects_Subdomain, na.rm = TRUE)
  kurtosis_random_Sub <- kurtosis(object$model$random_effects_Subdomain, na.rm = TRUE)
  if (length(object$model$random_effects_Subdomain) >= 3 &&
      length(object$model$random_effects_Subdomain) < 5000) {
    shapiro_random_W_Sub <- shapiro.test(object$model$random_effects_Subdomain)[[1]]
    shapiro_random_p_Sub <- shapiro.test(object$model$random_effects_Subdomain)[[2]]
  } else {
    warning(strwrap(prefix = " ", initial = "",
                    "Number of domains must be between 3 and 5000, otherwise
                    the Shapiro-Wilk test is not applicable."))
    shapiro_random_W_Sub <- NA
    shapiro_random_p_Sub <- NA
  }

  normality <- data.frame(
    Skewness = c(skewness_res, skewness_random_Dom, skewness_random_Sub),
    Kurtosis = c(kurtosis_res, kurtosis_random_Dom, kurtosis_random_Sub),
    Shapiro_W = c(
      shapiro_res_W,
      shapiro_random_W_Dom,
      shapiro_random_W_Sub
    ),
    Shapiro_p = c(
      shapiro_res_p,
      shapiro_random_p_Dom,
      shapiro_random_p_Sub
    ),
    row.names = c(
      "Standardized_residuals",
      "Random_effects_domain",
      "Random_effects_subdomain"
    )
  )

  sum_emdi <- list(
    out_of_smp_subdomain = object$framework$N_out_sub,
    in_smp_subdomain = object$framework$N_in_sub,
    out_of_smp_domain = object$framework$N_out_dom,
    in_smp_domain = object$framework$N_in_dom,
    normality = normality,
    R2 = object$R2,
    transformation = object$transformation,
    model = object$model,
    call = object$call
  )

  class(sum_emdi) <- c("summary.fh_tf", "emdi")
  sum_emdi
}


#' @export
#' @importFrom stats printCoefmat
print.summary.fh_tf <- function(x, ...) {
  throw_class_error(x, "fh_tf")
  cat("Call:\n ")
  print(x$call)
  cat("\n")

  cat("Out-of-sample domains: ", x$out_of_smp_domain, "\n")
  cat("In-sample domains: ", x$in_smp_domain, "\n")
  cat("\n")
  cat("Out-of-sample subdomains: ", x$out_of_smp_subdomain, "\n")
  cat("In-sample subdomains: ", x$in_smp_subdomain, "\n")
  cat("\n")

  cat("Estimated variance component(s):\n")
  print(x$model$variances)
  cat("\n")

  cat("Coefficients:\n")
  printCoefmat(as.matrix(x$model$coefficients), has.Pvalue = TRUE)
  cat("\n")

  cat("R squared:\n")
  print(x$R2)
  cat("\n")

  cat("Residual diagnostics:\n")
  print(x$normality)
  cat("\n")

  if (x$transformation == "no") {
    cat("Transformation: No transformation \n")
  } else {
    cat("Transformation: ", x$transformation, "\n")
  }
}
