# Summarizes an emdi ebp_tf Object

#' @export
#' @importFrom moments skewness kurtosis
#' @importFrom MuMIn r.squaredGLMM
#' @rdname emdi_summaries

summary.ebp_tf <- function(object, ...) {
  throw_class_error(object, "ebp_tf")

  call_emdi <- object$call

  #___________________________domains __________________________________________
  N_dom_unobs <- object$framework$N_dom_unobs
  N_dom_smp <- object$framework$N_dom_smp
  smp_size <- object$framework$N_smp
  pop_size <- object$framework$N_pop
  smp_size_dom <- summary(as.data.frame(
    table(object$framework$smp_domains_vec)
  )[, "Freq"])
  pop_size_dom <- summary(as.data.frame(
    table(object$framework$pop_domains_vec)
  )[, "Freq"])
  sizedom_smp_pop <- rbind(
    Sample_domains = smp_size_dom,
    Population_domains = pop_size_dom
  )
  #____________________________subdomains_______________________________________

  N_subdom_unobs <- object$framework$N_subdom_unobs
  N_subdom_smp <- object$framework$N_subdom_smp

  smp_size_subdom <- summary(as.data.frame(
    table(object$framework$smp_subdomains_vec)
  )[, "Freq"])
  pop_size_subdom <- summary(as.data.frame(
    table(object$framework$pop_subdomains_vec)
  )[, "Freq"])
  sizesubdom_smp_pop <- rbind(
    Sample_subdomains = smp_size_subdom,
    Population_subdomains = pop_size_subdom
  )

  if (object$transformation == "box.cox" || object$transformation == "dual") {
    transform_method <- data.frame(
      Transformation = object$transformation,
      Method = object$method,
      Optimal_lambda =
        object$transform_param$optimal_lambda,
      Shift_parameter =
        round(object$transform_param$shift_par, 3),
      row.names = ""
    )
  } else if (object$transformation == "log.shift") {
    transform_method <- data.frame(
      Transformation = object$transformation,
      Method = object$method,
      Optimal_lambda =
        object$transform_param$optimal_lambda,
      row.names = ""
    )
  } else if (object$transformation == "log") {
    transform_method <- data.frame(
      Transformation = object$transformation,
      Shift_parameter =
        round(object$transform_param$shift_par, 3),
      row.names = ""
    )
  } else if (object$transformation == "no") {
    transform_method <- NULL
  }

  skewness_res <- skewness(residuals(object$model,
    level = 0,
    type = "pearson"
  ))
  kurtosis_res <- kurtosis(residuals(object$model,
    level = 0,
    type = "pearson"
  ))

  skewness_ran_dom <- skewness(ranef(object$model)[[1]]$"(Intercept)")
  kurtosis_ran_dom <- kurtosis(ranef(object$model)[[1]]$"(Intercept)")

  skewness_ran_subdom <- skewness(ranef(object$model)[[2]]$"(Intercept)")
  kurtosis_ran_subdom <- kurtosis(ranef(object$model)[[2]]$"(Intercept)")

  if (length(residuals(object$model, level = 0, type = "pearson")) > 3 &&
    length(residuals(object$model, level = 0, type = "pearson")) < 5000) {
    shapiro_p_res <-
      shapiro.test(residuals(object$model, level = 0, type = "pearson"))[[2]]
    shapiro_W_res <-
      shapiro.test(residuals(object$model, level = 0, type = "pearson"))[[1]]
  } else {
    warning(strwrap(prefix = " ", initial = "",
                    "Number of observations exceeds 5000 or is lower then 3 and
                    thus the Shapiro-Wilk test is not applicable for the
                    residuals."))
    shapiro_p_res <- NA
    shapiro_W_res <- NA
  }

  if (length(ranef(object$model)[[1]]$"(Intercept)") > 3 &&
    length(ranef(object$model)[[1]]$"(Intercept)") < 5000) {
    shapiro_p_ran_dom <- shapiro.test(ranef(object$model)[[1]]$"(Intercept)")[[2]]
    shapiro_W_ran_dom <- shapiro.test(ranef(object$model)[[1]]$"(Intercept)")[[1]]
  } else {
    warning(strwrap(prefix = " ", initial = "",
                    "Number of domains exceeds 5000 or is lower then 3 and thus
                    the Shapiro-Wilk test is not applicable for the random
                    effects."))
    shapiro_p_ran_dom <- NA
    shapiro_W_ran_dom <- NA
  }
  if (length(ranef(object$model)[[2]]$"(Intercept)") > 3 &&
      length(ranef(object$model)[[2]]$"(Intercept)") < 5000) {
    shapiro_p_ran_subdom <- shapiro.test(ranef(object$model)[[2]]$"(Intercept)")[[2]]
    shapiro_W_ran_subdom <- shapiro.test(ranef(object$model)[[2]]$"(Intercept)")[[1]]
  } else {
    warning(strwrap(prefix = " ", initial = "",
                    "Number of subdomains exceeds 5000 or is lower then 3 and thus
                    the Shapiro-Wilk test is not applicable for the random
                    effects."))
    shapiro_p_ran_subdom <- NA
    shapiro_W_ran_subdom <- NA
  }

  norm <- data.frame(
    Skewness = c(skewness_res, skewness_ran_dom, skewness_ran_subdom),
    Kurtosis = c(kurtosis_res, kurtosis_ran_dom, kurtosis_ran_subdom),
    Shapiro_W = c(shapiro_W_res, shapiro_W_ran_dom, shapiro_W_ran_subdom),
    Shapiro_p = c(shapiro_p_res, shapiro_p_ran_dom, shapiro_p_ran_subdom),
    row.names = c("Error", "Random_effect_domain", "Random_effect_subdomain")
  )
  tempMod <- object$model
  tempMod$call$fixed <- object$fixed
  r_squared <- suppressWarnings(r.squaredGLMM(tempMod))
  if (is.matrix(r_squared)) {
    r_marginal <- r_squared[1, 1]
    r_conditional <- r_squared[1, 2]
  } else {
    r_marginal <- r_squared[1]
    r_conditional <- r_squared[2]
  }
  icc_mixed_dom <- icc_domain(object$model)
  icc_mixed_subdom <- icc_subdomain(object$model)

  coeff_det <- data.frame(
    Marginal_R2    = r_marginal,
    Conditional_R2 = r_conditional,
    row.names      = ""
  )

  sum_emdi <- list(
    out_of_smp_dom = N_dom_unobs,
    in_smp_dom = N_dom_smp,
    out_of_smp_subdom = N_subdom_unobs,
    in_smp_subdom = N_subdom_smp,
    size_smp = smp_size,
    size_pop = pop_size,
    size_dom = sizedom_smp_pop,
    size_subdom = sizesubdom_smp_pop,
    smp_size_tab = NULL,
    transform = transform_method,
    normality = norm,
    icc_domain = icc_mixed_dom,
    icc_subdomain = icc_mixed_subdom,
    coeff_determ = coeff_det,
    call = call_emdi
  )

  class(sum_emdi) <- c("summary.ebp_tf", "emdi")
  sum_emdi
}


#' @export
print.summary.ebp_tf <- function(x, ...) {
  throw_class_error(x, "ebp_tf")
  cat("Twofold Empirical Best Prediction\n")
  cat("\n")
  cat("Call:\n ")
  print(x$call)
  cat("\n")
  cat("Out-of-sample domains: ", x$out_of_smp_dom, "\n")
  cat("In-sample domains: ", x$in_smp_dom, "\n")
  cat("\n")
  cat("Out-of-sample subdomains: ", x$out_of_smp_subdom, "\n")
  cat("In-sample subdomains: ", x$in_smp_subdom, "\n")
  cat("\n")
  cat("Sample sizes:\n")
  cat("Units in sample: ", x$size_smp, "\n")
  cat("Units in population: ", x$size_pop, "\n")
  print(x$size_dom)
  cat("\n")
  print(x$size_subdom)
  cat("\n")
  if (is.null(x$call$weights)) {
    cat("Explanatory measures:\n")
  } else {
    cat("Explanatory measures for the mixed model:\n")
  }
  print(x$coeff_determ)
  cat("\n")
  if (is.null(x$call$weights)) {
    cat("Residual diagnostics:\n")
  } else {
    cat("Residual diagnostics for the mixed model:\n")
  }
  print(x$normality)
  cat("\n")
  cat("ICC_domain: ", x$icc_domain, "\n")
  cat("ICC_subdomain: ", x$icc_subdomain, "\n")
  cat("\n")
  if (is.null(x$transform)) {
    cat("Transformation: No transformation \n")
  } else {
    cat("Transformation:\n")
    print(x$transform)
  }
}


#  ICC

icc_domain <- function(model) {
  u1 <- as.numeric(VarCorr(model)[2, 1])
  u2 <- as.numeric(VarCorr(model)[4, 1])
  e <- model$sigma^2
  u1 / (u1+ u2 + e)
}

icc_subdomain <- function(model) {
  u1 <- as.numeric(VarCorr(model)[2, 1])
  u2 <- as.numeric(VarCorr(model)[4, 1])
  e <- model$sigma^2
  u2 / (u1 + u2 + e)
}
