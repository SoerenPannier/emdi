#' @rdname plot.emdi
#' @export
plot.fh_tf <- function(x,
                    label = "orig",
                    color = c("blue", "lightblue3"),
                    gg_theme = NULL,
                    cooks = TRUE,
                    range = NULL, ...) {
  plot_check(x = x, label = label, color = color, cooks = cooks, range = range)

  if (any(is.na(x$model$std_real_residuals))) {
    residuals <- x$model$std_real_residuals[!is.na(x$model$std_real_residuals)]
    warning(strwrap(prefix = " ", initial = "",
                    "At least one value in the standardized realized residuals
                    is NA. Only numerical values are plotted."))
  } else {
    residuals <- x$model$std_real_residuals
  }
  residuals <- (residuals - mean(residuals)) / sd(residuals)
  rand.eff_domain <- x$model$random_effects_Domain
  srand.eff_domain <- (rand.eff_domain - mean(rand.eff_domain)) / sd(rand.eff_domain)
  tmp_domain <- srand.eff_domain

  rand.eff_subdomain <- x$model$random_effects_Subdomain
  srand.eff_subdomain <- (rand.eff_subdomain - mean(rand.eff_subdomain)) /
    sd(rand.eff_subdomain)
  tmp_subdomain <- srand.eff_subdomain


  NextMethod("plot",
    cooks = FALSE, boxcox = FALSE,
    cook_df = NULL, indexer = NULL, likelihoods = NULL,
    opt_lambda = FALSE, residuals = residuals,
    srand.eff_domain = srand.eff_domain, srand.eff_subdomain = srand.eff_subdomain,
    tmp_domain = tmp_domain, tmp_subdomain = tmp_subdomain
  )
}
