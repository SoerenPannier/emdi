#' @rdname plot.emdi
#' @export
plot.fh <- function(x,
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
  rand.eff <- x$model$random_effects
  srand.eff <- (rand.eff - mean(rand.eff)) / sd(rand.eff)
  tmp <- srand.eff


  NextMethod("plot",
    cooks = FALSE, boxcox = FALSE,
    cook_df = NULL, indexer = NULL, likelihoods = NULL,
    opt_lambda = FALSE, residuals = residuals, srand.eff = srand.eff, tmp = tmp
  )
}
