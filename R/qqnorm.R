#' Quantile-quantile plots for an emdi object
#'
#' Normal quantile-quantile plots of the underlying model in the EBP (see also
#' \code{\link{ebp}}) or Fay-Herriot (see also \code{\link{fh}}) approaches are 
#' obtained. The plots are obtained by \code{\link[ggplot2]{ggplot}}.
#' @param y a model object of type "emdi", either "ebp" or "fh", representing 
#' point and, if chosen, MSE estimates obtained by the EBP or Fay-Herriot approach 
#' (see also \code{\link{ebp}} and \code{\link{fh}}).
#' @param color a character vector with two elements. The first element defines
#' the color for the line in the QQ-plots, for the Cook's Distance plot and for
#' the Box-Cox plot. The second element defines the color for the densities.
#' @param gg_theme \code{\link[ggplot2]{theme}} list from package \pkg{ggplot2}.
#' For using this argument, package \pkg{ggplot2} must be loaded via
#' \code{library(ggplot2)}. See also Example 4.
#' @param ... optional arguments passed to generic function.
#' @return Two Q-Q plots in one grid obtained by \code{\link[ggplot2]{ggplot}}. 
#' @seealso \code{\link{emdiObject}}, \code{\link{ebp}}, \code{\link{fh}}
#' @examples
#' \donttest{
#' # Examples for models of type ebp
#' # Loading data - population and sample data
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'
#' # With default setting but na.rm = TRUE; with Box-Cox transformation
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl +
#' unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow +
#' house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#' na.rm = TRUE)
#'
#' # Example 1: Creation of default diagnostic plots
#' qqnorm(emdi_model)
#'
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
#' # Example 5: Creation of default diagnostic plots for Fay-Herriot model
#' qqnorm(fh_std)
#'
#' }
#' @export 
#' @method qqnorm emdi
#' @importFrom ggplot2 qplot geom_abline ggtitle ylab xlab ggplot stat_qq
#' @importFrom ggplot2 aes geom_point geom_smooth coord_fixed geom_line
#' @importFrom ggplot2 scale_color_manual scale_fill_manual geom_segment
#' @importFrom ggplot2 scale_linetype_discrete geom_density geom_text
#' @importFrom ggplot2 geom_line geom_vline stat_function geom_qq
#' @importFrom nlme ranef random.effects
#' @importFrom gridExtra arrangeGrob grid.arrange
#' @importFrom stats shapiro.test logLik cooks.distance
#' @importFrom HLMdiag mdffits

qqnorm.emdi <- function(y, color = c("blue", "lightblue3"),
                        gg_theme = NULL, ...) {
  
  extra_args <- list(...)
  residuals <- extra_args[["residuals"]]
  tmp <- extra_args[["tmp"]]

  ## QQ Plots
  # Residuals
  res <- qplot(sample = residuals) +
    geom_abline(colour = color[1]) +
    ggtitle("Error term") + ylab("Quantiles of pearson residuals") +
    xlab("Theoretical quantiles") + gg_theme
  
  # Random effects
  ran <- ggplot(data.frame(tmp) ,aes(sample = tmp)) +
    stat_qq(distribution = qnorm, dparams = list(mean = mean(tmp),
                                                sd = sd(tmp))) +
    geom_abline(intercept = 0, slope = 1,na.rm = TRUE, col = color[1]) +
    ggtitle("Random effect") + ylab("Quantiles of random effects") +
    xlab("Theoretical quantiles") + gg_theme
  
  invisible(grid.arrange(arrangeGrob(res, ran ,ncol = 2)))
}


#' @rdname qqnorm.emdi
#' @export
qqnorm.ebp <- function(y, color = c("blue", "lightblue3"),
                        gg_theme = NULL, ...) {

  residuals <- residuals(y$model, level = 0, type = "pearson")
  rand.eff <- nlme::ranef(y$model)$'(Intercept)'
  srand.eff <- (rand.eff - mean(rand.eff)) / sd(rand.eff)
  tmp <- as.matrix(random.effects(y$model))[,1]
  
  model <- y$model
  model$call$fixed <- y$fixed
  
  NextMethod("qqnorm",
             residuals = residuals, 
             tmp = tmp
             )
}

#' @rdname qqnorm.emdi
#' @export
qqnorm.fh <- function(y, color = c("blue", "lightblue3"),
                       gg_theme = NULL, ...) {
  
  if (any(is.na(y$model$std_real_residuals))) {
    residuals <- y$model$std_real_residuals[!is.na(y$model$std_real_residuals)]
    warning("At least one value in the standardized realized residuals is NA. Only
              numerical values are plotted.")
  } else {
    residuals <- y$model$std_real_residuals
  }
  residuals <- (residuals - mean(residuals)) / sd(residuals)
  rand.eff <- y$model$random_effects
  srand.eff <- (rand.eff - mean(rand.eff)) / sd(rand.eff)
  tmp <- srand.eff
  
  NextMethod("qqnorm",
             residuals = residuals, 
             tmp = tmp
  )
}

#' @rdname qqnorm.emdi
#' @export
qqnorm.direct <- function(y, ...) {
  message("For emdi objects obtained by direct estimation diagnostic plots are not reasonable.")
}