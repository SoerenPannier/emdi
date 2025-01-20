#' Plots for an emdi Object
#'
#' Diagnostic plots of the underlying model in the EBP (see also
#' \code{\link{ebp}}; \code{\link{ebp_tf}}) or Fay-Herriot (see also
#' \code{\link{fh}}; \code{\link{fh_tf}}) approaches are obtained. These include
#' Q-Q plots and density plots of residuals and random effects from the nested
#' error linear regression model/the Fay-Herriot model, a Cook's distance plot
#' for detecting outliers and the log-likelihood of the estimation of the
#' optimal parameter in the data-driven transformations (Box-Cox and log-shift
#' transformations) - the latter two only for ebp and ebp_tf. The return depends
#' on the transformation such that a plot for the optimal parameter is only
#' returned in case a transformation with transformation parameter is chosen.
#' The range of the x-axis is optional but necessary to change if there are
#' convergence problems. All plots are obtained by \code{\link[ggplot2]{ggplot}}.
#' @param x an object of type "emdi", either "ebp", "ebp_tf", "fh" or "fh_tf",
#' representing point and, if chosen, MSE estimates obtained by the EBP or
#' Fay-Herriot approach (see also \code{\link{ebp}}, \code{\link{ebp_tf}},
#' \code{\link{fh}} and \code{\link{fh_tf}}).
#' @param label argument that enables to customize title and axis labels. There
#' are three instant options to label the diagnostic plot: (i) original labels
#' ("orig"), (ii) axis labels but no title ("no_title"), (iii) neither axis
#' labels nor title ("blank").
#' (iv) individual labels by a list that needs to
#' have below structure. Six elements can be defined called
#' \code{qq_res, qq_ran, qq_ran_dom, qq_ran_subdom, d_res, d_ran, d_ran_dom,
#' d_ran_subdom, cooks} and \code{opt_lambda} for the ten different plots and
#' these list elements need to have three elements each called \code{title,
#' y_lab and x_lab}. Only the labels for the plots that should be different to
#' the original need to be specified. Please see the details section for an
#' example with the default labels.
#' @param color a character vector with two elements. The first element defines
#' the color for the line in the QQ-plots, for the Cook's Distance plot and for
#' the transformation parameter plot. The second element defines the color for
#' the densities.
#' @param gg_theme \code{\link[ggplot2]{theme}} list from package \pkg{ggplot2}.
#' For using this argument, package \pkg{ggplot2} must be loaded via
#' \code{library(ggplot2)}. See also Example 4.
#' @param cooks if \code{TRUE}, a Cook's distance plot is returned when the ebp
#' or ebp_tf function is used. The used method \code{mdffits.default} from the
#' package \pkg{HLMdiag} struggles when data sets get large. In these cases,
#' \code{cooks} should be set to \code{FALSE}. It defaults to \code{TRUE}.
#' @param range optional sequence determining the range of the x-axis for plots
#' of the optimal transformation parameter that defaults to \code{NULL}. In
#' that case a range of the default interval is used for the plots of the
#' optimal parameter. This leads in some cases to convergence problems such
#' that it should be changed to e.g. the selected \code{interval}. The default
#' value depends on the chosen data driven transformation and equals the
#' default interval for the estimation of the optimal parameter.
#' @param ... optional arguments passed to generic function.
#' @return Two Q-Q plots in one grid, two density plots, a Cook's distance plot
#' and a likelihood plot for the optimal parameter of transformations with
#' transformation parameter obtained by \code{\link[ggplot2]{ggplot}}. The
#' latter two plots are only provided for ebp object.
#' @details The default settings of the \code{label} argument for EBP approaches
#' are as follows (please note that the title for opt_lambda depends on the chosen
#' transformation, for the example Box-Cox is shown):\cr
##' \describe{
##' \item{list(}{}
##' \item{qq_res =}{c(title="Error term",
##'                 y_lab="Quantiles of pearson residuals",
##'                 x_lab="Theoretical quantiles"),}
##' \item{qq_ran =}{c(title="Random effect",
##'                 y_lab="Quantiles of random effects",
##'                 x_lab="Theoretical quantiles"),}
##' \item{qq_ran_dom =}{c(title="Random effect - domain level",
##'                     y_lab="Quantiles of random effects - domain level",
##'                     x_lab="Theoretical quantiles"),}
##' \item{qq_ran_subdom =}{c(title="Random effect - subdomain level",
##'                     y_lab="Quantiles of random effects - subdomain level",
##'                     x_lab="Theoretical quantiles"),}
##' \item{d_res =}{c(title="Density - Pearson residuals",
##'                y_lab="Density",
##'                x_lab="Pearson residuals"),}
##' \item{d_ran =}{c(title="Density - Standardized random effects",
##'                y_lab="Density",
##'                x_lab="Standardized random effects"),}
##' \item{d_ran_dom =}{c(title="Density - Standardized random effects",
##'                y_lab="Density",
##'                x_lab="Standardized random effects - domain level"),}
##' \item{d_ran_subdom =}{c(title="Density - Standardized random effects",
##'                y_lab="Density",
##'                x_lab="Standardized random effects - subdomain level"),}
##' \item{cooks =}{c(title="Cook's Distance Plot",
##'                y_lab="Cook's Distance",
##'                x_lab="Index"),}
##' \item{opt_lambda =}{c(title="Box-Cox - REML",
##'                y_lab="Log-Likelihood",
##'                x_lab="expression(lambda)"))}
##' }
##' The default settings of the \code{label} argument for FH approaches are as
##' follows (please note that the cook's distance plot and transformation
##' parameter plot are not provided for FH approaches):\cr
##' \describe{
##' \item{list(}{}
##' \item{qq_res =}{c(title = "Realized residuals",
##'                 y_lab = "Quantiles of std. residuals/sqrt(direct var.)",
##'                 x_lab = "Theoretical quantiles"),}
##' \item{qq_ran =}{c(title = "Random effect",
##'                 y_lab = "Quantiles of std. random effects",
##'                 x_lab = "Theoretical quantiles"),}
##' \item{qq_ran_dom =}{c(title = "Random effect at domain level",
##'                     y_lab = "Quantiles of std. random effects",
##'                     x_lab = "Theoretical quantiles"),}
##' \item{qq_ran_subdom =}{c(title = "Random effect at subdomain level",
##'                          y_lab = "Quantiles of std. random effects",
##'                          x_lab = "Theoretical quantiles"),}
##' \item{d_res =}{c(title = "Density - Std. residuals/sqrt(direct var.)",
##'                  y_lab = "Density",
##'                  x_lab = "Std. real. residuals"),}
##' \item{d_ran =}{c(title = "Density - Random effects",
##'                  y_lab = "Density",
##'                  x_lab = "Std. random effects"),}
##' \item{d_ran_dom =}{c(title = "Density - Random effects at domain level",
##'                      y_lab = "Density",
##'                      x_lab = "Std. random effects"),}
##' \item{d_ran_subdom =}{c(title = "Density - Random effects at subdomain level",
##'                         y_lab = "Density",
##'                         x_lab = "Std. random effects"),}
##' \item{cooks =}{c(title="",
##'                y_lab="",
##'                x_lab=""),}
##' \item{opt_lambda =}{c(title="",
##'                y_lab="",
##'                x_lab=""))}
##' }
#' @seealso \code{\link{emdiObject}}, \code{\link{ebp}}, \code{\link{ebp_tf}},
#' \code{\link{fh}}, \code{\link{fh_tf}}
#' @examples
#' \donttest{
#' # Examples for models of type ebp
#' # Loading data - population and sample data
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'
#' # With default setting but na.rm = TRUE; with Box-Cox transformation
#' emdi_model <- ebp(
#'   fixed = eqIncome ~ gender + eqsize + cash + self_empl +
#'     unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow +
#'     house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#'   pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#'   na.rm = TRUE
#' )
#'
#' # Example 1: Creation of default diagnostic plots
#' plot(emdi_model)
#'
#' # Example 2: Creation of diagnostic plots without labels and titles,
#' # different colors and without Cook's distance plot.
#' plot(emdi_model,
#'   label = "no_title", color = c("red", "yellow"),
#'   cooks = FALSE
#' )
#'
#' # Example 3: Creation of diagnostic plots where labels and title differs for
#' # residual plot
#' plot(emdi_model,
#'   label = list(qq_res = c(
#'     title = "Pearson resid.",
#'     y_lab = "Quant.", x_lab = "Theo. Quant."
#'   )), color = c("red", "yellow"),
#'   cooks = FALSE
#' )
#'
#' # Example 4: Usage of theme from ggplot2 within plot.emdi
#' library(ggplot2)
#' plot(emdi_model, gg_theme = theme(
#'   panel.background =
#'     element_rect(fill = "white", colour = "white"),
#'   plot.title = element_text(face = "bold"),
#'   title = element_text(color = "navy")
#' ))
#'
#' # Examples for models of type ebp_tf
#' # Loading data - population and sample data
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'
#' # With default setting but na.rm = TRUE; with Box-Cox transformation
#' emdi_model_tf <- ebp_tf(
#'   fixed = eqIncome ~ gender + eqsize + cash + self_empl +
#'     unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow +
#'     house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#'   pop_subdomains = "district", pop_domains = "state", smp_data = eusilcA_smp,
#'   smp_subdomains = "district", smp_domains = "state",
#'   na.rm = TRUE
#' )
#'
#' # Example 5: Creation of default diagnostic plots for ebp_tf
#' plot(emdi_model_tf)
#'
#' # Example for models of type fh
#'
#' # Loading data - population and sample data
#' data("eusilcA_popAgg")
#' data("eusilcA_smpAgg")
#'
#' # Combine sample and population data
#' combined_data <- combine_data(
#'   pop_data = eusilcA_popAgg,
#'   pop_domains = "Domain",
#'   smp_data = eusilcA_smpAgg,
#'   smp_domains = "Domain"
#' )
#'
#' # Generation of the emdi object
#' fh_std <- fh(
#'   fixed = Mean ~ cash + self_empl, vardir = "Var_Mean",
#'   combined_data = combined_data, domains = "Domain",
#'   method = "ml", MSE = TRUE
#' )
#'
#' # Example 6: Creation of default diagnostic plots for Fay-Herriot model
#' plot(fh_std)
#'
#' # Loading data - combined population and sample data
#' data(eusilcA_Agg_mun)
#'
#' # Generation of the emdi object
#' fh_std_tf <- fh_tf(fixed = Dir_Mean ~ eqsize + cash + self_empl + unempl_ben,
#'                    vardir = "Var_Mean",
#'                    domains = "ID_district",
#'                    subdomains = "ID_municipality",
#'                    transformation = "no",
#'                    subdomsize = "N_ik",
#'                    MSE = TRUE,
#'                    B = 50,
#'                    combined_data = eusilcA_Agg_mun)
#'
#' # Example 6: Creation of default diagnostic plots for twofold FH model
#' plot(fh_std_tf)
#' }
#' @export
#' @method plot emdi
#' @importFrom ggplot2 qplot geom_abline ggtitle ylab xlab ggplot stat_qq
#' @importFrom ggplot2 aes geom_point geom_smooth coord_fixed geom_line
#' @importFrom ggplot2 scale_color_manual scale_fill_manual geom_segment
#' @importFrom ggplot2 scale_linetype_discrete geom_density geom_text
#' @importFrom ggplot2 geom_line geom_vline stat_function geom_qq
#' @importFrom nlme ranef random.effects
#' @importFrom gridExtra arrangeGrob grid.arrange
#' @importFrom stats shapiro.test logLik cooks.distance
#' @importFrom HLMdiag mdffits
#' @importFrom stringr str_to_title
#' @name plot.emdi
#' @order 1

plot.emdi <- function(x,
                      label = "orig",
                      color = c("blue", "lightblue3"),
                      gg_theme = NULL,
                      cooks = TRUE,
                      range = NULL, ...) {
  plot_check(x = x, label = label, color = color, cooks = cooks, range = range)
  Residuals <- Random <- index <- lambda <- log_likelihood <- cooksdist <- NULL

  plotList <- vector(mode = "list", length = 5)
  plotList <- lapply(plotList, function(x) NA)
  names(plotList) <- c(
    "qq_plots", "density_res", "density_ran",
    "cooks_distance", "likelihood"
  )
  extra_args <- list(...)
  residuals <- extra_args[["residuals"]]
  #srand.eff <- extra_args[["srand.eff"]]
  #tmp <- extra_args[["tmp"]]
  cook_df <- extra_args[["cook_df"]]
  indexer <- extra_args[["indexer"]]
  likelihoods <- extra_args[["likelihoods"]]
  opt_lambda <- extra_args[["opt_lambda"]]

  #___________________Rachael added_____________________________________________
  if (any(inherits(x, which = TRUE, c("fh_tf", "ebp_tf")))) {
    srand.eff_dom <- extra_args[["srand.eff_dom"]]
    srand.eff_subdom <- extra_args[["srand.eff_subdom"]]
    tmp_dom <- extra_args[["tmp_dom"]]
    tmp_subdom <- extra_args[["tmp_subdom"]]
  }else{
    srand.eff <- extra_args[["srand.eff"]]
    tmp <- extra_args[["tmp"]]
    }
  #_____________________________________________________________________________

  label <- define_label(x = x, label = label)

  ## QQ Plots
  # Residuals
  res <- qplot(sample = residuals) +
    geom_abline(colour = color[1]) +
    ggtitle(label$qq_res["title"]) + ylab(label$qq_res["y_lab"]) +
    xlab(label$qq_res["x_lab"]) + gg_theme

  #_____________________________Rachael added___________________________________
  if(any(inherits(x, which = TRUE, c("fh_tf", "ebp_tf")))){
    ran_dom <- ggplot(data.frame(tmp_dom), aes(sample = tmp_dom)) +
      stat_qq(distribution = qnorm, dparams = list(
        mean = mean(tmp_dom),
        sd = sd(tmp_dom)
      )) +
      geom_abline(intercept = 0, slope = 1, na.rm = TRUE, col = color[1]) +
      ggtitle(label$qq_ran_dom["title"]) +
      ylab(label$qq_ran_dom["y_lab"]) +
      xlab(label$qq_ran_dom["x_lab"]) +
      gg_theme

    ran_subdom <- ggplot(data.frame(tmp_subdom), aes(sample = tmp_subdom)) +
      stat_qq(distribution = qnorm, dparams = list(
        mean = mean(tmp_subdom),
        sd = sd(tmp_subdom)
      )) +
      geom_abline(intercept = 0, slope = 1, na.rm = TRUE, col = color[1]) +
      ggtitle(label$qq_ran_subdom["title"]) +
      ylab(label$qq_ran_subdom["y_lab"]) +
      xlab(label$qq_ran_subdom["x_lab"]) +
      gg_theme

    plotList[[1]] <- arrangeGrob(res, ran_dom, ran_subdom, ncol = 3)
    grid.arrange(plotList[[1]])
  }else{
    # Random effects
    ran <- ggplot(data.frame(tmp), aes(sample = tmp)) +
      stat_qq(distribution = qnorm, dparams = list(
        mean = mean(tmp),
        sd = sd(tmp)
      )) +
      geom_abline(intercept = 0, slope = 1, na.rm = TRUE, col = color[1]) +
      ggtitle(label$qq_ran["title"]) +
      ylab(label$qq_ran["y_lab"]) +
      xlab(label$qq_ran["x_lab"]) +
      gg_theme
    plotList[[1]] <- arrangeGrob(res, ran, ncol = 2)
    grid.arrange(plotList[[1]])
  }
  #_____________________________________________________________________________

  #plotList[[1]] <- arrangeGrob(res, ran, ncol = 2)
  #grid.arrange(plotList[[1]])
  cat("Press [enter] to continue")
  line <- readline()

  #_________________________________________Rachael added________________________
  if(any(inherits(x, which = TRUE, c("fh_tf", "ebp_tf")))){
    #plotList[[1]] <- arrangeGrob(res, ran_dom, ran_subdom, ncol = 3)
    #grid.arrange(plotList[[1]])
    #cat("Press [enter] to continue")
    #line <- readline()

    print((plotList[[2]] <- ggplot(data.frame(Residuals = residuals),
                                   aes(x = Residuals),
                                   fill = color[2], color = color[2]
    ) +
      geom_density(
        fill = color[2], color = color[2],
        alpha = 0.4
      ) +
      stat_function(fun = dnorm) +
      ylab(label$d_res["y_lab"]) +
      xlab(label$d_res["x_lab"]) +
      ggtitle(label$d_res["title"]) +
      gg_theme))
    cat("Press [enter] to continue")
    line <- readline()
    #print((plotList[[3]]))

    ran_dom_density <- ggplot(data.frame(Random = srand.eff_dom),
                              aes(x = Random),
                              fill = color[2], color = color[2]
    ) +
      geom_density(
        fill = color[2], color = color[2],
        alpha = 0.4
      ) +
      stat_function(fun = dnorm) +
      ylab(label$d_ran_dom["y_lab"]) +
      xlab(label$d_ran_dom["x_lab"]) +
      ggtitle(label$d_ran_dom["title"]) +
      gg_theme

    ran_subdom_density <- ggplot(data.frame(Random = srand.eff_subdom),
                                 aes(x = Random),
                                 fill = color[2], color = color[2]
    ) +
      geom_density(
        fill = color[2], color = color[2],
        alpha = 0.4
      ) +
      stat_function(fun = dnorm) +
      ylab(label$d_ran_subdom["y_lab"]) +
      xlab(label$d_ran_subdom["x_lab"]) +
      ggtitle(label$d_ran_subdom["title"]) +
      gg_theme

    plotList[[3]] <- arrangeGrob(ran_dom_density, ran_subdom_density, ncol = 2)
    grid.arrange(plotList[[3]])
  }else {
    print((plotList[[2]] <- ggplot(data.frame(Residuals = residuals),
                                   aes(x = Residuals),
                                   fill = color[2], color = color[2]
    ) +
      geom_density(
        fill = color[2], color = color[2],
        alpha = 0.4
      ) +
      stat_function(fun = dnorm) +
      ylab(label$d_res["y_lab"]) +
      xlab(label$d_res["x_lab"]) +
      ggtitle(label$d_res["title"]) +
      gg_theme))
    cat("Press [enter] to continue")
    line <- readline()
    print((plotList[[3]] <- ggplot(data.frame(Random = srand.eff),
                                   aes(x = Random),
                                   fill = color[2], color = color[2]
    ) +
      geom_density(
        fill = color[2], color = color[2],
        alpha = 0.4
      ) +
      stat_function(fun = dnorm) +
      ylab(label$d_ran["y_lab"]) +
      xlab(label$d_ran["x_lab"]) +
      ggtitle(label$d_ran["title"]) +
      gg_theme))
  }



  #_____________________________________________________________________________
  if (cooks == TRUE) {
    cat("Press [enter] to continue")
    line <- readline()
    print((plotList[[4]] <- ggplot(data = cook_df, aes(
      x = index,
      y = cooksdist
    )) +
      geom_segment(aes(
        x = index, y = 0, xend = index,
        yend = cooksdist
      ),
      colour = color[1]
      ) +
      xlab("Index") +
      ylab(label$cooks["y_lab"])
      +
      geom_text(label = indexer[, 1], data = indexer) +
      ggtitle(label$cooks["title"]) +
      gg_theme))
  }

  if (opt_lambda == TRUE) {
    cat("Press [enter] to continue")
    line <- readline()

    if (any(label$opt_lambda["x_lab"] == "expression(lambda)") ||
      any(label$opt_lambda["x_lab"] == "expression(Lambda)")) {
      x_lab <- expression(lambda)
    } else {
      x_lab <- label$opt_lambda["x_lab"]
    }
    if (any(is.na(likelihoods))) {
      warning(strwrap(prefix = " ", initial = "",
                      "For some lambda in the chosen range, the
                      likelihood does not converge. For these lambdas,
                      no likelihood is plotted. Choose a different range
                      to avoid this behaviour"
                      ))
    }
    print((plotList[[5]] <- ggplot(
      data.frame(
        lambda = range,
        log_likelihood = likelihoods
      ),
      aes(x = lambda, y = log_likelihood)
    ) +
      geom_line() +
      xlab(x_lab) +
      ylab(label$opt_lambda["y_lab"]) +
      # geom_vline(xintercept = range[which.max(likelihoods)],
      #  colour = color[1]) + ggtitle(label$opt_lambda["title"]) +
      geom_vline(
        xintercept = x$transform_param$optimal_lambda,
        colour = color[1]
      ) +
      ggtitle(label$opt_lambda["title"]) +
      gg_theme))
  }
  invisible(plotList)
}



#' @rdname plot.emdi
#' @export
plot.direct <- function(x, ...) {
  message(strwrap(prefix = " ", initial = "",
                  "For emdi objects obtained by direct estimation diagnostic
                  plots are not reasonable."))
}


# Definition of the labels

define_label <- function(x, label) {
  if (!inherits(label, "list")) {
    if (label == "orig") {
      if (inherits(x, "ebp")) {
        label <- list(
          qq_res = c(
            title = "Error term",
            y_lab = "Quantiles of pearson residuals",
            x_lab = "Theoretical quantiles"
          ),
          qq_ran = c(
            title = "Random effect",
            y_lab = "Quantiles of random effects",
            x_lab = "Theoretical quantiles"
          ),
          d_res = c(
            title = "Density - Pearson residuals",
            y_lab = "Density",
            x_lab = "Pearson residuals"
          ),
          d_ran = c(
            title = "Density - Standardized random effects",
            y_lab = "Density",
            x_lab = "Standardized random effects"
          ),
          cooks = c(
            title = "Cook's Distance Plot",
            y_lab = "Cook's Distance",
            x_lab = "Index"
          ),
          opt_lambda = c(
            title =
              paste0(
                str_to_title(
                  gsub(
                    "\\.", "-",
                    x$transformation
                  )
                ), " - REML"
              ),
            y_lab = "Log-Likelihood",
            x_lab = "expression(lambda)"
          )
        )
      }else if (inherits(x, "ebp_tf")) {
        label <- list(
          qq_res = c(
            title = "Error term",
            y_lab = "Quantiles of pearson residuals",
            x_lab = "Theoretical quantiles"
          ),
          qq_ran_dom = c(
            title = "Random effect - domain level",
            y_lab = "Quantiles of random effects",
            x_lab = "Theoretical quantiles"
          ),
          qq_ran_subdom = c(
            title = "Random effect - subdomain level",
            y_lab = "Quantiles of random effects",
            x_lab = "Theoretical quantiles"
          ),
          d_res = c(
            title = "Density - Pearson residuals",
            y_lab = "Density",
            x_lab = "Pearson residuals"
          ),
          d_ran_dom = c(
            title = "Density - Standardized random effects",
            y_lab = "Density",
            x_lab = "Standardized random effects - domain"
          ),
          d_ran_subdom = c(
            title = "Density - Standardized random effects",
            y_lab = "Density",
            x_lab = "Standardized random effects -subdomain"
          ),
          cooks = c(
            title = "Cook's Distance Plot",
            y_lab = "Cook's Distance",
            x_lab = "Index"
          ),
          opt_lambda = c(
            title =
              paste0(
                str_to_title(
                  gsub(
                    "\\.", "-",
                    x$transformation
                  )
                ), " - REML"
              ),
            y_lab = "Log-Likelihood",
            x_lab = "expression(lambda)"
          )
        )
      }else if (inherits(x, "fh")) {
        label <- list(
          qq_res = c(
            title = "Realized residuals",
            y_lab = "Quantiles of std. residuals/sqrt(direct var.)",
            x_lab = "Theoretical quantiles"
          ),
          qq_ran = c(
            title = "Random effect",
            y_lab = "Quantiles of std. random effects",
            x_lab = "Theoretical quantiles"
          ),
          d_res = c(
            title = "Density - Std. residuals/sqrt(direct var.)",
            y_lab = "Density",
            x_lab = "Std. real. residuals"
          ),
          d_ran = c(
            title = "Density - Random effects",
            y_lab = "Density",
            x_lab = "Std. random effects"
          ),
          cooks = c(
            title = "",
            y_lab = "",
            x_lab = ""
          ),
          opt_lambda = c(
            title = "",
            y_lab = "",
            x_lab = ""
          )
        )
      } else if (inherits(x, "fh_tf")) {
        label <- list(
          qq_res = c(
            title = "Realized residuals",
            y_lab = "Quantiles of std. residuals/sqrt(direct var.)",
            x_lab = "Theoretical quantiles"
          ),
          qq_ran_dom = c(
            title = "Random effect at domain level",
            y_lab = "Quantiles of std. random effects",
            x_lab = "Theoretical quantiles"
          ),
          qq_ran_subdom = c(
            title = "Random effect at subdomain level",
            y_lab = "Quantiles of std. random effects",
            x_lab = "Theoretical quantiles"
          ),
          d_res = c(
            title = "Density - Std. residuals/sqrt(direct var.)",
            y_lab = "Density",
            x_lab = "Std. real. residuals"
          ),
          d_ran_dom = c(
            title = "Density - Random effects at domain level",
            y_lab = "Density",
            x_lab = "Std. random effects"
          ),
          d_ran_subdom = c(
            title = "Density - Random effects at subdomain level",
            y_lab = "Density",
            x_lab = "Std. random effects"
          ),
          cooks = c(
            title = "",
            y_lab = "",
            x_lab = ""
          ),
          opt_lambda = c(
            title = "",
            y_lab = "",
            x_lab = ""
          )
        )
      }
    } else if (label == "blank") {
      label <- list(
        qq_res = c(
          title = "",
          y_lab = "",
          x_lab = ""
        ),
        qq_ran = c(
          title = "",
          y_lab = "",
          x_lab = ""
        ),
        qq_ran_dom = c(
          title = "",
          y_lab = "",
          x_lab = ""
        ),
        qq_ran_subdom = c(
          title = "",
          y_lab = "",
          x_lab = ""
        ),
        d_res = c(
          title = "",
          y_lab = "",
          x_lab = ""
        ),
        d_ran = c(
          title = "",
          y_lab = "",
          x_lab = ""
        ),
        d_ran_dom = c(
          title = "",
          y_lab = "",
          x_lab = ""
        ),
        d_ran_subdom = c(
          title = "",
          y_lab = "",
          x_lab = ""
        ),
        cooks = c(
          title = "",
          y_lab = "",
          x_lab = ""
        ),
        opt_lambda = c(
          title = "",
          y_lab = "",
          x_lab = ""
        )
      )
    } else if (label == "no_title") {
      if (inherits(x, "ebp")) {
        label <- list(
          qq_res = c(
            title = "",
            y_lab = "Quantiles of pearson residuals",
            x_lab = "Theoretical quantiles"
          ),
          qq_ran = c(
            title = "",
            y_lab = "Quantiles of random effects",
            x_lab = "Theoretical quantiles"
          ),
          d_res = c(
            title = "",
            y_lab = "Density",
            x_lab = "Pearson residuals"
          ),
          d_ran = c(
            title = "",
            y_lab = "Density",
            x_lab = "Standardized random effects"
          ),
          cooks = c(
            title = "",
            y_lab = "Cook's Distance",
            x_lab = "Index"
          ),
          opt_lambda = c(
            title = "",
            y_lab = "Log-Likelihood",
            x_lab = "expression(lambda)"
          )
        )
      }else if (inherits(x, "ebp_tf")) {
        label <- list(
          qq_res = c(
            title = "",
            y_lab = "Quantiles of pearson residuals",
            x_lab = "Theoretical quantiles"
          ),
          qq_ran_dom = c(
            title = "",
            y_lab = "Quantiles of random effects",
            x_lab = "Theoretical quantiles"
          ),
          qq_ran_subdom = c(
            title = "",
            y_lab = "Quantiles of random effects",
            x_lab = "Theoretical quantiles"
          ),
          d_res = c(
            title = "",
            y_lab = "Density",
            x_lab = "Pearson residuals"
          ),
          d_ran_dom = c(
            title = "",
            y_lab = "Density",
            x_lab = "Standardized random effects - domain level"
          ),
          d_ran_subdom = c(
            title = "",
            y_lab = "Density",
            x_lab = "Standardized random effects - subdomain level"
          ),
          cooks = c(
            title = "",
            y_lab = "Cook's Distance",
            x_lab = "Index"
          ),
          opt_lambda = c(
            title = "",
            y_lab = "Log-Likelihood",
            x_lab = "expression(lambda)"
          )
        )
      } else if (inherits(x, "fh")) {
        label <-
          list(
            qq_res = c(
              title = "",
              y_lab = "Quantiles of std. residuals/sqrt(direct var.)",
              x_lab = "Theoretical quantiles"
            ),
            qq_ran = c(
              title = "",
              y_lab = "Quantiles of std. random effects",
              x_lab = "Theoretical quantiles"
            ),
            d_res = c(
              title = "",
              y_lab = "Density",
              x_lab = "Std. real. residuals"
            ),
            d_ran = c(
              title = "",
              y_lab = "Density",
              x_lab = "Std. random effects"
            ),
            cooks = c(
              title = "",
              y_lab = "",
              x_lab = ""
            ),
            opt_lambda = c(
              title = "",
              y_lab = "",
              x_lab = ""
            )
          )
      }     else if (inherits(x, "fh_tf")) {
        label <- list(
          qq_res = c(
            title = "",
            y_lab = "Quantiles of std. residuals/sqrt(direct var.)",
            x_lab = "Theoretical quantiles"
          ),
          qq_ran_dom = c(
            title = "",
            y_lab = "Quantiles of std. random effects",
            x_lab = "Theoretical quantiles"
          ),
          qq_ran_subdom = c(
            title = "",
            y_lab = "Quantiles of std. random effects",
            x_lab = "Theoretical quantiles"
          ),
          d_res = c(
            title = "",
            y_lab = "Density",
            x_lab = "Std. real. residuals"
          ),
          d_ran_dom = c(
            title = "",
            y_lab = "Density",
            x_lab = "Std. random effects"
          ),
          d_ran_subdom = c(
            title = "",
            y_lab = "Density",
            x_lab = "Std. random effects"
          ),
          cooks = c(
            title = "",
            y_lab = "",
            x_lab = ""
          ),
          opt_lambda = c(
            title = "",
            y_lab = "",
            x_lab = ""
          )
        )
      }

    }
  } else if (inherits(label, "list")) {
    if (any(names(label) == "box_cox")) {
      warning(strwrap(prefix = " ", initial = "",
                      "In following versions of package emdi, the list element
                      box_cox will be renamed into opt_lambda."))
    }

    if (!any(names(label) %in% c(
      "qq_res", "qq_ran", "qq_ran_dom", "qq_ran_subdom",
      "d_res", "d_ran", "d_ran_dom", "d_ran_subdom",
      "cooks", "opt_lambda"
    )) ||
      !any(names(label) %in% c(
        "qq_res", "qq_ran", "qq_ran_dom", "qq_ran_subdom",
        "d_res", "d_ran", "d_ran_dom", "d_ran_subdom",
        "cooks", "box_cox"
      ))) {
      stop(strwrap(prefix = " ", initial = "",
                   "List elements must have following names even though not
                   all must be included: qq_res, qq_ran, qq_ran_dom,
                   qq_ran_subdom, d_res, d_ran, d_ran_dom, d_ran_subdom, cooks,
                   opt_lambda. Every list element must have the elements title,
                   y_lab and x_lab. See also help(plot.emdi)."))
    }
    for (i in names(label)) {
      if (!all(names(label[[i]]) == c("title", "y_lab", "x_lab"))) {
        stop(strwrap(prefix = " ", initial = "",
                     "Every list element must have the elements title, y_lab
                     and x_lab in this order. See also help(plot.emdi)."))
      }
    }

    orig_label <- list(
      qq_res = c(
        title = "Error term",
        y_lab = "Quantiles of pearson residuals",
        x_lab = "Theoretical quantiles"
      ),
      qq_ran = c(
        title = "Random effect",
        y_lab = "Quantiles of random effects",
        x_lab = "Theoretical quantiles"
      ),
      qq_ran_dom = c(
        title = "Random effect - domain",
        y_lab = "Quantiles of random effects - domain",
        x_lab = "Theoretical quantiles"
      ),
      qq_ran_dom = c(
        title = "Random effect - subdomain",
        y_lab = "Quantiles of random effects - subdomain",
        x_lab = "Theoretical quantiles"
      ),
      d_res = c(
        title = "Density - Pearson residuals",
        y_lab = "Density",
        x_lab = "Pearson residuals"
      ),
      d_ran = c(
        title = "Density - Standardized random effects",
        y_lab = "Density",
        x_lab = "Standardized random effects"
      ),
      d_ran_dom = c(
        title = "Density - Standardized random effects",
        y_lab = "Density",
        x_lab = "Standardized random effects - domain"
      ),
      d_ran_subdom = c(
        title = "Density - Standardized random effects",
        y_lab = "Density",
        x_lab = "Standardized random effects - subdom"
      ),
      cooks = c(
        title = "Cook's Distance Plot",
        y_lab = "Cook's Distance",
        x_lab = "Index"
      ),
      opt_lambda = c(
        title = paste0(
          str_to_title(gsub(
            "\\.", "-",
            x$transformation
          )), " - REML"
        ),
        y_lab = "Log-Likelihood",
        x_lab = "expression(lambda)"
      )
    )

    if (any(names(label) == "qq_res")) {
      label$qq_res <- label$qq_res
    } else {
      label$qq_res <- orig_label$qq_res
    }
    if (any(names(label) == "qq_ran")) {
      label$qq_ran <- label$qq_ran
    }else {
      label$qq_ran <- orig_label$qq_ran
    }
    if (any(names(label) == "qq_ran_dom")) {
      label$qq_ran_dom <- label$qq_ran_dom
    }else {
      label$qq_ran_dom <- orig_label$qq_ran_dom
    }
    if (any(names(label) == "qq_ran_subdom")) {
      label$qq_ran_subdom <- label$qq_ran_subdom
    }else {
      label$qq_ran_subdom <- orig_label$qq_ran_subdom
    }
    if (any(names(label) == "d_res")) {
      label$d_res <- label$d_res
    } else {
      label$d_res <- orig_label$d_res
    }
    if (any(names(label) == "d_ran")) {
      label$d_ran <- label$d_ran
    } else {
      label$d_ran <- orig_label$d_ran
    }
    if (any(names(label) == "d_ran_dom")) {
      label$d_ran_dom <- label$d_ran_dom
    } else {
      label$d_ran_dom <- orig_label$d_ran_dom
    }
    if (any(names(label) == "d_ran_subdom")) {
      label$d_ran_subdom <- label$d_ran_subdom
    } else {
      label$d_ran_subdom <- orig_label$d_ran_subdom
    }
    if (any(names(label) == "cooks")) {
      label$cooks <- label$cooks
    } else {
      label$cooks <- orig_label$cooks
    }
    if (any(names(label) == "opt_lambda") || any(names(label) == "box_cox")) {
      if (any(names(label) == "opt_lambda")) {
        label$opt_lambda <- label$opt_lambda
      } else if (any(names(label) == "box_cox")) {
        label$opt_lambda <- label$box_cox
      }
    } else {
      label$opt_lambda <- orig_label$opt_lambda
    }
  }

  if (any(!(names(label) %in% c(
    "qq_res", "qq_ran", "qq_ran_dom", "qq_ran_subdom",
    "d_res", "d_ran", "d_ran_dom", "d_ran_subdom",
    "cooks", "opt_lambda", "box_cox"
  )))) {
    warning(strwrap(prefix = " ", initial = "",
                    "One or more list elements are not called qq_res, qq_ran,
                    qq_ran_dom, qq_ran_subdom,d_res, d_ran, d_ran_dom,
                    d_ran_subdom, cooks or opt_lambda. The changes are for
                    this/these element(s) is/are not done. Instead the original
                    labels are used."))
  }

  return(label)
}
