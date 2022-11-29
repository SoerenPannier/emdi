#' @rdname compare_plot
#' @export
compare_plot.fh <- function(model = NULL, direct = NULL, indicator = "all",
                            MSE = FALSE, CV = FALSE, label = "orig",
                            color = c("blue", "lightblue3"),
                            shape = c(16, 16), line_type = c(
                              "solid",
                              "solid"
                            ),
                            gg_theme = NULL, ...) {
  compare_plot_check(
    model = model, indicator = indicator,
    label = label, color = color, shape = shape,
    line_type = line_type, gg_theme = gg_theme
  )

  if (inherits(direct, "ebp")) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0("It is not possible to compare the point and MSE
                        estimates of a model of type 'fh', to the point and MSE
                        estimates of an 'ebp' object.")))
  }

  if (inherits(model, "fh") && inherits(direct, "direct")) {
    warning(strwrap(prefix = " ", initial = "",
                    paste0("fh models are only compared to their own inherrent
                           direct estimates. Hence, the argument direct is
                           ignored."
                           )))
  }

  compare_plot_fh(
    model = model, direct = model, indicator = indicator,
    MSE = MSE, CV = CV,
    label = label, color = color, shape = shape,
    line_type = line_type, gg_theme = gg_theme
  )
}


#' Shows plots for the comparison of estimates
#'
#' For all indicators or a selection of indicators two plots are returned. The
#' first plot is a scatter plot of estimates to compare and the second is a line
#' plot with these estimates.
#' @param model an object of type "emdi", either "ebp" or "fh", representing
#' point and MSE estimates.
#' @param direct an object of type "direct","emdi", representing point
#' and MSE estimates.
#' @param indicator optional character vector that selects which indicators
#' shall be returned: (i) all calculated indicators ("all");
#' (ii) each indicator name: "Mean", "Quantile_10", "Quantile_25", "Median",
#' "Quantile_75", "Quantile_90", "Head_Count",
#' "Poverty_Gap", "Gini", "Quintile_Share" or the function name/s of
#' "custom_indicator/s"; (iii) groups of indicators: "Quantiles", "Poverty",
#' "Inequality" or "Custom".If two of these groups are selected, only the first
#' one is returned. Defaults to "all". Note, additional custom indicators can be
#' defined as argument for model-based approaches (see also \code{\link{ebp}})
#' and do not appear in groups of indicators even though these might belong to
#' one of the groups.
#' @param MSE optional logical. If \code{TRUE}, the MSE estimates of the direct
#' and model-based estimates are compared via suitable plots. Defaults to
#' \code{FALSE}.
#' @param CV optional logical. If \code{TRUE}, the coefficient of variation
#' estimates of the direct and model-based estimates are compared via suitable
#' plots. Defaults to \code{FALSE}.
#' @param label argument that enables to customize title and axis labels. There
#' are three options to label the evaluation plots: (i) original labels
#' ("orig"), (ii) axis labels but no title ("no_title"), (iii) neither axis
#' labels nor title ("blank").
#' @param color a vector with two elements determining color schemes in returned
#' plots.
#' @param shape a numeric vector with two elements determining the shape of
#' points in returned plots.
#' @param line_type a character vector with two elements determining the line
#' types in returned plots.
#' @param gg_theme \code{\link[ggplot2]{theme}} list from package \pkg{ggplot2}.
#' For using this argument, package \pkg{ggplot2} must be loaded via
#' \code{library(ggplot2)}.
#' @param ... further arguments passed to or from other methods.
#' @return A scatter plot and a line plot comparing direct and model-based
#' estimators for each selected indicator obtained by
#' \code{\link[ggplot2]{ggplot}}. If the input arguments MSE and CV are set to
#' TRUE two extra plots are created, respectively: the MSE/CV estimates of the
#' direct and model-based estimates are compared by boxplots and scatter plots.
#' @noRd

compare_plot_fh <- function(model, direct, indicator = "all", MSE = FALSE,
                            CV = FALSE, label = "orig",
                            color = c("blue", "lightblue3"),
                            shape = c(16, 16), line_type = c("solid", "solid"),
                            gg_theme = NULL) {
  Model_based <- NULL
  Direct <- NULL
  ID <- NULL
  value <- NULL
  Method <- NULL

  Data <- point_emdi(object = model, indicator = "all")$ind
  Data <- Data[!is.na(Data$Direct), ]
  selected_indicators <- colnames(Data)[!(colnames(Data) %in% c(
    "Domain",
    "Direct"
  ))]
  colnames(Data) <- c(
    "Domain", "FH_Direct",
    paste0(
      colnames(Data)[!(colnames(Data) %in%
        c("Domain", "Direct"))],
      "_Model"
    )
  )
  if ("FH_Bench" %in% selected_indicators) {
    Data$FH_Bench_Direct <- Data$FH_Direct
  }
  if ("FH_Bench" %in% indicator && !("FH_Bench" %in% selected_indicators)) {
    message(strwrap(prefix = " ", initial = "",
                   "emdi object does not contain benchmarked fh estimates.
                   Only FH estimates are compared with direct. See also
                   help(benchmark)."))
  }

  if (!(any(indicator == "all") || any(indicator == "direct") ||
    any(indicator == "Direct"))) {
    selected_indicators <- selected_indicators[selected_indicators %in%
      indicator]
  }

  if (is.null(model$MSE)) {
    Data$smp_size <- NULL
  }

  if (MSE == TRUE || CV == TRUE) {
    all_precisions <- mse_emdi(object = model, indicator = "all", CV = TRUE)
    colnames(all_precisions$ind) <- c("Domain", paste0(c(
      "FH_Direct",
      "FH_Model"
    ), "_MSE"))
    colnames(all_precisions$ind_cv) <- c("Domain", paste0(c(
      "FH_Direct",
      "FH_Model"
    ), "_CV"))
    combined <- merge(all_precisions$ind, all_precisions$ind_cv, id = "Domain")
    combined <- combined[!is.na(combined$FH_Direct_MSE), ]

    Data <- merge(Data, combined, id = "Domain")
    Data$smp_size <- -Data$FH_Direct_MSE
    Data$smp_size2 <- -Data$FH_Direct_CV
  }

  if (model$framework$N_dom_unobs > 0) {
    message(strwrap(prefix = " ", initial = "",
                   "Please note that since all of the comparisons need a direct
                   estimator, the plots are only created for in-sample
                   domains."))
  }

  compare_plots(
    object = Data, type = "area",
    selected_indicators = selected_indicators,
    MSE = MSE, CV = CV, label = label, color = color,
    shape = shape, line_type = line_type, gg_theme = gg_theme
  )
}
