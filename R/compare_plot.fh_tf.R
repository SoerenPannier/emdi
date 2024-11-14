#' @rdname compare_plot
#' @export
compare_plot.fh_tf <- function(model = NULL, direct = NULL, indicator = "all",
                            MSE = FALSE, CV = FALSE, label = "orig",
                            color = c("blue", "lightblue3"),
                            shape = c(16, 16), line_type = c(
                              "solid",
                              "solid"
                            ),
                            gg_theme = NULL, level = NULL, ...) {
  compare_plot_check(
    model = model, indicator = indicator,
    label = label, color = color, shape = shape,
    line_type = line_type, gg_theme = gg_theme, level = level
  )

  if (any(inherits(direct, c("ebp", "ebp_tf", "fh")))) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0("It is not possible to compare the point and MSE
                        estimates of a model of type 'fh_tf', to the point and MSE
                        estimates of other model based emdi object such as 'ebp', 'ebp_tf', and 'fh'.")))
  }

  if (inherits(model, "fh_tf") && inherits(direct, "direct") && level == "subdomain") {
    warning(strwrap(prefix = " ", initial = "",
                    paste0("At subdomain level, fh_tf models are only compared to their own inherrent
                           direct estimates. Hence, the argument direct is
                           ignored."
                           )))
  }

  if (inherits(model, "fh_tf") && is.null(direct) && level == "domain") {
    stop(strwrap(prefix = " ", initial = "",
                    paste0("If the model is of type 'fh_tf' and the level is set at domain level,
                    the input argument direct is required."
                    )))
  }

  compare_plot_fh_tf(
    model = model, direct = direct, indicator = indicator,
    MSE = MSE, CV = CV, level = level,
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

compare_plot_fh_tf <- function(model, direct, indicator = "all", MSE = FALSE,
                            CV = FALSE, label = "orig", level = NULL,
                            color = c("blue", "lightblue3"),
                            shape = c(16, 16), line_type = c("solid", "solid"),
                            gg_theme = NULL) {
  Model_based <- NULL
  Direct <- NULL
  ID <- NULL
  value <- NULL
  Method <- NULL

  if(level == "domain"){
    Data <- point_emdi(object = model, indicator = "all")$ind_Domain

    compare_plot_check2(Data, direct$ind)

    Data <- merge(direct$ind[, c("Domain", indicator)], Data,
                  id = "Domain", all.x = T, all.y = F)
    colnames(Data)[2] <- "Direct"
    selected_indicators <- colnames(Data)[!(colnames(Data) %in% c(
      "Domain",
      "Direct"
    ))]
    colnames(Data) <- c("Domain", "FH_TF_Direct",
                        paste0(colnames(Data)[!(colnames(Data) %in%
                                                  c("Domain", "Direct"))], "_Model"))


    if (is.null(model$MSE_Domain)) {
      Data$smp_size <- NULL
    }
  }else if(level == "subdomain"){
    Data <- point_emdi(object = model, indicator = "all")$ind_Subdomain
    Data <- Data[!is.na(Data$Direct), ]
    selected_indicators <- colnames(Data)[!(colnames(Data) %in% c(
      "Subdomain",
      "Direct"
    ))]
    colnames(Data) <- c(
      "Subdomain", "FH_TF_Direct",
      paste0(
        colnames(Data)[!(colnames(Data) %in%
                           c("Subdomain", "Direct"))],
        "_Model"
      )
    )

    if (!(any(indicator == "all") || any(indicator == "direct") ||
          any(indicator == "Direct"))) {
      selected_indicators <- selected_indicators[selected_indicators %in%
                                                   indicator]

      if (is.null(model$MSE_Subdomain)) {
        Data$smp_size <- NULL
      }
    }
  }


  if (MSE == TRUE || CV == TRUE) {
    all_precisions <- mse_emdi(object = model, indicator = "all", CV = TRUE)
    if(level == "subdomain"){
      colnames(all_precisions$ind_Subdomain) <- c("Subdomain", paste0(c(
        "FH_TF_Direct",
        "FH_TF_Model"
      ), "_MSE"))
      colnames(all_precisions$ind_cv_Subdomain) <- c("Subdomain", paste0(c(
        "FH_TF_Direct",
        "FH_TF_Model"
      ), "_CV"))
      combined <- merge(all_precisions$ind_Subdomain,
                        all_precisions$ind_cv_Subdomain, id = "Subdomain")
      combined <- combined[!is.na(combined$FH_TF_Direct_MSE), ]

      Data <- merge(Data, combined, id = "Subdomain")
      colnames(Data)[1] <- "Domain"
      Data$smp_size <- -Data$FH_TF_Direct_MSE
      Data$smp_size2 <- -Data$FH_TF_Direct_CV
    } else if(level == "domain"){
      colnames(all_precisions$ind_Domain) <- c("Domain", paste0(c(
        "FH_TF_Model"
      ), "_MSE"))
      colnames(all_precisions$ind_cv_Domain) <- c("Domain", paste0(c(
        "FH_TF_Model"
      ), "_CV"))
      combined <- merge(all_precisions$ind_Domain,
                        all_precisions$ind_cv_Domain, id = "Domain")

      dir_precision <- merge(direct$MSE[, c("Domain", indicator)],
                             direct$ind[, c("Domain", indicator)],
                             by = "Domain")
      colnames(dir_precision) <- c("Domain", "MSE", "Direct")
      dir_precision$CV <- sqrt(dir_precision$MSE)/dir_precision$Direct

      combined <- merge(dir_precision[, c("Domain", "MSE", "CV")], combined,
                        id = "Domain", all.x = T, all.y = F)
      colnames(combined)[2:3] <- c("FH_TF_Direct_MSE", "FH_TF_Direct_CV")

      Data <- merge(Data, combined, id = "Domain")
      Data$smp_size <- -Data$FH_TF_Direct_MSE
      Data$smp_size2 <- -Data$FH_TF_Direct_CV
    }

  }

  if (model$framework$N_out_sub > 0) {
    message(strwrap(prefix = " ", initial = "",
                   "Please note that since all of the comparisons need a direct
                   estimator, the plots are only created for in-sample domains or subdomains"))
  }
  if(level == "subdomain"){
    compare_plots(
      object = Data, type = "tf_sub",
      selected_indicators = selected_indicators, level = level,
      MSE = MSE, CV = CV, label = label, color = color,
      shape = shape, line_type = line_type, gg_theme = gg_theme
    )
  } else if(level == "domain"){
    compare_plots(
      object = Data, type = "area",
      selected_indicators = selected_indicators, level = level,
      MSE = MSE, CV = CV, label = label, color = color,
      shape = shape, line_type = line_type, gg_theme = gg_theme
    )
  }


}
