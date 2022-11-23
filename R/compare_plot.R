#' Shows Plots for the Comparison of Estimates
#'
#' Function \code{compare_plot} is a generic function used to produce plots
#' comparing point and existing MSE/CV estimates of direct and model-based
#' estimation for all indicators or a selection of indicators.
#' @param model a model object of type "emdi", either "ebp" or "fh",
#' representing point and MSE estimates.
#' @param direct an object of type "direct","emdi", representing point
#' and MSE estimates. If the input argument \code{model} is of type "ebp",
#' \code{direct} is required. If the input argument \code{model} is of type
#' "fh", the \code{direct} component is already included in the input
#' argument \code{model}.
#' @param indicator optional character vector that selects which indicators
#' shall be returned. Defaults to "all".
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
#' @return Plots comparing direct and model-based estimators for each selected
#' indicator obtained by \code{\link[ggplot2]{ggplot}}.
#' @export
#' @name compare_plot

compare_plot <- function(model, direct, indicator = "all", MSE = FALSE,
                         CV = FALSE, label = "orig",
                         color = c("blue", "lightblue3"),
                         shape = c(16, 16), line_type = c("solid", "solid"),
                         gg_theme = NULL, ...) {
  UseMethod("compare_plot")
}



#' Shows Plots for the Comparison of Estimates
#'
#' Methods \code{compare_plot.direct}, \code{compare_plot.ebp} and
#' \code{compare_plot.fh} produce plots comparing point and existing
#' MSE/CV estimates of direct and model-based estimation for all indicators or a
#' selection of indicators for objects of type "emdi". The direct and
#' model-based point estimates are compared by a scatter plot and a line plot
#' for each selected indicator. If the input arguments MSE and CV are set to
#' TRUE, two extra plots are created, respectively: the MSE/CV estimates of the
#' direct and model-based estimates are compared by boxplots and scatter plots.
#' @param model a model object of type "emdi", either "ebp" or "fh",
#' representing point and MSE estimates.
#' @param direct an object of type "direct","emdi", representing point
#' and MSE estimates. If the input argument \code{model} is of type "ebp",
#' \code{direct} is required. If the input argument \code{model} is of type
#' "fh", the \code{direct} component is already included in the input
#' argument \code{model}.
#' @param indicator optional character vector that selects which indicators
#' shall be returned: (i) all calculated indicators ("all");
#' (ii) each indicator name: "Mean", "Quantile_10", "Quantile_25", "Median",
#' "Quantile_75", "Quantile_90", "Head_Count",
#' "Poverty_Gap", "Gini", "Quintile_Share" or the function name/s of
#' "custom_indicator/s"; (iii) groups of indicators: "Quantiles", "Poverty",
#' "Inequality" or "Custom". If two of these groups are selected, only the first
#' one is returned. Note, additional custom indicators can be defined as
#' argument for the EBP approaches (see also \code{\link{ebp}}) and do not
#' appear in groups of indicators even though these might belong to one of the
#' groups. If the \code{model} argument is of type "fh", indicator can be set
#' to "all", "Direct", FH", or "FH_Bench" (if emdi object is overwritten by
#' function \code{\link{benchmark}}). Defaults to "all".
#' @param MSE optional logical. If \code{TRUE}, the MSE estimates of the direct
#' and model-based estimates are compared via boxplots and scatter plots.
#' @param CV optional logical. If \code{TRUE}, the coefficient of variation
#' estimates of the direct and model-based estimates are compared via boxplots
#' and scatter plots.
#' @param label argument that enables to customize title and axis labels. There
#' are three options to label the evaluation plots: (i) original labels
#' ("orig"), (ii) axis labels but no title ("no_title"), (iii) neither axis
#' labels nor title ("blank").
#' @param color a vector with two elements. The first color determines
#' the color for the regression line in the scatter plot and the color for
#' the direct estimates in the remaining plots. The second color specifies the
#' color of the intersection line in the scatter plot and the color for the
#' model-based estimates in the remaining plots. Defaults to
#' c("blue", "lightblue3").
#' @param shape a numeric vector with two elements. The first shape determines
#' the shape of the points in the scatterplot and the shape of the points for
#' the direct estimates in the remaining plots. The second shape determines
#' the shape for the points for the model-based estimates. The options
#' are numbered from 0 to 25. Defaults to c(16, 16).
#' @param line_type a character vector with two elements. The first line type
#' determines the line type for the regression line in the scatter plot and the
#' line type for the direct estimates in the remaining plots. The second line
#' type specifies the line type of the intersection line in the scatter plot and
#' the line type for the model-based estimates in the remaining plots. The
#' options are: "twodash", "solid", "longdash", "dotted", "dotdash", "dashed"
#' and "blank". Defaults to  c("solid", "solid").
#' @param gg_theme \code{\link[ggplot2]{theme}} list from package \pkg{ggplot2}.
#' For using this argument, package \pkg{ggplot2} must be loaded via
#' \code{library(ggplot2)}. See also Example 2.
#' @param ... further arguments passed to or from other methods.
#' @return A scatter plot and a line plot comparing direct and model-based
#' estimators for each selected indicator obtained by
#' \code{\link[ggplot2]{ggplot}}. If the input arguments MSE and CV are set to
#' TRUE two extra plots are created, respectively: the MSE/CV estimates of the
#' direct and model-based estimates are compared by boxplots and scatter plots.
#' @details Since all of the comparisons need a direct estimator, the plots are
#' only created for in-sample domains.
#' @seealso \code{\link{emdiObject}}, \code{\link{direct}}, \code{\link{ebp}},
#' \code{\link{fh}}
#' @examples
#' \donttest{
#' # Examples for comparisons of direct estimates and models of type ebp
#'
#' # Loading data - population and sample data
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'
#' # Generation of two emdi objects
#' emdi_model <- ebp(
#'   fixed = eqIncome ~ gender + eqsize + cash +
#'     self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent +
#'     fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#'   pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#'   threshold = function(y) {
#'     0.6 * median(y)
#'   }, L = 50, MSE = TRUE,
#'   na.rm = TRUE, cpus = 1
#' )
#'
#' emdi_direct <- direct(
#'   y = "eqIncome", smp_data = eusilcA_smp,
#'   smp_domains = "district", weights = "weight", threshold = 11161.44,
#'   var = TRUE, boot_type = "naive", B = 50, seed = 123, na.rm = TRUE
#' )
#'
#' # Example 1: Receive first overview
#' compare_plot(model = emdi_model, direct = emdi_direct)
#'
#' # Example 2: Change plot theme
#' library(ggplot2)
#' compare_plot(emdi_model, emdi_direct,
#'   indicator = "Median",
#'   gg_theme = theme(
#'     axis.line = element_line(size = 3, colour = "grey80"),
#'     plot.background = element_rect(fill = "lightblue3"),
#'     legend.position = "none"
#'   )
#' )
#'
#' # Example for comparison of direct estimates and models of type fh
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
#' # Example 3: Receive first overview
#' compare_plot(fh_std)
#'
#' # Example 4: Compare also MSE and CV estimates
#' compare_plot(fh_std, MSE = TRUE, CV = TRUE)
#' }
#' @importFrom reshape2 melt
#' @importFrom ggplot2 geom_point geom_smooth geom_line geom_boxplot
#' @importFrom ggplot2 aes xlim ylim scale_shape_manual scale_linetype_manual
#' @importFrom ggplot2 scale_color_manual scale_fill_manual coord_flip
#' @name compare_plots_emdi
#' @rdname compare_plot
NULL



compare_plots <- function(object, type, selected_indicators, MSE, CV, label,
                          color, shape, line_type, gg_theme, ...) {
  Model_based <- NULL
  Direct <- NULL
  ID <- NULL
  value <- NULL
  Method <- NULL
  slope <- NULL
  intercept <- NULL
  area <- NULL


  if (MSE == FALSE && CV == FALSE) {
    plotList <- vector(mode = "list", length = length(selected_indicators) * 2)
    names(plotList) <- paste(rep(
      c("scatter", "line"),
      length(selected_indicators)
    ),
    rep(selected_indicators, each = 2),
    sep = "_"
    )
  } else if ((MSE == TRUE || CV == TRUE) && !(MSE == TRUE && CV == TRUE)) {
    plotList <- vector(mode = "list", length = length(selected_indicators) * 4)
    names(plotList) <- paste(rep(
      c("scatter", "line"),
      length(selected_indicators)
    ),
    rep(selected_indicators, each = 4),
    sep = "_"
    )
  } else if (MSE == TRUE && CV == TRUE) {
    plotList <- vector(mode = "list", length = length(selected_indicators) * 6)
    names(plotList) <- paste(rep(
      c("scatter", "line"),
      length(selected_indicators)
    ),
    rep(selected_indicators, each = 6),
    sep = "_"
    )
  }

  # scatter line
  for (ind in selected_indicators) {
    label_ind <- define_evallabel(type = type, label = label, indi = ind)

    if (is.null(object$smp_size)) {
      data_tmp <- data.frame(
        Direct = object[, paste0(ind, "_Direct")],
        Model_based = object[, paste0(ind, "_Model")]
      )
      label_ind$line["x_lab"] <- "Domains (unordered)"
    } else {
      data_tmp <- data.frame(
        Direct = object[, paste0(ind, "_Direct")],
        Model_based = object[, paste0(ind, "_Model")],
        smp_size = object$smp_size
      )
      data_tmp <- data_tmp[order(data_tmp$smp_size), ]
      data_tmp$smp_size <- NULL
    }

    data_tmp$ID <- seq_along(object$Domain)
    data_shaped <- melt(data_tmp, id.vars = "ID")
    names(data_shaped) <- c("ID", "Method", "value")


    print((plotList[[paste("scatter", ind, sep = "_")]] <-
      ggplot(data_tmp, aes(x = Direct, y = Model_based)) +
      geom_point(shape = shape[1]) +
      geom_smooth(
        method = lm, formula = y ~ x, se = FALSE,
        inherit.aes = FALSE,
        lty = line_type[1],
        aes(colour = "Reg. line", x = Direct, y = Model_based)
      ) +
      geom_abline(
        mapping = aes(
          colour = "Intersection",
          slope = slope, intercept = intercept
        ),
        data.frame(slope = 1, intercept = 0),
        lty = line_type[2]
      ) +
      xlim(
        min(min(data_tmp$Direct), min(data_tmp$Model_based)),
        max(max(data_tmp$Direct), max(data_tmp$Model_based))
      ) +
      ylim(
        min(min(data_tmp$Direct), min(data_tmp$Model_based)),
        max(max(data_tmp$Direct), max(data_tmp$Model_based))
      ) +
      ggtitle(label_ind$scatter["title"]) +
      ylab(label_ind$scatter["y_lab"]) +
      xlab(label_ind$scatter["x_lab"]) +
      scale_color_manual(name = "", values = c(
        "Intersection" = color[2],
        "Reg. line" = color[1]
      )) +
      gg_theme))

    cat("Press [enter] to continue")
    line <- readline()


    print((plotList[[paste("line", ind, sep = "_")]] <-
      ggplot(data = data_shaped, aes(
        x = ID,
        y = value, group = Method,
        colour = Method
      )) +
      geom_line(aes(linetype = Method), size = 0.7) +
      geom_point(aes(color = Method, shape = Method), size = 2) +
      scale_shape_manual(
        values = c(shape[1], shape[2]),
        breaks = c("Direct", "Model_based"),
        labels = c("Direct", "Model-based")
      ) +
      scale_linetype_manual(
        values = c(line_type[1], line_type[2]),
        breaks = c("Direct", "Model_based"),
        labels = c("Direct", "Model-based")
      ) +
      scale_color_manual(
        values = c(color[1], color[2]),
        breaks = c("Direct", "Model_based"),
        labels = c("Direct", "Model-based")
      ) +
      scale_fill_manual(
        name = "Method",
        breaks = c("Direct", "Model_based"),
        labels = c("Direct", "Model-based")
      ) +
      xlab(label_ind$line["x_lab"]) +
      ylab(label_ind$line["y_lab"]) +
      ggtitle(label_ind$line["title"]) +
      gg_theme))

    if (MSE == TRUE) {
      data_tmp2 <- data.frame(
        Direct = object[, paste0(ind, "_Direct_MSE")],
        Model_based = object[, paste0(ind, "_Model_MSE")],
        smp_size = object$smp_size
      )

      data_tmp2 <- data_tmp2[order(data_tmp2$smp_size, decreasing = TRUE), ]
      data_tmp2$smp_size <- NULL
      data_tmp2$ID <- seq_along(object$Domain)
      data_shaped <- melt(data_tmp2, id.vars = "ID")
      names(data_shaped) <- c("ID", "Method", "value")
      data_shaped$area <- rep(seq_len(NROW(data_tmp2$Direct)), 2)

      cat("Press [enter] to continue")
      line <- readline()

      print((plotList[[paste("boxplot", "MSE", ind, sep = "_")]] <-
        ggplot(data_shaped, aes(x = Method, y = value, fill = Method)) +
        geom_boxplot() +
        coord_flip() +
        labs(
          title = label_ind$boxplot_MSE["title"],
          x = label_ind$boxplot_MSE["x_lab"],
          y = label_ind$boxplot_MSE["y_lab"]
        ) +
        scale_fill_manual(
          name = "Method",
          values = color
        ) +
        gg_theme))

      cat("Press [enter] to continue")
      line <- readline()

      print((plotList[[paste("ordered", "MSE", ind, sep = "_")]] <-
        ggplot(data_shaped, aes(x = area, y = value, colour = Method)) +
        geom_point(aes(color = Method, shape = Method)) +
        labs(
          title = label_ind$ordered_MSE["title"],
          x = label_ind$ordered_MSE["x_lab"],
          y = label_ind$ordered_MSE["y_lab"]
        ) +
        scale_color_manual(values = color)) +
        scale_shape_manual(values = c(shape[1], shape[2])) + gg_theme)
    }

    if (CV == TRUE) {
      data_tmp3 <- data.frame(
        Direct = object[, paste0(ind, "_Direct_CV")],
        Model_based = object[, paste0(ind, "_Model_CV")],
        smp_size = object$smp_size2
      )

      data_tmp3 <- data_tmp3[order(data_tmp3$smp_size, decreasing = TRUE), ]
      data_tmp3$smp_size <- NULL
      data_tmp3$ID <- seq_along(object$Domain)
      data_shaped <- melt(data_tmp3, id.vars = "ID")
      names(data_shaped) <- c("ID", "Method", "value")
      data_shaped$area <- rep(seq_len(NROW(data_tmp3$Direct)), 2)

      cat("Press [enter] to continue")
      line <- readline()

      print((plotList[[paste("boxplot", "CV", ind, sep = "_")]] <-
        ggplot(data_shaped, aes(x = Method, y = value, fill = Method)) +
        geom_boxplot() +
        coord_flip() +
        labs(
          title = label_ind$boxplot_CV["title"],
          x = label_ind$boxplot_CV["x_lab"],
          y = label_ind$boxplot_CV["y_lab"]
        ) +
        scale_fill_manual(
          name = "Method",
          values = color
        )) + gg_theme)

      cat("Press [enter] to continue")
      line <- readline()

      data_shaped

      print((plotList[[paste("ordered", "CV", ind, sep = "_")]] <-
        ggplot(data_shaped, aes(x = area, y = value, colour = Method)) +
        geom_point(aes(color = Method, shape = Method)) +
        labs(
          title = label_ind$ordered_CV["title"],
          x = label_ind$ordered_CV["x_lab"],
          y = label_ind$ordered_CV["y_lab"]
        ) +
        scale_color_manual(values = color)) +
        scale_shape_manual(values = c(shape[1], shape[2])) + gg_theme)
    }


    if (!ind == tail(selected_indicators, 1)) {
      cat("Press [enter] to continue")
      line <- readline()
    }
  }
  invisible(plotList)
}

define_evallabel <- function(type, label, indi) {
  if (!inherits(label, "list")) {
    if (label == "orig") {
      if (type == "unit") {
        label <- list(
          scatter = c(
            title = indi,
            y_lab = "Model-based",
            x_lab = "Direct"
          ),
          line = c(
            title = indi,
            y_lab = "Value",
            x_lab = "Domain (ordered by sample size)"
          ),
          boxplot_MSE = c(
            title = indi,
            y_lab = "MSE",
            x_lab = ""
          ),
          ordered_MSE = c(
            title = indi,
            y_lab = "MSE",
            x_lab = "Domain (ordered by
                                      sample size)"
          ),
          boxplot_CV = c(
            title = indi,
            y_lab = "CV",
            x_lab = ""
          ),
          ordered_CV = c(
            title = indi,
            y_lab = "CV",
            x_lab = "Domain (ordered by sample size)"
          )
        )
      } else if (type == "area") {
        label <- list(
          scatter = c(
            title = indi,
            y_lab = "Model-based",
            x_lab = "Direct"
          ),
          line = c(
            title = indi,
            y_lab = "Value",
            x_lab = "Domain (ordered by decreasing MSE
                               of Direct)"
          ),
          boxplot_MSE = c(
            title = indi,
            y_lab = "",
            x_lab = "MSE"
          ),
          ordered_MSE = c(
            title = indi,
            y_lab = "MSE",
            x_lab = "Domain (ordered by increasing
                                      MSE of Direct)"
          ),
          boxplot_CV = c(
            title = indi,
            y_lab = "CV",
            x_lab = ""
          ),
          ordered_CV = c(
            title = indi,
            y_lab = "CV",
            x_lab = "Domain (ordered by increasing CV
                                     of Direct)"
          )
        )
      }
    } else if (label == "blank") {
      label <- list(
        scatter = c(
          title = "",
          y_lab = "",
          x_lab = ""
        ),
        line = c(
          title = "",
          y_lab = "",
          x_lab = ""
        ),
        boxplot_MSE = c(
          title = "",
          y_lab = "",
          x_lab = ""
        ),
        ordered_MSE = c(
          title = "",
          y_lab = "",
          x_lab = ""
        ),
        boxplot_CV = c(
          title = "",
          y_lab = "",
          x_lab = ""
        ),
        ordered_CV = c(
          title = "",
          y_lab = "",
          x_lab = ""
        )
      )
    } else if (label == "no_title") {
      if (type == "unit") {
        label <- list(
          scatter = c(
            title = "",
            y_lab = "Model-based",
            x_lab = "Direct"
          ),
          line = c(
            title = "",
            y_lab = "Value",
            x_lab = "Domain (ordered by sample size)"
          ),
          boxplot_MSE = c(
            title = "",
            y_lab = "MSE",
            x_lab = ""
          ),
          ordered_MSE = c(
            title = "",
            y_lab = "MSE",
            x_lab = "Domain (ordered by sample
                                      size)"
          ),
          boxplot_CV = c(
            title = "",
            y_lab = "CV",
            x_lab = ""
          ),
          ordered_CV = c(
            title = "",
            y_lab = "CV",
            x_lab = "Domain (ordered by sample size)"
          )
        )
      } else if (type == "area") {
        label <- list(
          scatter = c(
            title = "",
            y_lab = "Model-based",
            x_lab = "Direct"
          ),
          line = c(
            title = "",
            y_lab = "Value",
            x_lab = "Domain (ordered by decreasing MSE of
                               Direct)"
          ),
          boxplot_MSE = c(
            title = "",
            y_lab = "",
            x_lab = "MSE"
          ),
          ordered_MSE = c(
            title = "",
            y_lab = "MSE",
            x_lab = "Domain (ordered by increasing MSE
                                      of Direct)"
          ),
          boxplot_CV = c(
            title = "",
            y_lab = "CV",
            x_lab = ""
          ),
          ordered_CV = c(
            title = "",
            y_lab = "CV",
            x_lab = "Domain (ordered by increasing CV
                                     of Direct)"
          )
        )
      }
    }
  }
  return(label)
}
