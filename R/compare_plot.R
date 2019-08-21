#' Shows plots for the comparison of estimates
#'
#' For all indicators or a selection of indicators two plots are returned. The
#' first plot is a scatter plot of estimates to compare and the second is a line
#' plot with these estimates.
#' @param direct optional, an object of type "emdi","direct", representing point
#' and MSE estimates.
#' @param model an object of type "emdi","model", representing point and MSE
#' estimates.
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
#' @return A scatter plot and a line plot comparing direct and model-based
#' estimators for each selected indicator obtained by \code{\link[ggplot2]{ggplot}}.
#' @export

compare_plot <- function(object, indicator, ...) UseMethod("compare_plot")



#' Shows plots for the comparison of estimates
#'
#' For all indicators or a selection of indicators two plots are returned. The
#' first plot is a scatter plot of estimates to compare and the second is a line
#' plot with these estimates.
#' @param direct optional, an object of type "emdi","direct", representing point
#' and MSE estimates.
#' @param model an object of type "emdi","model", representing point and MSE
#' estimates.
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
#' @return A scatter plot and a line plot comparing direct and model-based
#' estimators for each selected indicator obtained by \code{\link[ggplot2]{ggplot}}.
#' @keywords internal

compare_plot_ebp <- function(direct, model, indicator = "all", MSE = FALSE,
                             CV = FALSE, label = "orig",
                             color = c("blue", "lightblue3"),
                             shape = c(16, 16), line_type = c("solid", "solid"),
                             gg_theme = NULL) {

  Direct <- NULL
  Model_based <- NULL
  ID <- NULL
  value <- NULL
  Method <- NULL

  #compare_plot_check(direct = direct, model = model, indicator = indicator,
  #                   label = label, color = color, shape = shape,
  #                   line_type = line_type, gg_theme = gg_theme)


  ind_direct <- point_emdi(object = direct, indicator = indicator)$ind
  selected_direct <- colnames(ind_direct)[-1]
  colnames(ind_direct) <- c("Domain", paste0(colnames(ind_direct)[-1], "_Direct"))

  ind_model <- point_emdi(object = model, indicator = indicator)$ind
  selected_model <- colnames(ind_model)[-1]
  colnames(ind_model) <- c("Domain", paste0(colnames(ind_model)[-1], "_Model"))
  smp_size <- (table(direct$framework$smp_domains_vec))

  #compare_plot_check2(ind_direct, ind_model)

  Data <- merge(ind_direct, ind_model, by = "Domain" )

  matcher <- match(Data$Domain, names(smp_size))
  Data$smp_size <- as.numeric(smp_size)[matcher]

  if (MSE == TRUE || CV == TRUE ) {
    precisions_direct <- mse_emdi(object = direct, indicator = indicator, CV = TRUE)
    colnames(precisions_direct$ind) <- paste0(colnames(precisions_direct$ind), "Direct_MSE")
    colnames(precisions_direct$ind_cv) <- paste0(colnames(precisions_direct$ind_cv), "Direct_CV")

    precisions_model <- mse_emdi(object = model, indicator = indicator, CV = TRUE)
    colnames(precisions_model$ind) <- paste0(colnames(precisions_model$ind), "Direct_MSE")
    colnames(precisions_model$ind_cv) <- paste0(colnames(precisions_model$ind_cv), "Direct_CV")

    if (MSE == TRUE) {
      Data <- merge(Data, precisions_direct$ind, id = "Domain")
      Data <- merge(Data, precisions_model$ind, id = "Domain")
    }
    if (CV == TRUE) {
      Data <- merge(Data, precisions_direct$ind_cv, id = "Domain")
      Data <- merge(Data, precisions_model$ind_cv, id = "Domain")
    }

  }

  selected_indicators <- selected_model[selected_model %in% selected_direct]

  compare_plots(object = Data, type = "unit", selected_indicators = selected_indicators,
                MSE = MSE, CV = CV, label = label, color = color,
                shape = shape, line_type = line_type, gg_theme = gg_theme)

}


#' Shows plots for the comparison of estimates
#'
#' For all indicators or a selection of indicators two plots are returned. The
#' first plot is a scatter plot of estimates to compare and the second is a line
#' plot with these estimates.
#' @param direct optional, an object of type "emdi","direct", representing point
#' and MSE estimates.
#' @param model an object of type "emdi","model", representing point and MSE
#' estimates.
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
#' @return A scatter plot and a line plot comparing direct and model-based
#' estimators for each selected indicator obtained by \code{\link[ggplot2]{ggplot}}.
#' @keywords internal

compare_plot_fh <- function(direct, model, indicator = "all", MSE = FALSE, CV = FALSE,
                            label = "orig",
                             color = c("blue", "lightblue3"),
                             shape = c(16, 16), line_type = c("solid", "solid"),
                             gg_theme = NULL) {

  Direct <- NULL
  Model_based <- NULL
  ID <- NULL
  value <- NULL
  Method <- NULL

  #compare_plot_check(direct = direct, model = model, indicator = indicator,
  #                   label = label, color = color, shape = shape,
  #                   line_type = line_type, gg_theme = gg_theme)

  Data <- direct$ind[direct$ind$ind == 0,]
  names(Data) <- c("Domain", "FH_Direct", "FH_Model", "ind")
  if (is.null(direct$MSE)) {
    Data$smp_size <- NULL
  } else {
    Data$smp_size <- -direct$MSE$Direct[direct$MSE$ind == 0]
  }
  selected_indicators <- "FH"

  if (MSE == TRUE || CV == TRUE) {
    all_precisions <- mse_emdi(object = direct, indicator = "all", CV = TRUE)
    colnames(all_precisions$ind) <- c("Domain", paste0(c("FH_Direct", "FH_Model"), "_MSE"))
    colnames(all_precisions$ind_cv) <- c("Domain", paste0(c("FH_Direct", "FH_Model"), "_CV"))
    combined <- merge(all_precisions$ind, all_precisions$ind_cv, id = "Domain")
    combined <- combined[!is.na(combined$FH_Direct_MSE), ]

    Data <- merge(Data, combined, id = "Domain")
    Data$smp_size <- -Data$FH_Direct_MSE
    Data$smp_size2 <- -Data$FH_Direct_CV
  }

  #compare_plot_check2(ind_direct, ind_model)

  compare_plots(object = Data, type = "area", selected_indicators = selected_indicators,
                MSE = MSE, CV = CV, label = label, color = color,
                shape = shape, line_type = line_type, gg_theme = gg_theme)

}


#' Shows plots for the comparison of estimates
#'
#' For all indicators or a selection of indicators two plots are returned. The
#' first plot is a scatter plot of estimates to compare and the second is a line
#' plot with these estimates.
#' @param direct optional, an object of type "emdi","direct", representing point
#' and MSE estimates.
#' @param model an object of type "emdi","model", representing point and MSE
#' estimates.
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
#' @return A scatter plot and a line plot comparing direct and model-based
#' estimators for each selected indicator obtained by \code{\link[ggplot2]{ggplot}}.
#' @export
#' @importFrom reshape2 melt
#' @importFrom ggplot2 geom_point geom_smooth geom_line geom_boxplot
#' @importFrom ggplot2 aes xlim ylim scale_shape_manual scale_linetype_manual coord_flip
#' @importFrom ggplot2 scale_color_manual scale_fill_manual


compare_plot.emdi <- function(direct, model = NULL, indicator = "all",
                              MSE = FALSE, CV = FALSE, label = "orig",
                            color = c("blue", "lightblue3"),
                            shape = c(16, 16), line_type = c("solid", "solid"),
                            gg_theme = NULL) {

  if(inherits(direct, "direct") & inherits(model, "ebp")) {
    compare_plot_ebp(direct = direct, model = model, indicator = indicator,
                     MSE = MSE, CV = CV,
                     label = label, color = color, shape = shape,
                     line_type = line_type, gg_theme = gg_theme)
  } else if(inherits(direct, "fh")) {
    compare_plot_fh(direct = direct, model = model, indicator = indicator,
                    MSE = MSE, CV = CV,
                     label = label, color = color, shape = shape,
                     line_type = line_type, gg_theme = gg_theme)
  }

}



define_evallabel <- function(type, label, indi){
  if (!inherits(label, "list")) {
    if (label == "orig") {

      if (type == "unit") {
        label <- list(scatter = c(title = indi,
                                  y_lab = "Model-based",
                                  x_lab = "Direct"),
                      line = c(title = indi,
                               y_lab = "Value",
                               x_lab = "Domain (ordered by sample size)"),
                      boxplot_MSE = c(title = indi,
                               y_lab = "MSE",
                               x_lab = "Domain (ordered by sample size)"),
                      ordered_MSE = c(title = indi,
                               y_lab = "MSE",
                               x_lab = "Domain (ordered by sample size)"),
                      boxplot_CV = c(title = indi,
                                  y_lab = "CV",
                                  x_lab = "Domain (ordered by sample size)"),
                      ordered_CV = c(title = indi,
                                  y_lab = "CV",
                                  x_lab = "Domain (ordered by sample size)"))
      } else if (type == "area") {
        label <- list(scatter = c(title = indi,
                                  y_lab = "Model-based",
                                  x_lab = "Direct"),
                      line = c(title = indi,
                               y_lab = "Value",
                               x_lab = "Domain (ordered by increasing MSE of Direct)"),
                      boxplot_MSE = c(title = indi,
                                      y_lab = "",
                                      x_lab = "MSE"),
                      ordered_MSE = c(title = indi,
                                      y_lab = "MSE",
                                      x_lab = "Domain (ordered by increasing MSE of Direct)"),
                      boxplot_CV = c(title = indi,
                                     y_lab = "CV",
                                     x_lab = ""),
                      ordered_CV = c(title = indi,
                                     y_lab = "CV",
                                     x_lab = "Domain (ordered by increasing CV of Direct)"))
      }

    } else if (label == "blank") {
      label <- list(scatter = c(title = "",
                               y_lab = "",
                               x_lab = ""),
                    line = c(title = "",
                               y_lab = "",
                               x_lab = ""),
                    boxplot_MSE = c(title = "",
                             y_lab = "",
                             x_lab = ""),
                    ordered_MSE = c(title = "",
                             y_lab = "",
                             x_lab = ""),
                    boxplot_CV = c(title = "",
                             y_lab = "",
                             x_lab = ""),
                    ordered_CV = c(title = "",
                             y_lab = "",
                             x_lab = ""))
    } else if (label == "no_title") {

      if (type == "unit") {
        label <- list(scatter = c(title = "",
                                  y_lab = "Model-based",
                                  x_lab = "Direct"),
                      line = c(title = "",
                               y_lab = "Value",
                               x_lab = "Domain (ordered by sample size)"),
                      boxplot_MSE = c(title = "",
                                      y_lab = "MSE",
                                      x_lab = "Domain (ordered by sample size)"),
                      ordered_MSE = c(title = "",
                                      y_lab = "MSE",
                                      x_lab = "Domain (ordered by sample size)"),
                      boxplot_CV = c(title = "",
                                     y_lab = "CV",
                                     x_lab = "Domain (ordered by sample size)"),
                      ordered_CV = c(title = "",
                                     y_lab = "CV",
                                     x_lab = "Domain (ordered by sample size)"))
      } else if (type == "area") {
        label <- list(scatter = c(title = "",
                                  y_lab = "Model-based",
                                  x_lab = "Direct"),
                      line = c(title = "",
                               y_lab = "Value",
                               x_lab = "Domain (ordered by increasing MSE of Direct)"),
                      boxplot_MSE = c(title = "",
                                      y_lab = "",
                                      x_lab = "MSE"),
                      ordered_MSE = c(title = "",
                                      y_lab = "MSE",
                                      x_lab = "Domain (ordered by increasing MSE of Direct)"),
                      boxplot_CV = c(title = "",
                                     y_lab = "CV",
                                     x_lab = ""),
                      ordered_CV = c(title = "",
                                     y_lab = "CV",
                                     x_lab = "Domain (ordered by increasing CV of Direct)"))
      }

    }

  }

  #else if (inherits(label, "list")) {

  #  if (!any(names(label) %in% c("scatter", "line"))) {
  #    stop("List elements must have following names even though not
  #         all must be included: scatter and line Every list element must
  #         have the elements title, y_lab and x_lab.")
  #  }
  #  for (i in names(label)) {
  #    if (!all(names(label[[i]]) == c("title", "y_lab", "x_lab"))) {
  #      stop("Every list element must have the elements title,
  #           y_lab and x_lab in this order.")
  #    }
  #    }

  #  if (type == "unit") {
  #    orig_label <- list(scatter = c(title = indi,
  #                              y_lab = "Model-based",
  #                              x_lab = "Direct"),
  #                  line = c(title = indi,
  #                           y_lab = "Value",
  #                           x_lab = "Domain (ordered by sample size)"),
  #                  boxplot_MSE = c(title = indi,
  #                                  y_lab = "Value",
  #                                  x_lab = "Domain (ordered by sample size)"),
  #                  ordered_MSE = c(title = indi,
  #                                  y_lab = "Value",
  #                                  x_lab = "Domain (ordered by sample size)"),
  #                  boxplot_CV = c(title = indi,
  #                                 y_lab = "Value",
  #                                 x_lab = "Domain (ordered by sample size)"),
  #                  ordered_CV = c(title = indi,
  #                                 y_lab = "Value",
  #                                 x_lab = "Domain (ordered by sample size)"))
  #  } else if (type == "area") {
  #    orig_label <- list(scatter = c(title = indi,
  #                              y_lab = "Model-based",
  #                              x_lab = "Direct"),
  #                  line = c(title = indi,
  #                           y_lab = "Value",
  #                           x_lab = "Domain (ordered by sample size)"),
  #                  boxplot_MSE = c(title = indi,
  #                                  y_lab = "",
  #                                  x_lab = "MSE"),
  #                  ordered_MSE = c(title = indi,
  #                                  y_lab = "MSE",
  #                                  x_lab = "Domain (ordered by increasing MSE of Direct)"),
  #                  boxplot_CV = c(title = indi,
  #                                 y_lab = "CV",
  #                                 x_lab = ""),
  #                  ordered_CV = c(title = indi,
  #                                 y_lab = "Value",
  #                                 x_lab = "Domain (ordered by increasing CV of Direct)"))
  #  }


  #  if (any(names(label) == "scatter")) {
  #    label$scatter <- label$scatter
  #  } else {
   #   label$scatter <- orig_label$scatter
  #  }
  #  if (any(names(label) == "line")) {
  #    label$line <- label$line
  #  } else {
  #    label$line <- orig_label$line
  #  }
  #    }

  #if (any(!(names(label) %in%  c("scatter", "line")))) {
  #  warning("One or more list elements are not called scatter or line. The
  #           changes are for this/these element(s) is/are not done. Instead the
  #          original labels are used.")
  #}

  return(label)
    }


compare_plots <- function(object, type, selected_indicators, MSE, CV, label, color,
                          shape, line_type, gg_theme,...) {


  if (MSE == FALSE & CV == FALSE) {
    plotList <- vector(mode = "list", length = length(selected_indicators) * 2)
    names(plotList) <- paste(rep(c("scatter", "line"), length(selected_indicators)),
                             rep(selected_indicators, each = 2), sep = "_")
  } else if ((MSE == TRUE | CV == TRUE) & !(MSE == TRUE & CV == TRUE)) {
    plotList <- vector(mode = "list", length = length(selected_indicators) * 4)
    names(plotList) <- paste(rep(c("scatter", "line"), length(selected_indicators)),
                             rep(selected_indicators, each = 4), sep = "_")
  } else if (MSE == TRUE & CV == TRUE) {
    plotList <- vector(mode = "list", length = length(selected_indicators) * 6)
    names(plotList) <- paste(rep(c("scatter", "line"), length(selected_indicators)),
                             rep(selected_indicators, each = 6), sep = "_")
  }

  #scatter line
  for (ind in selected_indicators) {

    label_ind <- define_evallabel(type = type, label = label, indi = ind)

    if (is.null(object$smp_size)) {
      data_tmp <- data.frame(Direct = object[, paste0(ind, "_Direct")],
                             Model_based = object[, paste0(ind, "_Model")])
      label_ind$line["x_lab"] <- "Domains (unordered)"
    } else {
      data_tmp <- data.frame(Direct = object[, paste0(ind, "_Direct")],
                             Model_based = object[, paste0(ind, "_Model")],
                             smp_size = object$smp_size)
      data_tmp <- data_tmp[order(data_tmp$smp_size), ]
      data_tmp$smp_size <- NULL
    }


    print((plotList[[paste("scatter", ind, sep = "_")]] <- ggplot(data_tmp,
                                                                  aes(x = Direct, y = Model_based)) + geom_point() +
             geom_smooth(method = lm, color = color[1], se = FALSE) +
             geom_abline(intercept = 0, slope = 1, size = 1, color = color[2]) +
             xlim(min(min(data_tmp$Direct), min(data_tmp$Model_based)),
                  max(max(data_tmp$Direct), max(data_tmp$Model_based))) +
             ylim(min(min(data_tmp$Direct), min(data_tmp$Model_based)),
                  max(max(data_tmp$Direct), max(data_tmp$Model_based))) +
             ggtitle(label_ind$scatter["title"]) + ylab(label_ind$scatter["y_lab"]) +
             xlab(label_ind$scatter["x_lab"]) + gg_theme))
    cat("Press [enter] to continue")
    line <- readline()


    data_tmp$ID <- seq_along(object$Domain)
    data_shaped <- melt(data_tmp, id.vars = "ID")
    names(data_shaped) <- c("ID", "Method", "value")

    print((plotList[[paste("line", ind, sep = "_")]] <-
             ggplot(data = data_shaped, aes(x = ID,
                                            y = value, group = Method,
                                            colour = Method)) +
             geom_line(aes(linetype = Method), size = 0.7) +
             geom_point(aes(color = Method, shape = Method), size = 2) +
             scale_shape_manual(values = c(shape[1], shape[2]),
                                breaks = c("Direct", "Model_based"),
                                labels = c("Direct", "Model-based")) +
             scale_linetype_manual(values = c(line_type[1], line_type[2]),
                                   breaks = c("Direct", "Model_based"),
                                   labels = c("Direct", "Model-based")) +
             scale_color_manual(values = c(color[1], color[2]),
                                breaks = c("Direct", "Model_based"),
                                labels = c("Direct", "Model-based")) +
             scale_fill_manual(name = "Method",
                               breaks = c("Direct", "Model_based"),
                               labels = c("Direct", "Model-based")) +
             xlab(label_ind$line["x_lab"]) + ylab(label_ind$line["y_lab"]) +
             ggtitle(label_ind$line["title"]) + gg_theme))

    if (MSE == TRUE) {

      data_tmp2 <- data.frame(Direct = object[, paste0(ind, "_Direct_MSE")],
                              Model_based = object[, paste0(ind, "_Model_MSE")],
                              smp_size = object$smp_size)

      data_tmp2 <- data_tmp2[order(data_tmp2$smp_size, decreasing = TRUE), ]
      data_tmp2$smp_size <- NULL
      data_tmp2$ID <- seq_along(object$Domain)
      data_shaped <- melt(data_tmp2, id.vars = "ID")
      names(data_shaped) <- c("ID", "Method", "value")
      data_shaped$area <- rep(1:NROW(data_tmp2$Direct), 2)

      cat("Press [enter] to continue")
      line <- readline()

      print((plotList[[paste("boxplot", "MSE", ind, sep = "_")]] <- ggplot(data_shaped,
                                                                        aes(x = Method, y = value,
                                                                            fill = Method)) +
               geom_boxplot() +
               coord_flip() +
               labs(title = label_ind$boxplot_MSE["title"],
                    x = label_ind$boxplot_MSE["x_lab"],
                    y = label_ind$boxplot_MSE["y_lab"]) +
               scale_fill_manual(name = "Method",
                                 values = color)))

      cat("Press [enter] to continue")
      line <- readline()

      print((plotList[[paste("ordered", "MSE", ind, sep = "_")]] <- ggplot(data_shaped,
                                                                        aes(x = area,
                                                                            y = value,
                                                                            colour = Method)) +
               geom_point(aes(color = Method, shape = Method)) +
               labs(title = label_ind$ordered_MSE["title"],
                                   x = label_ind$ordered_MSE["x_lab"],
                                   y = label_ind$ordered_MSE["y_lab"]) +
               scale_color_manual(values = color)) +
        scale_shape_manual(values = c(shape[1], shape[2])))
    }

    if (CV == TRUE) {

      data_tmp3 <- data.frame(Direct = object[, paste0(ind, "_Direct_CV")],
                              Model_based = object[, paste0(ind, "_Model_CV")],
                              smp_size = object$smp_size2)

      data_tmp3 <- data_tmp3[order(data_tmp3$smp_size, decreasing = TRUE), ]
      data_tmp3$smp_size <- NULL
      data_tmp3$ID <- seq_along(object$Domain)
      data_shaped <- melt(data_tmp3, id.vars = "ID")
      names(data_shaped) <- c("ID", "Method", "value")
      data_shaped$area <- rep(1:NROW(data_tmp3$Direct), 2)

      cat("Press [enter] to continue")
      line <- readline()

      print((plotList[[paste("boxplot", "CV", ind, sep = "_")]] <- ggplot(data_shaped,
                                                                        aes(x = Method, y = value,
                                                                            fill = Method)) +
               geom_boxplot() +
               coord_flip() +
               labs(title = label_ind$boxplot_CV["title"],
                    x = label_ind$boxplot_CV["x_lab"],
                    y = label_ind$boxplot_CV["y_lab"]) +
               scale_fill_manual(name = "Method",
                                 values = color)))

      cat("Press [enter] to continue")
      line <- readline()

      data_shaped

      print((plotList[[paste("ordered", "CV", ind, sep = "_")]] <- ggplot(data_shaped,
                                                                        aes(x = area,
                                                                            y = value,
                                                                            colour = Method)) +
               geom_point(aes(color = Method, shape = Method)) +
               labs(title = label_ind$ordered_CV["title"],
                                   x = label_ind$ordered_CV["x_lab"],
                                   y = label_ind$ordered_CV["y_lab"]) +
               scale_color_manual(values = color))+
        scale_shape_manual(values = c(shape[1], shape[2])))
    }


    if (!ind == tail(selected_indicators, 1)) {
      cat("Press [enter] to continue")
      line <- readline()
    }
  }
  invisible(plotList)
}

