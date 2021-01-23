#' @rdname compare_plots_emdi
#' @export
compare_plot.direct <- function(model = NULL, direct = NULL, indicator = "all",
                              MSE = FALSE, CV = FALSE, label = "orig",
                              color = c("blue", "lightblue3"),
                              shape = c(16, 16), line_type = c("solid", "solid"),
                              gg_theme = NULL, ...) {
  
  compare_plot_check(model = model, indicator = indicator,
                     label = label, color = color, shape = shape,
                     line_type = line_type, gg_theme = gg_theme)
  
  if (inherits(direct, "ebp") || inherits(direct, "fh")) {
    warning("Wrong use of the compare plot interface. Model needs to be either ",
            "of class ebp or fh. compare_plot is now called with the arguments ",
            "swapped.")
    compare_plot(model = direct, direct = model, indicator = indicator,
                 MSE = MSE, CV = CV, label = label,
                 color = color,
                 shape = shape, line_type = line_type,
                 gg_theme = gg_theme, ... = ...)
  } else {
    stop("model needs to be either an ebp or a fh type model.")
  }
  
  
 
}