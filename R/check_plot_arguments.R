plot_check <- function(x, label, color, cooks, range) {
  if (!any(inherits(x, which = TRUE, c("ebp", "fh")))) {
    stop(strwrap(prefix = " ", initial = "",
                 "First object needs to be of class emdi, model. For emdi
                 objects obtained by direct estimation diagnostic plots are not
                 reasonable."))
  }
  if (is.null(label) || (!(label == "orig" || label == "no_title" ||
    label == "blank") && !inherits(label, "list"))) {
    stop(strwrap(prefix = " ", initial = "",
                 "label can be either one of the following characters 'orig',
                 'no_title' or 'blank' or a list as specified in help(plot.emdi)."))
  }
  if (length(color) != 2 || !is.vector(color)) {
    stop(strwrap(prefix = " ", initial = "",
                 "color needs to be a vector of length 2 defining the two
                 colors for the diagnostic plot. See also help(plot.emdi)."))
  }
  if (!(inherits(cooks, "logical") && length(cooks) == 1)) {
    stop(strwrap(prefix = " ", initial = "",
                 "cooks needs to be a logical value. Set na.rm to TRUE or
                 FALSE. See also help(plot.emdi)."))
  }
  if (!is.null(range) && !inherits(range, "numeric")) {
    stop(strwrap(prefix = " ", initial = "",
                 "range must be a sequence determining the range of the x-axis
                 for plots of the optimal parameter.. Set na.rm to TRUE or
                 FALSE. See also help(plot.emdi)."))
  }
}
