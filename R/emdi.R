#' A package for estimating and mapping disaggregated indicators
#'
#' The package \pkg{emdi} supports estimating and mapping regional
#' disaggregated indicators. For estimating these indicators, direct estimation,
#' the unit-level Empirical Best Prediction approach by
#' \cite{Molina and Rao (2010)}, the extension for data under informative
#' selection by \cite{Guadarrama et al. (2018)}, the area-level model by
#' \cite{Fay and Herriot (1979)} and various extensions of it (adjusted variance
#' estimation methods, log and arcsin transformation, spatial, robust and
#' measurement error models) are provided. Depending on the particular method,
#' analytical, bootstrap and jackknife MSE estimation approaches are
#' implemented. The assessment of the used model is supported by a summary and
#' diagnostic plots. For a suitable presentation of estimates, map plots can be
#' easily created. Furthermore, results can easily be exported to Excel.
#' Additionally, for the area-level models a stepwise variable selection
#' function, benchmarking options and spatial correlation tests are provided.
#'
#' @details
#' The three estimation functions are called \code{\link{direct}},
#' \code{\link{ebp}} and \code{\link{fh}}. For all functions, several methods
#' are available as \code{\link{estimators.emdi}} and
#' \code{\link{emdi_summaries}}. For a full list, please see
#' \code{\link{emdiObject}}. Furthermore, functions \code{\link{map_plot}} and
#' \code{\link{write.excel}} help to visualize and export results. An overview
#' of all currently provided functions can be requested by
#' \code{library(help=emdi)}.
#'
#' @references
#' Battese, G.E., Harter, R.M. and Fuller, W.A. (1988). An Error-Components
#' Model for Predictions of County Crop Areas Using Survey and Satellite Data.
#' Journal of the American Statistical Association, Vol.83, No. 401,
#' 28-36. \cr \cr
#' Fay, R. E. and Herriot, R. A. (1979), Estimates of income for small places:
#' An application of James-Stein procedures to census data, Journal of the
#' American Statistical Association 74(366), 269-277. \cr \cr
#' Kreutzmann, A., Pannier, S., Rojas-Perilla, N., Schmid, T., Templ, M.
#' and Tzavidis, N. (2019). The R Package emdi for Estimating and
#' Mapping Regionally Disaggregated Indicators, Journal of Statistical Software,
#' Vol. 91, No. 7, 1--33, <doi:10.18637/jss.v091.i07> \cr \cr
#' Molina, I. and Rao, J.N.K. (2010). Small area estimation of poverty
#' indicators. The Canadian Journal of Statistics, Vol. 38, No.3, 369-385.
#' Guadarrama, M., Molina, I. and Rao, J.N.K. (2018). Small area estimation of
#' general parameters under complex sampling designs. Computational Statistics &
#' Data Analysis, Vol. 121, 20-40.
#' _PACKAGE
#' @name emdi
NULL
