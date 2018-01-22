#' A package for estimating and mapping disaggregated indicators
#'
#' The package \pkg{emdi} supports estimating and mapping regional
#' disaggregated indicators. For estimating these indicators direct estimation 
#' and the Empirical Best Prediction approach by \cite{Molina and Rao (2010)} 
#' are provided. Estimates of the mean squared error for these methods can be 
#' conducted by using a parametric bootstrap approach (\cite{Gonzalez-Manteiga et al. 2008}). 
#' Furthermore, a mapping tool for plotting the estimates on their geographic 
#' regions is provided. Point and uncertainty measures as well as diagnostic 
#' tests can be easily extracted to excel.
#'
#' @details
#' The two estimation functions are called \code{\link{direct}} and 
#' \code{\link{ebp}}. For both functions several methods are available as 
#' \code{\link{estimators.emdi}}, \code{\link{plot.emdi}} (only for emdi objects 
#' obtained by function \code{ebp}), \code{\link{print.emdi}} and 
#' \code{\link{summary.emdi}}. Furthermore, functions \code{\link{map_plot}} and
#' \code{\link{write.excel}} help to visualize and export results.
#' 
#' An overview of all currently provided functions can be requested by
#' \code{library(help=emdi)}.
#'
#' @references
#' Battese, G.E., Harter, R.M. and Fuller, W.A. (1988). An Error-Components
#' Model for Predictions of County Crop Areas Using Survey and Satellite Data.
#' Journal of the American Statistical Association, Vol.83, No. 401, 28-36. \cr \cr
#' Gonzalez-Manteiga, W. et al. (2008). Bootstrap mean squared error of
#' a small-area EBLUP. Journal of Statistical Computation and Simulation,
#' 78:5, 443-462. \cr \cr
#' Molina, I. and Rao, J.N.K. (2010). Small area estimation of poverty
#' indicators. The Canadian Journal of Statistics, Vol. 38, No.3, 369-385.
#' @docType package
#' @name emdi
NULL
