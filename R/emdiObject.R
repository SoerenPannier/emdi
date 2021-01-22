#' Fitted emdiObject
#'
#' An object of class emdi that represents point predictions of regional
#' disaggregated indicators. Optionally, it also contains corresponding MSE
#' estimates. Three different estimation approaches are implemented: 
#' direct estimation (class 'direct), the Fay-Herriot model (class "fh"), and 
#' the empirical best prediction (class "ebp"). Objects of these classes have 
#' methods for various generic functions. See Details for more information.
#'
#' @return
#' The following components are always included in an emdi object but not
#' always filled and with different components depending on the estimation
#' approach:
#' \item{\code{call}}{the function call that produced the object.}
#' \item{\code{fixed}}{for details, see \code{fixed} in \code{\link{fh}} and
#'              \code{\link{ebp}}. Not filled for class direct.}
#'  \item{\code{framework}}{a list with components that describe the data
#'  setup, e.g., number of domains in the sample.}
#' \item{\code{ind}}{data frame containing estimates for indicators per domain.}
#' \item{\code{method}}{character returning the method for the estimation of
#'    the optimal lambda (for class ebp), here "reml", or a list returning method for
#'    the estimation of the variance of the random effect and the applied MSE
#'    estimation (for class fh). Not filled for class direct.}
#' \item{\code{model}}{list containing a  selection of model components.
#'    Not filled for class direct.}
#' \item{\code{MSE}}{data frame containing MSE estimates corresponding to the
#' point predictions in \code{ind} per indicator per domain if MSE is selected
#' in function call. If \code{FALSE}, \code{MSE} is \code{NULL}.}
#' \item{\code{transformation}}{character or list containing information about applied
#' transformation and, if appropriate, backtransformation. Not filled for class direct.}
#' \item{\code{transform_param}}{a list with two elements, \code{optimal_lambda}
#'    and \code{shift_par}, where the first contains the optimal parameter for a
#'    Box-Cox transformation or NULL for no and log transformation and the
#'    second the potential shift parameter in the log or Box-Cox transformation
#'    and NULL for no transformation. Not filled for class fh and direct.}
#' \item{\code{successful_bootstraps}}{for class direct, a matrix with domains as 
#'  rows and indicators as columns. The cells contain the number of successful 
#'  bootstraps for each combination. For non-robust spatial Fay-Herriot, string
#'  with number of successful bootstraps. Not filled for other models.}
#' @details 
#' Objects of class "emdi" have following methods: \code{\link[emdi]{comparePred.emdi}}, 
#' \code{\link[emdi]{confint.emdi}}, 
#' \code{\link[emdi]{estimators}}, \code{\link[emdi]{plot.emdi}}, 
#' \code{\link[emdi]{predict.emdi}}, \code{\link[emdi]{qqnorm.emdi}}\cr \cr
#' Objects of class "direct", "ebp" and "fh" have method for following generic 
#' functions: \code{compare_plot} (for documentation, see \code{\link[emdi]{compare_plots_emdi}}), 
#' \code{getData} (for default documentation, see \code{\link[nlme]{getData}}), 
#' \code{getGroups} (for default documentation, see \code{\link[nlme]{getGroups}}), 
#' \code{getGroupsFormula} (for default documentation, see \code{\link[nlme]{getGroupsFormula}}), 
#' \code{getResponse} (for default documentation, see \code{\link[nlme]{getResponse}}), 
#' \code{plot} (for documentation, see \code{\link[emdi]{plot.emdi}}), \code{print}, 
#' \code{qqnorm} (for documentation, see \code{\link[emdi]{qqnorm.emdi}}) and
#' \code{summary} (for documentation, see \code{\link[emdi]{emdi_summaries}}).\cr \cr
#' Objects of class "ebp" and "fh" additionally have methods for following generic functions: 
#' \code{coef} (for default documentation, see \code{\link[stats]{coef}}), 
#' \code{family} (for default
#' documentation, see \code{\link[stats]{family}}), \code{fitted} (for default
#' documentation, see \code{\link[stats]{fitted.values}}), \code{fixef} (for 
#' default documentation, see \code{\link[nlme]{fixed.effects}}), \code{formula} (for 
#' default documentation, see \code{\link[stats]{formula}}), \code{getVarCov} 
#' (for documentation, see \code{\link[emdi]{getVarCov.ebp}} and 
#' \code{\link[emdi]{getVarCov.fh}}),\code{logLik} (for
#' default documentation, see \code{\link[stats]{logLik}}), \code{nobs} (for
#' default documentation, see \code{\link[stats]{nobs}}), \code{ranef} (for default 
#' documentation, see \code{\link[nlme]{random.effects}}), \code{residuals} 
#' (for default documentation, see \code{\link[stats]{residuals}}), \code{terms} 
#' (for default documentation, see \code{\link[stats]{terms}}), \code{vcov} 
#' (for default documentation, see \code{\link[stats]{vcov}}) \cr \cr
#' Objects of class "ebp" have additionally methods for following generic 
#' functions: \code{sigma} (for default documentation, see \code{\link[stats]{sigma}})\cr \cr
#' Objects of class "fh" have additionally methods for following generic functions: 
#' \code{\link[emdi]{compare}}, \code{extractAIC} (for default documentation, 
#' see \code{\link[stats]{extractAIC}}) and \code{\link[emdi]{step}}.
#' @references
#' Alfons, A. and Templ, M. (2013). Estimation of Social Exclusion Indicators
#' from Complex Surveys: The \R Package \pkg{laeken}. Journal of
#' Statistical Software, 54(15), 1-25.  \cr \cr
#' Fay R.E., Herriot R.A. (1979) Estimates of income for small places: An
#' application of James–Stein procedures to census data. Journal of the American
#' Statistical Association, Vol. 74, 269–277.  \cr \cr
#' Molina, I. and Rao, J.N.K. (2010). Small area estimation of poverty
#' indicators. The Canadian Journal of Statistics, Vol. 38, No.3, 369-385.
#' @seealso \code{\link{direct}}, \code{\link{ebp}}, \code{\link{fh}}, 
#' \code{ \link[nlme]{lme}}, \code{ \link[nlme]{lmeObject}}
#'
#' @name emdiObject
NULL