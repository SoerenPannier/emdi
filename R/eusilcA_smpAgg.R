#' Simulated eusilc data - aggregated sample data
#'
#' The data set is a simple random sample of data set \code{\link{eusilcA_pop}}
#' which is based on \code{\link[simFrame]{eusilcP}} from package
#' \pkg{simFrame}. The data set is aggregated on the district level and contains
#' different variables that are related to income and a regional variable for
#' the districts.
#'
#' @format A data frame with 94 observations and 8 variables:
#' \describe{
#' \item{Mean}{numeric; mean of a simplified version of the equivalized
#' household income.}
#' \item{MTMED}{numeric; share of households who earn more than the national
#' median income.}
#' \item{Cash}{numeric; mean of employee cash or near cash income.}
#' \item{Var_Mean}{numeric; variance of a simplified version of the equivalized
#' household income.}
#' \item{Var_MTMED}{numeric; variance of the share of households who earn more
#' than the national median income.}
#' \item{Var_Cash}{numeric; variance of the employee cash or near cash income.}
#' \item{n}{numeric; effective sample sizes.}
#' \item{Domain}{factor; Austrian districts (94 levels).}
#' }
#' @docType data
"eusilcA_smpAgg"