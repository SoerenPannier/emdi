#' Simulated eusilc data - aggregated sample data
#'
#' The data set is a simple random sample of data set \code{\link{eusilcA_pop}} 
#' which is based on \code{\link[simFrame]{eusilcP}} from package \pkg{simFrame} 
#' The data set is aggregated on the district level and contains different 
#' variables that are related to income and a regional variable for the districts. 
#'
#' @format A data frame with 94 observations and 10 variables:
#' \describe{
#' \item{Mean}{numeric; a simplified version of the equivalized household income.}
#' \item{Head_Count}{numeric; head count ratio.}
#' \item{MTMED}{numeric; share of households who earn more than the national 
#' median income.}
#' \item{Cash}{numeric; employee cash or near cash income.}
#' \item{Var_Mean}{numeric; variance of the equivalized household income.}
#' \item{Var_Head_Count}{numeric; variance of the head count ratio.}
#' \item{Var_MTMED}{numeric; variance of the share of households who earn more 
#' than the national median income.}
#' \item{Var_Cash}{numeric; variance of the employee cash or near cash income.}
#' \item{n}{numeric; effective sample sizes.}
#' \item{Domain}{factor; Austrian districts (94 levels).}
#' }
#' @docType data
"eusilcA_smpAgg"