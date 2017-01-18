#' Simulated eusilc data - population data
#'
#' The data set is synthetic EU-SILC data based on the data set
#' \code{\link[simFrame]{eusilcP}} from package \pkg{simFrame}. The data set is 
#' reduced to 19 variables containing four regional variables for the country, 
#' states, districts and counties.  
#'
#' @format A data frame with 25000 observations and 18 variables:
#' \describe{
#' \item{eqIncome}{numeric; a simplified version of the equivalized household income.}
#' \item{eqsize}{numeric; the equivalized household size according to the 
#' modified OECD scale.}
#' \item{gender}{factor; the person's gender (levels male and female).}
#' \item{cash}{numeric; employee cash or near cash income (net).}
#' \item{self_empl}{numeric; cash benefits or losses from self-employment (net).}
#' \item{unempl_ben}{numeric; unemployment benefits (net).}
#' \item{age_ben}{numeric; old-age benefits (net).}
#' \item{surv_ben}{numeric; survivor's benefits (net).}
#' \item{sick_ben}{numeric; sickness benefits (net).}
#' \item{dis_ben}{numeric; disability benefits (net).}
#' \item{rent}{numeric; income from rental of a property or land (net).}
#' \item{fam_allow}{numeric; family/children related allowances (net).}
#' \item{house_allow}{numeric; housing allowances (net).}
#' \item{cap_inv}{numeric; interest, dividends, profit from capital investments
#'  in unincorporated business (net).}
#' \item{tax_adj}{numeric; repayments/receipts for tax adjustment (net).}
#' \item{state}{factor; state (nine levels)}
#' \item{district}{factor; districts (96 levels)}
#' \item{county}{factor; counties (2115 levels)}
#' }
#' @docType data
"eusilcA_pop"