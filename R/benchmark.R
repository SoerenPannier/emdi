#' Benchmark function
#'
#' This function benchmarks the EBLUP estimates.
#'
#' @param object an object of type "fh".
#' @param benchmark a number determining the benchmark value.
#' @param eta a vector containing the ratios of the population size per area and 
#' the total population size ($N_d/N).Values must be sorted like the domains in 
#' the fh object.
#' @param type Character indicating the type of benchmarking. Types that can be chosen
#' (i) Raking ("\code{raking}"),
#' (ii) Ratio adjustment ("\code{ratio}"),
#' (iii) MSE adjustment ("\code{MSE_adj}"). Defaults to \code{raking}".
#' @param overwrite if \code{TRUE}, the EBLUP results of the fh object are 
#' substituted by the respective benchmarked results. Defaults
#' to \code{FALSE}. 
#' @return A data frame containing a domain indicator (Domain), direct estimates 
#' (Direct), point predictions (FH), benchmarked point predictions (FHBENCH) and 
#' a variable indicating out-of-sample domains Out (1 for out-of-sample, 0 for 
#' in-sample) . If overwrite is set to TRUE, the fh object is returned, but the 
#' point predictions of the ind data frame are substituted by the benchmarked 
#' results.
#' @details
#' The benchmarking algorithm does not work, if no out-of-sample point estimates 
#' are available.
#' @references 
#' Datta,G. S., Ghosh, M., Steorts, R. and Maples, J. (2010) Bayesian 
#' benchmarking with applications to small area estimation. Test, 20, 574–588.
#' @examples 
#' # Loading data - population and sample data
#' data("eusilcA_popAgg")
#' data("eusilcA_smpAgg")
#' 
#' # Combine sample and population data -------------------------------------------
#' combined_data <- combine_data(pop_data = eusilcA_popAgg, pop_domains = "Domain",
#'                              smp_data = eusilcA_smpAgg, smp_domains = "Domain")
#'
#' # Estimate Fay-Herriot model
#' fh_std <- fh(fixed = Mean ~ cash + self_empl, vardir = "Var_Mean",
#' combined_data = combined_data, domains = "Domain", method = "ml", 
#' interval = c(0, 100000000), MSE = TRUE)  
#' 
#' # Benchmark the point estimates
#' 
#' # Example 1: receive data frame with point estimates and their benchmarked results
#' fh_bench <- benchmark(fh_std, benchmark = 20140.09, 
#' eta = eusilcA_popAgg$ratio_n, type = "ratio")
#' 
#' # Example 2: overwrite the point estimates of the fh object with their benchmarked 
#' # results
#' fh_bench <- benchmark(fh_std, benchmark = 20140.09, 
#' eta = eusilcA_popAgg$ratio_n, type = "ratio", overwrite = TRUE)
#' @export


benchmark <- function(object, benchmark, eta, type = "raking", overwrite = FALSE) {
  
  check_benchmark_arguments(object = object, benchmark = benchmark, eta = eta, 
                            type = type, overwrite = overwrite)
  
  estim <- object$ind$FH
  FH_bench <- rep(NA, length(object$ind$FH))
  #benchmark <- sum(eta * object$ind$Direct)
  if (type == "raking"){
    FH_bench <- estim + benchmark - sum(eta * estim)
  } else if (type == "ratio"){
    phi <- eta/object$ind$FH
    FH_bench <- estim + (1/(sum(eta^2/phi))) * 
      (benchmark - sum(eta * estim)) * (eta / phi)
  } else if (type == "MSE_adj"){
    phi <- eta/object$MSE$FH
    FH_bench <- estim + (1/(sum(eta^2/phi))) * 
      (benchmark - sum(eta * estim)) * (eta / phi)
  }
  if (overwrite == FALSE){
    EBLUP_data_bench <- data.frame(Domain = object$ind$Domain)
    EBLUP_data_bench$Direct <- object$ind$Direct
    EBLUP_data_bench$FH <- object$ind$FH
    EBLUP_data_bench$FHBENCH <- FH_bench
    EBLUP_data_bench$Out <- object$ind$Out
    result <- EBLUP_data_bench
  } else {
    object$ind$FH <- FH_bench
    result <- object
  }
  return(result)
}



################################################################################
# Argument checking
check_benchmark_arguments <- function(object, benchmark, eta, type, overwrite){
  if(!inherits(object, "fh")){
    stop('Object needs to be fh object.')
  }
  if ((any(is.na(object$ind$FH)))){
    stop("If no predictions for out-of-sample domains 
         are available, the benchmarking algorithm does not work.")
  }
  if (is.null(type) || !(is.character(type)) || !(type == "raking" || 
                                                  type == "ratio" || 
                                                  type == "MSE_adj")) {
    stop("Type must be a character. The three options for types are ''raking'', 
         ''ratio'' and ''MSE_adj''.")
  }
  if ((is.null(object$MSE)) && type == "MSE_adj"){
    stop("If no MSE estimates are available,
         ''MSE_adj'' benchmarking does not work. The fh object has to contain MSE 
          estimates. Therefore set the MSE argument of the fh function to TRUE.")
  }
  if ((any(is.na(object$MSE$FH))) && type == "MSE_adj"){
    stop("For the benchmarking type ''MSE_adj'' the MSE estimates of the fh 
         object must not contain NAs. If no MSE estimates for out-of-sample domains 
         are available,''MSE_adj'' benchmarking does not work.")
  }
  if (is.null(benchmark)  || !(is.numeric(benchmark) && length(benchmark) == 1)) { 
    stop("benchmark needs to be a single numeric value. See also help(benchmark).")
  }
  if (!is.vector(eta) || length(eta) != length(object$ind$Domain)) {
    stop("eta must be a vector with length equal to the number of domains. 
         See also help(benchmark).")
  } 
  if (any(is.na(eta))) {
    stop("eta must not contain NAs.")
  }
  if (!is.logical(overwrite) || length(overwrite) != 1) {
    stop("overwrite must be a logical value. Set overwrite to TRUE or FALSE. The 
          default is set to FALSE. See also help(benchmark).")
  }
}



