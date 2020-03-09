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
#' (iii) MSE adjustment ("\code{MSE_adj}")
#' @return a list with following components: 
#' \tabular{lll}{
#'      \tab \code{benchmark_data} \tab data frame containing a domain indicator 
#'      (Domain), direct estimates (Direct),  \cr
#'    \tab \tab point predictions (FH), benchmarked point predictions (FHBENCH) and 
#' a  \cr
#' \tab \tab variable indicating out-of-sample domains Out (1 for out-of-sample, 0 for 
#' in- \cr
#' \tab \tab sample) .\cr
#'      \tab \code{benchmark_type} \tab character returning the type of the 
#'      benchmark method. \cr
#' }
#' @references 
#' Datta,G. S., Ghosh, M., Steorts, R. and Maples, J. (2010) Bayesian 
#' benchmarking with applications to small area estimation. Test, 20, 574–588.
#' @export


benchmark <- function(object, benchmark, eta, type) {
  
  check_benchmark_arguments(object = object, benchmark = benchmark, eta = eta, 
                            type = type)
  
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
  EBLUP_data_bench <- data.frame(Domain = object$ind$Domain)
  EBLUP_data_bench$Direct <- object$ind$Direct
  EBLUP_data_bench$FH[object$ind$Out == 0] <- object$ind$FH
  EBLUP_data_bench$FHBENCH[object$ind$Out == 0] <- FH_bench
  EBLUP_data_bench$Out <- object$ind$Out
  result <- list(benchmark_data = EBLUP_data_bench, benchmark_type = type)
  return(result)
}



################################################################################
# Argument checking
check_benchmark_arguments <- function(object, benchmark, eta, type){
  if(!inherits(object, "fh")){
    stop('Object needs to be fh object.')
  }
  if (is.null(benchmark)  || !(is.numeric(benchmark) && length(benchmark) == 1)) { 
    stop("benchmark needs to be a single numeric value. See also help(benchmark).")
  }
  if (!is.vector(eta) || length(eta) != object$framework$N_dom_smp) {
    stop("eta must be a vector with length equal to the number of domains. 
         See also help(benchmark).")
  } 
  if (is.null(type) || !(is.character(type)) || !(type == "raking" || 
                                                 type == "ratio" || 
                                                 type == "MSE_adj")) {
    stop("Type must be a character. The three options for types are ''raking'', 
         ''ratio'' and ''MSE_adj''.")
  }
}



