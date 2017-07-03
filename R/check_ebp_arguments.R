# This script contains the checks of arguments that have be done for the 
# ebp function.


# Function called in ebp
ebp_check1 <- function(fixed, pop_data, pop_domains, smp_data, smp_domains, L){
  
  
  if (is.null(fixed)  || !inherits(fixed, "formula")) {
    stop('Fixed must be a formula object. See also help(ebp).')
  } 
  if (!is.data.frame(pop_data)) {
    stop('Pop_data must be a data frame containing population data. 
           See also help(ebp).')
  }
  if (!is.character(pop_domains) || length(pop_domains) != 1) {
    stop('Pop_domains must be a single character containing the name of 
           a numeric or factor variable indicating domains in the population 
           data. See also help(ebp).')
  }
  if (!is.data.frame(smp_data)) { 
    stop('Smp_data must be a data frame containing sample data.
           See also help(ebp).')
  }
  if (!is.character(smp_domains) || length(smp_domains) != 1) {
    stop('Smp_domains must be a single character containing the name of a 
          numeric or factor variable indicating domains in the sample data.
          See also help(ebp).')
  }
  if (!is.numeric(L) || length(L) != 1) {
    stop('L needs to be a single number determining the
           number of Monte-Carlo simulations. See also help(ebp).')
  }
  
}

ebp_check2 <- function(threshold, transformation, interval, MSE, B, 
                       custom_indicator, cpus, seed){
  if (!is.null(threshold) && !(is.numeric(threshold) && length(threshold) == 1)
       && !inherits(threshold, "function")) { 
    stop("threshold needs to be a single number or a function of y. 
          If it is NULL 60% of the median is selected as threshold.
         See also help(ebp).")
  }
  if (inherits(threshold, "function") && !all(attributes(formals(threshold))$names == c("y"))) {
    stop('If threshold is a function the argument needs to be y and only y. Also 
          a single number is possible as threshold. If it is 
          NULL 60% of the median of the target variable is selected as threshold. 
          See also help(ebp).')
  }
  if (is.null(transformation) || !(transformation == "box.cox" 
        || transformation == "log" 
        || transformation == "no")) {
    stop("The three options for transformation are ''no'', ''log'' or ''box.cox''." )
  }
  if (length(interval) != 2 || !is.vector(interval)) {
    stop("interval needs to be a vector of length 2 
         defining a lower and upper limit for the estimation of the optimal 
         transformation parameter. See also help(ebp).")
  }
  if (!is.logical(MSE) || length(MSE) != 1) {
    stop("MSE must be a logical value. Set MSE to TRUE or FALSE. See also
         help(ebp).")
  }
  if (MSE == TRUE && !(is.numeric(B) && length(B) == 1)) {
    stop('If MSE is set to TRUE, a single number for the number of bootstrap
         sample needs to be chosen. See also help(ebp).')
  }
  if (!is.numeric(cpus) || !(is.numeric(cpus) && length(cpus) == 1)) {
    stop("Cpus must be a single number determining the number of kernels for the
         parallelization.")
  }
  if (!is.null(seed) && (!is.numeric(seed) || !(is.numeric(seed) && length(seed) == 1))) {
    stop("Seed must be a single number or NULL as initialisation of the RNG. ")
  }
  
  if (!is.null(custom_indicator)) {
    N_custom <- length(custom_indicator)
    for (i in 1:N_custom) {
      #if(length(formals(custom_indicator[[i]])) != 2){
      #  stop("Function for custom indicators needs to have two arguments: y and
      #       threshold. See also help(ebp).")
      #}
      if (!all(names(formals(custom_indicator[[i]])) == c("y", "threshold"))) {
        stop("Functions for custom indicators need to have exactly the following 
             two arguments: y, threshold; even though a threshold is not 
             included in the indicator. See also help(ebp).")
      }
      }
  }
    
  }


# Functions called in notation
fw_check1 <- function(pop_data, mod_vars, pop_domains, smp_data, 
                      fixed, smp_domains) {
  if (!((mod_vars %in% names(pop_data)) && (pop_domains %in% names(pop_data)))) {
    stop('Both the variable name in pop_domains and the explanatory variables
         in argument fixed need to be contained in pop_data.')
  }
  if (!((mod_vars %in% names(smp_data)) && (smp_domains %in% names(smp_data))
        && (as.character(fixed[2])) %in% names(smp_data))) {
    stop('The variable name in smp_domains and the variables
         in argument fixed need to be contained in smp_data.')
  }
}



fw_check2 <- function(pop_domains_vec, smp_domains_vec){
  if (!(is.numeric(pop_domains_vec) || any(inherits(pop_domains_vec, "factor")))) {
    stop('Pop_domains needs to be the name of a variable that is numeric or
           a (ordered) factor.')
  }
  if (!(is.numeric(smp_domains_vec) || any(inherits(smp_domains_vec, "factor")))) {
    stop('Smp_domains needs to be the name of a variable that is numeric or
           a (ordered) factor.')
  }
  if ((is.numeric(pop_domains_vec) && any(inherits(smp_domains_vec, "factor"))) ||
     (is.numeric(smp_domains_vec) && any(inherits(pop_domains_vec, "factor"))) ) {
    stop('Pop_domains and smp_domains need to be names of variables that are
          of the same class (factor and ordered factor are considered to be 
         the same class). See also help(ebp).')
  }
}

fw_check3 <- function(obs_dom, dist_obs_dom){
  if (sum(obs_dom) == 0 || sum(dist_obs_dom) == 0) {
    stop('Pop_domains and smp_domains do not have any value in common. Do really
         both variables indicate the same domains in population data and sample
         data, respectively?')
  }
  
}
