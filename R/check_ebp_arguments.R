# This script contains the checks of arguments that have be done for the 
# ebp function.


# Function called in ebp
ebp_check1 <- function(fixed, pop_data, pop_domains, smp_data, smp_domains, L){
  
  
  if(!exists("fixed") || is.null(fixed)  || !class(fixed)=="formula"){
    stop('Fixed must be a formula object. See also help(ebp).')
  } 
  if(!exists("pop_data") || !is.data.frame(pop_data)){
    stop('Pop_data must be a data.frame containing population data. 
           See also help(ebp).')
  }
  if(!exists("pop_domains") || !is.character(pop_domains)){
    stop('Pop_domains must be a character containing the name of a numeric or 
           factor variable indicating domains in the population data. 
           See also help(ebp).')
  }
  if(!exists("smp_data") || !is.data.frame(smp_data)){
    stop('Smp_data must be a data.frame containing sample data.
           See also help(ebp).')
  }
  if(!exists("smp_domains") || !is.character(smp_domains)){
    stop('Smp_domains must be a character containing the name of a numeric or 
           factor variable indicating domains in the sample data.
           See also help(ebp).')
  }
  if(!is.numeric(L)){
    stop('L needs to be a number determining the
           number of Monte-Carlo simulations. See also help(ebp).')
  }
  
}

ebp_check2 <- function(threshold, transformation, interval, MSE, B, 
                       custom_indicator, cpus){
  if(!(is.null(threshold) || is.numeric(threshold))){
    stop("threshold needs to be a number. If it is NULL 60% of the median
         is selected as threshold.")
  }
  if(!(transformation=="box.cox" || transformation=="log" || transformation=="no")){
    stop("The three options for transformation are ''no'', ''log'' or ''box.cox''." )
  }
  if(class(MSE) != "logical"){
    stop("MSE must be a logical value. Set MSE to TRUE or FALSE. See also
         help(ebp).")
  }
  if(MSE==TRUE && !is.numeric(B)){
    stop('If MSE is set to TRUE, a numeric value for the number of bootstrap
         sample needs to be chosen. See also help(ebp).')
  }
  if(!is.numeric(cpus)){
    stop("Cpus must be a number determining the number of kernels for the
         parallelization.")
  }
  
  
  if(!is.null(custom_indicator)){
    N_custom <- length(custom_indicator)
    for(i in 1:N_custom){
      #if(length(formals(custom_indicator[[i]])) != 2){
      #  stop("Function for custom indicators needs to have two arguments: y and
      #       threshold. See also help(ebp).")
      #}
      if(!all(names(formals(custom_indicator[[i]])) == c("y", "threshold"))){
        stop("Functions for custom indicators need to have exactly the following 
             two arguments: y, threshold; even though a threshold is not 
             included in the indicator. See also help(ebp).")
      }
      }
  }
    
  }


# Function called in notation
fw_check1 <- function(pop_domains_vec, smp_domains_vec){
  if(!(is.numeric(pop_domains_vec) || any(class(pop_domains_vec)=="factor"))){
    stop('Pop_domains needs to be the name of a variable that is numeric or
           a (ordered) factor.')
  }
  if(!(is.numeric(smp_domains_vec) || any(class(smp_domains_vec)=="factor"))){
    stop('Smp_domains needs to be the name of a variable that is numeric or
           a (ordered) factor.')
  }
  if((is.numeric(pop_domains_vec) && any(class(smp_domains_vec)=="factor")) ||
     (is.numeric(smp_domains_vec) && any(class(pop_domains_vec)=="factor")) ){
    stop('Pop_domains and smp_domains need to be names of variables that are
          of the same class (factor and ordered factor are considered to be 
         the same class). See also help(ebp).')
  }
}

fw_check2 <- function(obs_dom, dist_obs_dom){
  if(sum(obs_dom) == 0 || sum(dist_obs_dom) == 0){
    stop('Pop_domains and smp_domains do not have any value in common. Do really
         both variables indicate the same domains in population data and sample
         data, respectively?')
  }
  
}
