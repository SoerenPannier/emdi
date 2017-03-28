# This script contains the checks of arguments that have be done for the 
# direct estimation function.


# Function called in ebp
direct_check1 <- function(y, 
                          smp_data){
  
  
  if(!exists("y") || is.null(y)  || !class(y)=="character"){
    stop('y must be a character indicating the variable that is used for 
         estimating the indicators. See also help(direct).')
  } 
  if(!exists("smp_data") || !is.data.frame(smp_data)){
    stop('Smp_data must be a data frame containing the variable y.
         See also help(direct).')
  }
}


direct_check2 <- function(smp_domains = NULL, 
                          weights = NULL, 
                          sort = NULL,
                          threshold = NULL,
                          var = FALSE, 
                          boot_type = "naive", 
                          B = NULL,
                          X = NULL, 
                          totals = NULL){
  
  if(!exists("smp_domains") || !class(smp_domains)=="character"){
    stop('Smp_domains must be a character containing the name of a numeric or 
          factor variable indicating domains in the sample data.
          See also help(direct).')
  } 
  if((boot_type != "naive" && (!exists("weights")))
      || (exists("weights") && !(is.null(weights) || class(weights)=="character"))){
    stop('Weights must be a character containing the name of a variable for the 
          sampling weights in the sample data. See also help(direct).')
  }
  if(!is.null(threshold) && !(is.numeric(threshold) || "function" %in% class(threshold))){
    stop('threshold needs to be a number or a function of y and weights. 
          If it is NULL 60% of the median is selected as threshold.')
  }
  if(class(var) != "logical"){
    stop("Var must be a logical value. Set Var to TRUE or FALSE. See also
         help(direct).")
  }
  if(var==TRUE && !(boot_type == "naive" || boot_type == "calibrate")){
    stop('If var is set to TRUE, boot_type "naive" or "calibrate" needs to be 
         selected. See also help(direct).')
  }
  if(var==TRUE && !is.numeric(B)){
    stop('If var is set to TRUE, a numeric value for the number of bootstrap
         sample needs to be chosen. See also help(direct).')
  }
  if(var==TRUE && boot_type == "calibrate" && !is.numeric(X)){
    stop("X must be a numeric matrix.")
  } 
    
  
}