# This script contains the checks of arguments that have be done for the 
# direct estimation function.


direct_check <- function(y, 
                          smp_data,
                          smp_domains = NULL, 
                          weights = NULL, 
                          design = NULL,
                          threshold = NULL,
                          var = FALSE, 
                          boot_type = "naive", 
                          B = NULL,
                          seed,
                          X_calib = NULL, 
                          totals = NULL,
                          na.rm, 
                          custom_indicator) {
  if (is.null(y)  || !(inherits(y, "character") && length(y) == 1 
                       && (y %in% names(smp_data)))) {
    stop('y must be a single character indicating the variable that is used for 
         estimating the indicators. The variable needs to be contained in 
         smp_data. See also help(direct).')
  } 
  if (!inherits(smp_data, "data.frame")) {
    stop('Smp_data must be a data frame containing the variable y.
         See also help(direct).')
  } 
  if (!inherits(smp_domains, "character") || length(smp_domains) != 1
      ||  !(smp_domains %in% names(smp_data))) {
    stop('Smp_domains must be a single character containing the name of a 
          variable indicating domains in the sample data. The variable needs 
          to be contained in smp_data. See also help(direct).')
  } 
  if ((!(is.null(weights) || (inherits(weights, "character") && length(weights) == 1
                              && (weights %in% names(smp_data)))))) {
    stop('Weights must be a single character containing the name of a variable 
          for the sampling weights in the sample data.  The variable needs 
          to be contained in smp_data. See also help(direct).')
  }
  if (var == TRUE && boot_type != "naive" && is.null(weights)) {
    stop("If boot_type is set to 'calibrate' weights must be chosen. Weights
          need to be a single character containing the name of a variable 
          for the sampling weights in the sample data. See also help(direct).")
  }
  if (!(is.null(design) || (inherits(design, "character") && length(design) == 1))) {
    stop('Design must be a single character containing the name of a variable 
         for the sampling design in the sample data. See also help(direct).')
  }
  if (!is.null(threshold) && !(is.numeric(threshold) && length(threshold) == 1) 
                              && !inherits(threshold, "function")) {
    stop('threshold needs to be a single number or a function of y and weights. 
          If it is NULL 60% of the median of the target variable is selected 
         as threshold. See also help(direct).')
  }
  if (inherits(threshold, "function") && !all(names(formals(threshold)) == c("y", "weights"))) {
    stop('If threshold is a function the arguments need to be y and weights in 
          this order. Also a single number is possible as threshold. If it is 
          NULL 60% of the median of the target variable is selected as threshold. 
          See also help(direct).')
  }
  if (!is.logical(var) || length(var) != 1) {
    stop("Var needs to be a logical value. Set Var to TRUE or FALSE. See also
         help(direct).")
  }
  if (var == TRUE && !(boot_type == "naive" || boot_type == "calibrate")) {
    stop('If var is set to TRUE, boot_type "naive" or "calibrate" needs to be 
         selected. See also help(direct).')
  }
  if (var == TRUE && !(is.numeric(B) && length(B) == 1)) {
    stop('If var is set to TRUE, a single number for the number of bootstrap
         sample needs to be chosen. See also help(direct).')
  }
  if (!is.null(seed) && (!is.numeric(seed) || !(is.numeric(seed) && length(seed) == 1))) {
    stop("Seed must be a single number or NULL as initialisation of the RNG.
         See also help(direct).")
  } 
  if (var == TRUE && boot_type == "calibrate" 
      && !(inherits(X_calib, c("matrix")) && is.numeric(X_calib) && (dim(X_calib)[1] == dim(smp_data)[1]))) {
    stop("If boot_type is set to 'calibrate', X_calib must be a numeric matrix
          which colums have the same length as the colums in the sample data.
         See also help(direct).")
  } 
  if (var == TRUE && boot_type == "calibrate" 
      && !(is.null(totals) || (is.numeric(totals) && (length(totals) == dim(X_calib)[2])))) {
    stop("If boot_type is set to 'calibrate', totals can be NULL or must be 
         a numeric vector which length equals to the number of colums in 
         X_calib. See also help(direct).")
  } 
  if (!(inherits(na.rm, "logical") && length(na.rm) == 1)) {
    stop("na.rm needs to be a logical value. Set na.rm to TRUE or FALSE. See 
         also help(direct).")
  }
  if (!is.null(custom_indicator)) {
    
    if (!inherits(custom_indicator, "list")) {
      stop("Additional indicators need to be added in argument custom_indicator
           as a list of functions. For help see Example 3 in help(direct).")
    }
    
    N_custom <- length(custom_indicator)
    for (i in 1:N_custom) {
      if (!inherits(custom_indicator[[i]], "function")) {
        stop("The elements of the list need to be functions. These Functions 
             for custom indicators need to have exactly the following 
             three arguments: y, weights, threshold; even though weights might 
             not be needed and a threshold might not be 
             included in the indicator. For help see Example 3 in help(direct).")
      }
      #if(length(formals(custom_indicator[[i]])) != 2){
      #  stop("Function for custom indicators needs to have two arguments: y and
      #       threshold. See also help(ebp).")
      #}
      else if (inherits(custom_indicator[[i]], "function") 
               && !all(names(formals(custom_indicator[[i]])) == c("y", "weights", "threshold"))) {
        stop("Functions for custom indicators need to have exactly the following 
             three arguments: y, weights threshold; even though weights might 
             not be needed and a threshold might not be 
             included in the indicator. For help see Example 3 in help(direct).")
      }
      }
      }
}