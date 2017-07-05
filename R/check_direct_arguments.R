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
  if (is.null(y)  || !(inherits(y, "character") && length(y) == 1)) {
    stop('y must be a vector of length 1 and of class character specifying the
          variable (name) that is used for estimating the indicators. The 
         variable needs to be contained in smp_data. See also help(direct).')
  } 
  if (!(y %in% colnames(smp_data))) {
    stop(paste0(y, " is not contained in smp_data. 
         Please provide valid variable name for y."))
  } 
  if (!inherits(smp_data, "data.frame")) {
    stop('Smp_data must be a data frame containing the variable y.
         See also help(direct).')
  } 
  if (!inherits(smp_domains, "character") || length(smp_domains) != 1) {
    stop('Smp_domains must be a vector of length 1 and of class character 
          specifying the variable (name) indicating domains in the sample data. 
          The variable needs to be contained in smp_data. See also help(direct).')
  } 
  if (!(smp_domains %in% colnames(smp_data))) {
    stop(paste0(smp_domains, " is not contained in smp_data. 
         Please provide valid variable name for smp_domains."))
  } 
  if ((!(is.null(weights) || (inherits(weights, "character") && length(weights) == 1)))) {
    stop('Weights must be a vector of length 1 and of class character 
          specifying the variable (name) of the sampling weights in the sample 
          data.  The variable needs to be contained in smp_data. 
          See also help(direct).')
  }
  if (!(is.null(weights) || weights %in% colnames(smp_data))) {
    stop(paste0(weights, " is not contained in smp_data. 
         Please provide valid variable name for weights."))
  } 
  if (!(is.null(weights) || is.numeric(smp_data[[weights]]))) {
    stop(paste0(weights, " must be the name of a variable that is a numeric vector."))
  }
  if (var == TRUE && boot_type != "naive" && is.null(weights)) {
    stop("If boot_type is set to 'calibrate' weights must be chosen. Weights
          must be a vector of length 1 and of class character 
          specifying the variable (name) of the sampling weights in the sample 
          data. See also help(direct).")
  }
  if (!(is.null(design) || (inherits(design, "character") && length(design) == 1))) {
    stop('Design must be a vector of length 1 and of class character 
          specifying the variable (name) of the sampling design in the sample 
          data. See also help(direct).')
  }
  if (!is.null(threshold) && !(is.numeric(threshold) && length(threshold) == 1) 
                              && !inherits(threshold, "function")) {
    stop('threshold must be a single numeric value or a function of y and weights. 
          If it is NULL 60% of the median of the target variable is selected 
         as threshold. See also help(direct).')
  }
  if (inherits(threshold, "function") && !all(names(formals(threshold)) == c("y", "weights"))) {
    stop('If threshold is a function the arguments need to be y and weights in 
          this order. Also a single numeric value is possible as threshold. If 
          it is NULL 60% of the median of the target variable is selected as 
          threshold. See also help(direct).')
  }
  if (!is.logical(var) || length(var) != 1) {
    stop("Var must be a logical value. Set Var to TRUE or FALSE. See also
         help(direct).")
  }
  if (var == TRUE && !(boot_type == "naive" || boot_type == "calibrate")) {
    stop('If var is set to TRUE, boot_type "naive" or "calibrate" needs to be 
         selected. See also help(direct).')
  }
  if (var == TRUE && !(is.numeric(B) && length(B) == 1)) {
    stop('If var is set to TRUE, a single value, interpreted as an integer, for 
          the number of bootstrap sample needs to be chosen. See also help(direct).')
  }
  if (!is.null(seed) && (!is.numeric(seed) || !(is.numeric(seed) && length(seed) == 1))) {
    stop("The seed must be a single value, interpreted as an integer, or NULL.
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
    stop("na.rm must be a logical value. Set na.rm to TRUE or FALSE. See 
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