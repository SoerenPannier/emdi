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
                          X_calib = NULL, 
                          totals = NULL,
                          seed) {
  if (is.null(y)  || !(inherits(y, "character") && length(y) == 1)) {
    stop('y must be a single character indicating the variable that is used for 
         estimating the indicators. See also help(direct).')
  } 
  if (!inherits(smp_data, "data.frame")) {
    stop('Smp_data must be a data frame containing the variable y.
         See also help(direct).')
  } 
  if (!inherits(smp_domains, "character") || length(smp_domains) != 1) {
    stop('Smp_domains must be a single character containing the name of a 
          variable indicating domains in the sample data.
          See also help(direct).')
  } 
  if ((!(is.null(weights) || (inherits(weights, "character") && length(weights) == 1)))) {
    stop('Weights must be a single character containing the name of a variable 
          for the sampling weights in the sample data. See also help(direct).')
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
  if (inherits(threshold, "function") && !all(attributes(formals(threshold))$names == c("y", "weights"))) {
    stop('If threshold is a function the arguments need to be y and weights in 
          this order. Also a single number is possible as threshold. If it is 
          NULL 60% of the median of the target variable is selected as threshold. 
          See also help(direct).')
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
    stop('If var is set to TRUE, a single number for the number of bootstrap
         sample needs to be chosen. See also help(direct).')
  }
  if (var == TRUE && boot_type == "calibrate" 
      && !(inherits(X_calib, c("numeric", "matrix")) && dim(X_calib)[1] == dim(smp_data)[1])) {
    stop("If boot_type is set to 'calibrate', X_calib must be a numeric matrix
          which colums have the same length as the colums in the sample data.
         See also help(direct).")
  } 
  if (!is.null(seed) && (!is.numeric(seed) || !(is.numeric(seed) && length(seed) == 1))) {
    stop("Seed must be a single number or NULL as initialisation of the RNG.
          See also help(direct).")
  } 
  
}