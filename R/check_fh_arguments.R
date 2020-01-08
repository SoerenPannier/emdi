# This script contains the checks of arguments that have be done for the 
# fh function.

# Function called in fh

fh_check <- function(fixed, vardir, combined_data, domains, method, interval,k, 
                     c, transformation, backtransformation, eff_smpsize,
                     correlation, corMatrix, time, Ci, tol, maxit, MSE, 
                     mse_type, B, seed, alpha){
  
  if (is.null(fixed) || !inherits(fixed, "formula")) {
    stop('Fixed must be a formula object. See also help(fh).')
  }
  if (is.null(vardir) || !(vardir %in% colnames(combined_data))) {
    stop(paste0("The sampling variances variable ",vardir, " is not contained in
                 combined_data. Please provide valid variable name for the
                 sampling variances."))
  }
  if (is.null(combined_data) || !is.data.frame(combined_data)) {
    stop('combined_data must be a data frame containing the direct estimates,
         the sampling variances, the explanatory variables and the domains. If 
         the arcsin transformation in combination with bootstrap mse is chosen, 
         also eff_smpsize needs to be included. See also help(fh).') 
  }
  if (!is.character(domains) || length(domains) != 1 || 
      !(domains %in% colnames(combined_data))) {
    stop('domains must be a vector of length 1 and of class character 
         specifying the variable name of a numeric or factor variable 
         indicating domains in the combined_data. See also help(fh).')
  }
  
  if (is.null(method) || !(method == "reml" 
                           || method == "amrl" 
                           || method == "amrl_yl" 
                           || method == "ampl" 
                           || method == "ampl_yl" 
                           || method == "ml"
                           || method == "moment"
                           || method == "reblup"
                           || method == "reblupbc")) {
    stop("The nine options for method are ''reml'', ''amrl'', ''amrl_yl'',
          ''ampl'', ''ampl_yl'', ''ml'', ''moment'',''reblup'' or ''reblupbc''.")
  }
  if (length(interval) != 2 || !is.vector(interval, mode = "numeric") ||
      !(interval[1] < interval[2])) {
    stop("interval needs to be a numeric vector of length 2 
         defining a lower and upper limit for the estimation of the variance of 
         the random effect. The value of the lower limit needs to be 
         smaller than the upper limit. See also help(fh).")
  }
  if (is.null(k)  || !(is.numeric(k) && length(k) == 1)) { 
    stop("k needs to be a single numeric value. See also help(fh).")
  }
  if (is.null(c)  || !(is.numeric(c) && length(c) == 1)) { 
    stop("c needs to be a single numeric value. See also help(fh).")
  }
  if (is.null(transformation) || !(transformation == "arcsin" 
                                   || transformation == "log" 
                                   || transformation == "no")) {
    stop("The three options for transformation are ''no'', ''log'' or ''arcsin''." )
  }
  if (!is.null(backtransformation) && !(backtransformation == "naive" 
                                   || backtransformation == "crude" 
                                   || backtransformation == "sm")) {
    stop("The three options for backtransformation are ''naive'', ''crude'' or 
         ''sm''." )
  }
  if (!is.null(eff_smpsize) && !(eff_smpsize %in% colnames(combined_data))) {
    stop(paste0("The effective sample size variable ",eff_smpsize, " is not 
                 contained in combined_data. Please provide valid variable name 
                 for the effective sample size."))
  }
  if (is.null(correlation) || !(correlation == "no" 
                           || correlation == "spatial")) {
    stop("The options for correlation are ''no'' or ''spatial''.")
  }
  if (correlation == "spatial" && 
     (!(is.matrix(corMatrix) || is.data.frame(corMatrix)))) {
    stop('corMatrix must be a data frame or matrix containing the row-standardised 
          proximity matrix. See also help(fh). A description how a proximity matrix
          can be computed can be found in the vignette.') ## evtl. ergänzen
  }
  estcoef <- makeXY(formula = fixed, data = combined_data)
  if (method == "moment" && !is.null(Ci) &&
      (!(dim(Ci_array)[1] == dim(estcoef$x)[2]) || 
       !(dim(Ci_array)[2] == dim(estcoef$x)[2]) || 
       !(dim(Ci_array)[3] == dim(estcoef$x)[1]))){
   stop('Ci must be an array with dimension number of estimated regression 
        coefficients times number of estimated regression coefficients times 
        number of areas containing the variance-covariance matrix of the 
        explanatory variables for each area. The areas should be sorted like in 
        combined_data. See also help(fh). For an example how to create the array 
        please refer to the Vignette.')
  }
  if ((method == "moment") && (!is.numeric(tol) || !(is.numeric(tol) &&
                                                     length(tol) == 1))) {
    stop("tol must be a single number determining the tolerance value for the 
          convergence of weights for the estimation of the variance of the random 
          effects and the beta coefficients. See help(fh).")
     }
  if ((method == "moment") && (!is.numeric(maxit) || !(is.numeric(maxit) &&
                                                         length(maxit) == 1))) {
    stop("maxit must be a single number determining the tolerance value for the 
          convergence of weights for the estimation of the variance of the random 
          effects and the beta coefficients. See help(fh).")
  }
  if (!is.logical(MSE) || length(MSE) != 1) {
    stop("MSE must be a logical value. Set MSE to TRUE or FALSE. The default is 
          set to FALSE. See also help(fh).")
  } 
  if (MSE == TRUE && 
      (is.null(mse_type) || !(length(mse_type) == 1 && 
                              (mse_type == "analytical" 
                               || mse_type == "boot" 
                               || mse_type == "pseudo" 
                               || mse_type == "jackknife"
                               || mse_type == "weighted_jackknife")))) {
    stop("The five mse types are ''analytical'', ''boot'', ''pseudo'', 
         ''jackknife'' or ''weighted_jackknife''." )
  } 
  if (MSE == TRUE && (mse_type == "boot" || mse_type == "spatialparboot" || 
                      mse_type == "spatialnonparboot") && is.null(B)){
    stop('If MSE is set to TRUE and a bootstrap MSE estimation method is chosen, 
          the argument B is required and cannot be NULL. See also help(fh).')
  }
  if (MSE == TRUE && (mse_type == "boot" || mse_type == "spatialparboot" || 
                      mse_type == "spatialnonparboot") && 
      !(is.numeric(B) && length(B) == 1 && B > 1)) {
    stop('If MSE is set to TRUE and a bootstrap MSE estimation method is chosen, 
          A single numeric value for the number of bootstrap samples (B) needs 
          to be chosen that is greater than 1. See also help(fh).')
  }
  if (!is.null(seed) && (!is.numeric(seed) || !(is.numeric(seed) && length(seed) == 1))) {
    stop("The seed must be a single value, interpreted as an integer, or NULL
         See also help(fh).")
  }
  ### alpha Argument fehlt. Konfidenzintervalle werden evtl noch rausgenommen.
}

# Functions called in notation (framework)
fh_fw_check1 <- function(fixed, vardir, combined_data, domains, eff_smpsize = NULL,
                         time = NULL, Ci = NULL){
  
  if (!(domains %in% colnames(combined_data))) {
    stop(paste0("The domain variable ",domains, " is not contained in combined_data.
                Please provide valid variable name for domains."))
  }
  if (!(vardir %in% colnames(combined_data))) {
    stop(paste0("The sampling variances variable ",vardir, " is not contained in
                 combined_data. Please provide valid variable name for vardir."))
  }
  if (!(eff_smpsize %in% colnames(combined_data))) { ##evtl doppelt (siehe FH checks)
    stop(paste0("The effective sample size variable ",eff_smpsize, " is not contained in
                 combined_data. Please provide valid variable name for eff_smpsize."))
  }
  if (!(as.character(lhs(fixed))) %in% colnames(combined_data)) {
    stop(paste0("Variable ", as.character(lhs(fixed)), " is not contained in combined_data. 
                Please provide valid variable name for the dependent variable."))
  }
  mod_vars <- all.vars(fixed)
  mod_vars <- mod_vars[mod_vars != as.character(fixed[2])]
  if (!all(mod_vars %in% colnames(combined_data))) {
    stop(paste0("Variable ", mod_vars[which(!(mod_vars %in% colnames(combined_data)))],
                " is not contained in combined_data. Please provide valid variable
                names for the explanatory variables."))
  }
  if (!is.numeric(combined_data[[paste(fixed[2])]])) {
    stop(paste0(as.character(fixed[2])," must be the name of a variable that 
               is a numeric vector."))
  }
  
  if (dim(pop_data)[1] < dim(smp_data)[1]) {
    stop("The population data set cannot have less observations than the 
         sample data set.")
  }
}




################################################################################
# Check all possible allowed combinations

fh_combinations <- function(fixed, vardir, combined_data, domains, method, 
                            interval, k, c, transformation, backtransformation, 
                            eff_smpsize, correlation, corMatrix, time, Ci, tol, 
                            maxit, MSE, mse_type, B, seed, alpha){
  if ((method == "reml" || method == "ml") && correlation == "no" && 
      transformation == "no" && MSE == TRUE && mse_type != "analytical"){
    stop("For the ''reml'' and ''ml'' variance estimation methods without 
         incorporating a correlation structure (correlation = ''no'') and without 
         applying a transformation (transformation = ''no''), the 
         mse_type must be set to ''analytical''. See also help(fh).")
  }
  if ((method == "reml" || method == "ml") && correlation == "no" && 
      is.null(interval)){
    stop("For the ''reml'' and ''ml'' variance estimation methods without 
         incorporating a correlation structure (correlation = ''no''), the 
         argument interval is required and cannot be ''NULL''. See also help(fh).")
  }
  if ((method == "amrl" || method == "ampl" || method == "amrl_yl" || 
       method == "ampl_yl") && MSE == TRUE && mse_type != "analytical"){
    stop("For the adjusted variance estimation methods, the mse_type must be set 
          to ''analytical''. See also help(fh).")
  }
  if ((method == "amrl" || method == "ampl" || method == "amrl_yl" || 
       method == "ampl_yl") && transformation != "no"){
    stop("For the adjusted variance estimation methods, it is not possible to 
         apply a transformation. Transformation must be set to ''no''. See also 
         help(fh).")
  }
  if ((method == "amrl" || method == "ampl" || method == "amrl_yl" || 
       method == "ampl_yl") &&  correlation != "no"){
    stop("For the adjusted variance estimation methods, it is not possible to 
         incorporate a correlation structure. Correlation must be set to ''no''. 
         If correlation is set to ''spatial'' only ''reml'' and ''ml'' variance 
         estimation methods are allowed. See also help(fh).")
  }
  if ((method == "amrl" || method == "ampl" || method == "amrl_yl" || 
       method == "ampl_yl") &&  is.null(interval)){
    stop("For the adjusted variance estimation methods, the argument interval 
         is required and cannot be ''NULL''. See also help(fh).")
  }
  if ((method == "moment") && (is.null(Ci) || is.null(tol) || is.null(maxit))){
    stop("For the measurement error model (method = ''moment''), the arguments 
         Ci, tol and maxit are required and cannot be ''NULL''. See also help(fh).")
  }
  if ((method == "moment") && correlation != "no"){
    stop("For the measurement error model (method = ''moment''), it is not 
         possible to incorporate a correlation structure. Correlation must be 
         set to ''no''. See also help(fh).")
  }
  if ((method == "moment") && transformation != "no"){
    stop("For the measurement error model (method = ''moment''), it is not 
         possible to apply a transformation. Transformation must be set to 
         ''no''. See also help(fh).")
  }
  if ((method == "moment") && MSE == TRUE && mse_type != "jackknife"){
    stop("For the measurement error model (method = ''moment''), mse_type must 
         be set to ''jackknife''. See also help(fh).")
  }
  if ((method == "reblup" || method == "reblupbc") && MSE == TRUE && 
      !(mse_type == "boot" || mse_type == "pseudo")){
    stop("For the robust estimation methods (method = ''reblup''/''reblupbc''), 
          possible mse_types are ''boot'' and ''pseudo''. See also help(fh).")
  }
  if ((method == "reblup" || method == "reblupbc") && transformation != "no"){
    stop("For the robust estimation methods (method = ''reblup''/''reblupbc''), 
         it is not possible to apply a transformation. Transformation must be 
         set to ''no''. See also help(fh).")
  }
  if ((method == "reblup" || method == "reblupbc") && 
      (is.null(k) || is.null(tol) || is.null(maxit))){
    stop("For the robust estimation methods (method = ''reblup''/''reblupbc''), 
         the arguments k, tol and maxit are required and cannot be ''NULL''. 
         See also help(fh).")
  }
  if ((method == "reblupbc") && is.null(c)){
    stop("For the bias corrected robust estimation method (method = ''reblupbc''), 
         the argument c is required and cannot be ''NULL''. See also help(fh).")
  }
  if ((correlation == "spatial") && (is.null(corMatrix) || is.null(tol) || 
                                     is.null(maxit))){
    stop("If correlation is set to ''spatial'' the arguments corMatrix, tol and 
         are required and cannot be ''NULL''. See also help(fh).")
  }
  if ((correlation == "spatial") && (transformation != "no")){
    stop("If correlation is set to ''spatial'', it is not possible to apply a 
         transformation. Transformation must be set to ''no''. See also help(fh).")
  }
  if ((correlation == "spatial") && method == "reml" && MSE == TRUE && 
      !(mse_type == "analytical" || mse_type == "spatialparboot" || 
        mse_type == "spatialnonparboot")){
    stop("If correlation is set to ''spatial'' and ''reml'' variance estimation 
          method is chosen, possible mse_types are ''analytical'', 
          ''spatialparboot'' and ''spatialnonparboot''. See also help(fh).")
  }
  if ((correlation == "spatial") && method == "ml" && MSE == TRUE && 
      !(mse_type == "analytical" || mse_type == "spatialparboot")){
    stop("If correlation is set to ''spatial'' and ''ml'' variance estimation 
          method is chosen, possible mse_types are ''analytical'' and 
          ''spatialparboot''. See also help(fh).")
  }
  if ((transformation != "no") && is.null(backtransformation)){
    stop("If a transformation is chosen, the argument backtransformation is 
         required and cannot be NULL. See also help(fh).")
  }
  if ((transformation == "log") && !(backtransformation == "crude" || 
                                     backtransformation == "sm")){
    stop("If transformation is set to ''log'', possible backtransformations are 
         ''crude'' and ''sm''. See also help(fh).")
  }
  if ((transformation == "log") && (MSE == TRUE) && mse_type != "analytical"){
    stop("If transformation is set to ''log'', the mse_type must be set to 
         ''analytical''. See also help(fh).")
  }
  if ((transformation != "no") && (backtransformation == "sm") && method != "ml"){
    stop("If backtransformation is set to ''sm'', only ''ml'' variance estimation
         is possible. See also help(fh).")
  }
  if ((transformation == "arcsin") && !(backtransformation == "naive" || 
                                     backtransformation == "sm")){
    stop("If transformation is set to ''arcsin'', possible backtransformations 
         are ''naive'' and ''sm''. See also help(fh).")
  }
  if ((transformation == "arcsin") && (MSE == TRUE) && 
      !(mse_type == "boot" || mse_type == "jackknife" || 
        mse_type == "weighted_jackknife")){
    stop("If transformation is set to ''arcsin'', the mse_type must be set to 
         ''boot'', ''jackknife'' or ''weighted_jackknife''. See also help(fh).")
  }
  if ((transformation == "arcsin") && (MSE == TRUE) && (mse_type == "boot") && 
      is.null(eff_smpsize)){
    stop("If transformation is set to ''arcsin'' and the mse_type ''boot'' is 
         chosen, the argument eff_smpsize is required and cannot be NULL. 
         See also help(fh).")
  }
}



################################################################################
# Check of arguments for the combine_data function.

combine_data_check <- function(pop_data, pop_domains, smp_data, smp_domains, vardir){
  
  if (!is.data.frame(pop_data)) {
    stop('Pop_data must be a data frame containing population data. 
         See also help(combine_data).')
  }
  if (!is.character(pop_domains) || length(pop_domains) != 1) {
    stop('Pop_domains must be a vector of length 1 and of class character 
         specifying the variable name of a numeric or factor variable 
         indicating domains in the population data. See also help(combine_data).')
  }
  if (!is.data.frame(smp_data)) { 
    stop('Smp_data must be a data frame containing sample data.
         See also help(combine_data).')
  }
  if (!is.character(smp_domains) || length(smp_domains) != 1) {
    stop('Smp_domains must be a vector of length 1 and of class character 
          specifying the variable (name)  of a numeric or factor variable 
          indicating domains in the sample data. See also help(combine_data).')
  }
  if (!is.character(vardir) || length(vardir) != 1) {
    stop('vardir must be a vector of length 1 and of class character 
          specifying the variable (name)  of a numeric or factor variable 
          indicating the direct variances in the sample data. See also help(combine_data).')
  }
  if (!(pop_domains %in% colnames(pop_data))) {
    stop(paste0("The domain variable ",pop_domains, " is not contained in pop_data.
                Please provide valid variable name for pop_domains."))
  }
  if (!(smp_domains %in% colnames(smp_data))) {
    stop(paste0("The domain variable ",smp_domains, " is not contained in smp_data.
                Please provide valid variable name for smp_domains."))
  }
  if (!(vardir %in% colnames(smp_data))) {
    stop(paste0("The variable ",vardir, " is not contained in smp_data.
                Please provide valid variable name for the direct variances."))
  }
  if (!all(unique(as.character(smp_data[[smp_domains]])) %in% 
           unique(as.character(pop_data[[pop_domains]])))) {
    stop('The sample data contains domains that are
         not contained in the population data.')
  }
  if (dim(pop_data)[1] < dim(smp_data)[1]) {
    stop("The population data set cannot have less observations than the 
         sample data set.")
  }
}