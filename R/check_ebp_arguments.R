# This script contains the checks of arguments that have be done for the
# ebp function.


# Function called in ebp
ebp_check1 <- function(fixed, pop_data, pop_domains, smp_data, smp_domains, L) {
  if (is.null(fixed) || !inherits(fixed, "formula")) {
    stop("Fixed must be a formula object. See also help(ebp).")
  }
  if (!is.data.frame(pop_data)) {
    stop("Pop_data must be a data frame containing population data.
           See also help(ebp).")
  }
  if (!is.character(pop_domains) || length(pop_domains) != 1) {
    stop("Pop_domains must be a vector of lenght 1 and of class character
         specifying the variable name of a numeric or factor variable
         indicating domains in the population data. See also help(ebp).")
  }
  if (!is.data.frame(smp_data)) {
    stop("Smp_data must be a data frame containing sample data.
           See also help(ebp).")
  }
  if (!is.character(smp_domains) || length(smp_domains) != 1) {
    stop("Smp_domains must be a vector of lenght 1 and of class character
          specifying the variable (name)  of a numeric or factor variable
          indicating domains in the sample data. See also help(ebp).")
  }

  if (!is.numeric(L) || length(L) != 1 || L < 1) {
    stop("L needs to be a single value, interpreted as an integer, determining
          the number of Monte-Carlo simulations. The value must be at least
          1. See also help(ebp).")
  }
  if (!all(unique(as.character(smp_data[[smp_domains]])) %in%
    unique(as.character(pop_data[[pop_domains]])))) {
    stop("The sample data contains domains that are not contained in the
         population data.")
  }
}

ebp_check2 <- function(threshold, transformation, interval, MSE, boot_type, B,
                       custom_indicator, cpus, seed, na.rm, weights) {
  if (!is.null(threshold) && !(is.numeric(threshold) &&
    length(threshold) == 1) && !inherits(threshold, "function")) {
    stop("threshold needs to be a single numeric value or a function of y.
          If it is NULL 60% of the median is selected as threshold.
         See also help(ebp).")
  }
  if (inherits(threshold, "function") &&
    !all(attributes(formals(threshold))$names == c("y"))) {
    stop("If threshold is a function the argument needs to be y and only y. Also
          a single numeric value is possible as threshold. If it is
          NULL 60% of the median of the target variable is selected as
          threshold. See also help(ebp).")
  }
  if (is.null(transformation) || !(transformation == "box.cox" ||
    transformation == "log" ||
    transformation == "dual" ||
    transformation == "log.shift" ||
    transformation == "no")) {
    stop("The five options for transformation are ''no'', ''log'', ''box.cox'',
         ''dual'' or ''log.shift''.")
  }
  if (any(interval != "default") & (!is.vector(interval, mode = "numeric") ||
    length(interval) != 2 || !(interval[1] < interval[2]))) {
    stop("interval needs to be a numeric vector of length 2
              defining a lower and upper limit for the estimation of the optimal
              transformation parameter. The value of the lower limit needs to be
              smaller than the upper limit. You can also choose 'default'.
              See also help(ebp).")
  }
  if (transformation == "dual" & any(interval < 0)) {
    stop("For the dual transformation, lambda needs to be positive, so the lower
          limit of the interval cannot be negative. See also help(ebp).")
  }
  if (!is.logical(MSE) || length(MSE) != 1) {
    stop("MSE must be a logical value. Set MSE to TRUE or FALSE. See also
         help(ebp).")
  }
  if (is.null(boot_type) || !(length(boot_type) == 1 &&
    (boot_type == "parametric" ||
      boot_type == "wild"))) {
    stop("The two bootstrap procedures are ''parametric'' or ''wild''.")
  }
  if (MSE == TRUE && !(is.numeric(B) && length(B) == 1 && B > 1)) {
    stop("If MSE is set to TRUE, a single numeric value for the number of
         bootstrap sample needs to be chosen that is greater than 1. See also
         help(ebp).")
  }
  if (!is.numeric(cpus) || !(is.numeric(cpus) && length(cpus) == 1)) {
    stop("Cpus must be a single number determining the number of kernels for the
         parallelization.")
  }
  if (!is.null(seed) && (!is.numeric(seed) ||
    !(is.numeric(seed) && length(seed) == 1))) {
    stop("The seed must be a single value, interpreted as an integer, or NULL
         See also help(ebp).")
  }
  if (!is.null(custom_indicator)) {
    if (!inherits(custom_indicator, "list")) {
      stop("Additional indicators need to be added in argument custom_indicator
           as a list of functions. For help see Example 2 in help(ebp).")
    }

    N_custom <- length(custom_indicator)
    for (i in seq_len(N_custom)) {
      if (!inherits(custom_indicator[[i]], "function")) {
        stop("The elements of the list need to be functions. These Functions
             for custom indicators need to have exactly the following
             two arguments: y, threshold; even though a threshold might not
             included in the indicator. For help see Example 2 in help(ebp).")
      } else if (inherits(custom_indicator[[i]], "function") &&
        !all(names(formals(custom_indicator[[i]])) ==
          c("y", "threshold"))) {
        stop("Functions for custom indicators need to have exactly the following
             two arguments: y, threshold; even though a threshold might not
             included in the indicator. For help see Example 2 in help(ebp).")
      }
    }
  }
  if (!(inherits(na.rm, "logical") && length(na.rm) == 1)) {
    stop("na.rm needs to be a logical value. Set na.rm to TRUE or FALSE. See
         also help(ebp).")
  }
  if (is.character(weights) && length(weights) != 1 || !is.character(weights) &&
    !is.null(weights)) {
    stop("Weights must be a vector of lenght 1 and of class character
         specifying the variable name of a numeric variable
         indicating weights in the sample data. See also help(ebp).")
  }
  if (!is.null(weights) && !(transformation == "log" ||
    transformation == "no")) {
    stop("Weighted ebp can only be used without transformation or the log-
    transformation")
  }
  if (!is.null(weights) && isTRUE(MSE) && boot_type == "wild") {
    stop("The weighted version of ebp is only available with the ''parametric''
         bootstrap.")
  }
}


# Functions called in notation
fw_check1 <- function(pop_data, mod_vars, pop_domains, smp_data,
                      fixed, smp_domains, threshold, weights) {
  if (!all(mod_vars %in% colnames(pop_data))) {
    stop(paste0(
      "Variable ",
      mod_vars[which(!(mod_vars %in% colnames(smp_data)))], " is not
         contained in pop_data. Please provide valid variable names for the
         explanatory variables."
    ))
  }
  if (!(pop_domains %in% colnames(pop_data))) {
    stop(paste0("The domain variable ", pop_domains, " is not contained
                in pop_data. Please provide valid variable name for
                pop_domains."))
  }
  if (!all(mod_vars %in% colnames(smp_data))) {
    stop(paste0(
      "Variable ",
      mod_vars[which(!(mod_vars %in% colnames(smp_data)))], " is not
                contained in smp_data. Please provide valid variable names for
                the explanatory variables."
    ))
  }
  if (!(smp_domains %in% colnames(smp_data))) {
    stop(paste0("The domain variable ", smp_domains, " is not contained in
                smp_data. Please provide valid variable name for smp_domains."))
  }
  if (!((as.character(fixed[2])) %in% colnames(smp_data))) {
    stop(paste0("Variable ", as.character(fixed[2]), " is not contained in
                smp_data. Please provide valid variable name for the dependent
                variable."))
  }

  if (!is.numeric(smp_data[[paste(fixed[2])]])) {
    stop(paste0(as.character(fixed[2]), " must be the name of a variable that
               is a numeric vector."))
  }
  if (is.character(weights)) {
    if (!(weights %in% colnames(smp_data))) {
      stop(paste0("The weights variable ", weights, " is not contained in
                smp_data. Please provide a valid variable name for the weights
                variable."))
    }
  }
  if (is.character(weights)) {
    if (!is.numeric(smp_data[[weights]])) {
      stop(paste0("The variable ", weights, " must be the name of a variable
           that is a numeric vector."))
    }
  }
  if (is.character(weights)) {
    if (!all(smp_data[[weights]] >= 1)) {
      stop(paste0("Negativ or zero weights are included in ", weights, " Please
                remove obersvations with weight values smaller than 1."))
    }
  }

  if (dim(pop_data)[1] < dim(smp_data)[1]) {
    stop("The population data set cannot have less observations than the
         sample data set.")
  }

  if (inherits(threshold, "function") &&
    (!is.numeric(threshold(smp_data[[paste(fixed[2])]])) ||
      length(threshold(smp_data[[paste(fixed[2])]])) != 1)) {
    stop("The threshold function must return a single numeric value when
         evaluated with the dependent variable.")
  }
}




fw_check2 <- function(pop_domains, pop_domains_vec, smp_domains,
                      smp_domains_vec) {
  if (!(is.numeric(pop_domains_vec) ||
    any(inherits(pop_domains_vec, "factor")))) {
    stop(paste0(pop_domains, " needs to be the name of a variable that is
                numeric or a (ordered) factor."))
  }
  if (!(is.numeric(smp_domains_vec) ||
    any(inherits(smp_domains_vec, "factor")))) {
    stop(paste0(smp_domains, " needs to be the name of a variable that is
         numeric or a (ordered) factor."))
  }
  if ((is.numeric(pop_domains_vec) &&
    any(inherits(smp_domains_vec, "factor"))) ||
    (is.numeric(smp_domains_vec) &&
      any(inherits(pop_domains_vec, "factor")))) {
    stop(paste0(pop_domains, " and ", smp_domains, " need to be names of
                variables that are of the same class (factor and ordered factor
                are considered to be the same class). See also help(ebp)."))
  }
}

fw_check3 <- function(obs_dom, dist_obs_dom, pop_domains, smp_domains) {
  if (sum(obs_dom) == 0 || sum(dist_obs_dom) == 0) {
    stop(paste0(pop_domains, " and ", smp_domains, " do not have any value in
                common. Do really both variables indicate the same domains in
                population data and sample data, respectively?"))
  }
}
