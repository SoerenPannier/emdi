# This script contains the checks of arguments that have be done for the
# ebp function.


# Function called in ebp
ebp_tf_check1 <- function(fixed, pop_data, pop_domains,pop_subdomains, smp_data,
                       smp_domains, smp_subdomains, L) {
  if (is.null(fixed) || !inherits(fixed, "formula")) {
    stop("Fixed must be a formula object. See also help(ebp).")
  }
  if (!is.data.frame(pop_data)) {
    stop(strwrap(prefix = " ", initial = "",
                 "Pop_data must be a data frame containing population data.
                 See also help(ebp)."))
  }
  if (!is.character(pop_domains) || length(pop_domains) != 1) {
    stop(strwrap(prefix = " ", initial = "",
                 "Pop_domains must be a vector of length 1 and of class
                 character specifying the variable name of a numeric or factor
                 variable indicating domains in the population data. See also
                 help(ebp_tf)."))
  }
  if (!is.character(pop_subdomains) || length(pop_subdomains) != 1) {
    stop(strwrap(prefix = " ", initial = "",
                 "Pop_subdomains must be a vector of length 1 and of class
                 character specifying the variable name of a numeric or factor
                 variable indicating subdomains in the population data. See also
                 help(ebp_tf)."))
  }
  if (!is.data.frame(smp_data)) {
    stop(strwrap(prefix = " ", initial = "",
                 "Smp_data must be a data frame containing sample data.
                 See also help(ebp_tf)."))
  }
  if (!is.character(smp_domains) || length(smp_domains) != 1) {
    stop(strwrap(prefix = " ", initial = "",
                 "Smp_domains must be a vector of length 1 and of class
                 character specifying the variable (name)  of a numeric or
                 factor variable indicating domains in the sample data. See
                 also help(ebp_tf)."))
  }
  if (!is.character(smp_subdomains) || length(smp_subdomains) != 1) {
    stop(strwrap(prefix = " ", initial = "",
                 "Smp_subdomains must be a vector of length 1 and of class
                 character specifying the variable (name)  of a numeric or
                 factor variable indicating subdomains in the sample data. See
                 also help(ebp_tf)."))
  }

  if (!is.numeric(L) || length(L) != 1 || L < 1) {
    stop(strwrap(prefix = " ", initial = "",
                 "L needs to be a single value, interpreted as an integer,
                 determining the number of Monte-Carlo simulations. The value
                 must be at least 1. See also help(ebp_tf)."))
  }
  if (!all(unique(as.character(smp_data[[smp_domains]])) %in%
    unique(as.character(pop_data[[pop_domains]])))) {
    stop(strwrap(prefix = " ", initial = "",
                "The sample data contains domains that are not contained in the
                population data."))
  }
  if (!all(unique(as.character(smp_data[[smp_subdomains]])) %in%
           unique(as.character(pop_data[[pop_subdomains]])))) {
    stop(strwrap(prefix = " ", initial = "",
                 "The sample data contains subdomains that are not contained in the
                population data."))
  }
}

ebp_tf_check2 <- function(threshold, transformation, interval, MSE, boot_type, B,
                       custom_indicator, cpus, seed, na.rm, weights,
                       pop_weights) {
  if (!is.null(threshold) && !(is.numeric(threshold) &&
    length(threshold) == 1) && !inherits(threshold, "function")) {
    stop(strwrap(prefix = " ", initial = "",
                 "threshold needs to be a single numeric value or a function
                 of y. If it is NULL 60% of the median is selected as threshold.
                 See also help(ebp_tf)."))
  }
  if (inherits(threshold, "function") &&
    !all(attributes(formals(threshold))$names == c("y"))) {
    stop(strwrap(prefix = " ", initial = "",
                 "If threshold is a function the argument needs to be y and
                 only y. Also a single numeric value is possible as threshold.
                 If it is NULL 60% of the median of the target variable is
                 selected as threshold. See also help(ebp_tf)."))
  }
  if (is.null(transformation) || !(transformation == "box.cox" ||
    transformation == "log" ||
    transformation == "dual" ||
    transformation == "log.shift" ||
    transformation == "no")) {
    stop(strwrap(prefix = " ", initial = "",
                 "The five options for transformation are ''no'', ''log'',
                 ''box.cox'', ''dual'' or ''log.shift''."))
  }
  if (any(interval != "default") & (!is.vector(interval, mode = "numeric") ||
    length(interval) != 2 || !(interval[1] < interval[2]))) {
    stop(strwrap(prefix = " ", initial = "",
                 "interval needs to be a numeric vector of length 2 defining a
                 lower and upper limit for the estimation of the optimal
                 transformation parameter. The value of the lower limit needs
                 to be smaller than the upper limit. You can also choose
                 'default'. See also help(ebp)."))
  }
  if (transformation == "dual" & any(interval < 0)) {
    stop(strwrap(prefix = " ", initial = "",
                 "For the dual transformation, lambda needs to be positive, so
                 the lower limit of the interval cannot be negative. See also
                 help(ebp)."))
  }
  if (!is.logical(MSE) || length(MSE) != 1) {
    stop(strwrap(prefix = " ", initial = "",
                 "MSE must be a logical value. Set MSE to TRUE or FALSE. See
                 also help(ebp)."))
  }
  if (is.null(boot_type) || !(length(boot_type) == 1 &&
    (boot_type == "parametric" ||
      boot_type == "wild"))) {
    stop("The two bootstrap procedures are ''parametric'' or ''wild''.")
  }
  if (MSE == TRUE && !(is.numeric(B) && length(B) == 1 && B > 1)) {
    stop(strwrap(prefix = " ", initial = "",
                 "If MSE is set to TRUE, a single numeric value for the number
                 of bootstrap sample needs to be chosen that is greater than 1.
                 See also help(ebp)."))
  }
  if (!is.numeric(cpus) || !(is.numeric(cpus) && length(cpus) == 1)) {
    stop(strwrap(prefix = " ", initial = "",
                 "Cpus must be a single number determining the number of
                 kernels for the parallelization."))
  }
  if (!is.null(seed) && (!is.numeric(seed) ||
    !(is.numeric(seed) && length(seed) == 1))) {
    stop(strwrap(prefix = " ", initial = "",
                 "The seed must be a single value, interpreted as an integer,
                 or NULL See also help(ebp)."))
  }
  if (!is.null(custom_indicator)) {
    if (!inherits(custom_indicator, "list")) {
      stop(strwrap(prefix = " ", initial = "",
                   "Additional indicators need to be added in argument
                   custom_indicator as a list of functions. For help see
                   Example 2 in help(ebp_tf)."))
    }

    N_custom <- length(custom_indicator)
    for (i in seq_len(N_custom)) {
      if(!is.null(pop_weights)) {
        #if(!all(c("y", "pop_weights") %in%
         #       names(formals(custom_indicator[[i]])))) {
        #stop(strwrap(prefix = " ", initial = "",
                 #    "Please provide the argument pop_weights to your
                  #   custom_indicator. All other indicators will be
                   #  calculated using population weights."))
        stop(strwrap(prefix = " ", initial = "",
                     "EBP twofold implementation with weights is not available.
                     Maybe try setting weights to NULL. See also help(ebp_tf)"))
       # }
      }
      if (!inherits(custom_indicator[[i]], "function")) {
        stop(strwrap(prefix = " ", initial = "",
                     "The elements of the list need to be functions. These
                     functions for custom indicators need to have the
                     argument y and threshold. For help see
                     Example 2 in help(ebp_tf)."))
      } else if (inherits(custom_indicator[[i]], "function") &&
        !all(names(formals(custom_indicator[[i]])) %in%
          c("y", "pop_weights", "threshold"))) {
        stop(strwrap(prefix = " ", initial = "",
                     "Functions for custom indicators need to have exactly the
                     following argument y and optional the arguments
                     pop_weights, threshold. For help see Example 2 in
                     help(ebp_tf)."))
      }
    }
  }
  if (!(inherits(na.rm, "logical") && length(na.rm) == 1)) {
    stop(strwrap(prefix = " ", initial = "",
                 "na.rm needs to be a logical value. Set na.rm to TRUE or FALSE.
                 See also help(ebp)."))
  }
  if (!is.null(weights)) {
    stop(strwrap(prefix = " ", initial = "",
                 "EBP twofold implementation with weights is not available.
                  Maybe try setting weights to NULL. See also help(ebp_tf)."))
  }
 # if (!is.null(weights) && !(transformation == "log" ||
  #  transformation == "no")) {
   # stop(strwrap(prefix = " ", initial = "",
    #             "Weighted ebp can only be used without transformation or the
     #            log-transformation"))
  #}
#  if (!is.null(weights) && isTRUE(MSE) && boot_type == "wild") {
   # stop(strwrap(prefix = " ", initial = "",
          #       "The weighted version of ebp is only available with the
             #    ''parametric'' bootstrap."))
 # }
 # if (is.character(pop_weights) && length(pop_weights) != 1 ||
 #     !is.character(pop_weights) && !is.null(pop_weights)) {
   # stop(strwrap(prefix = " ", initial = "",
    #             "Pop_weights must be a vector of length 1 and of class
    #             character specifying the variable name of a numeric variable
    #             indicating weights in the population data. See also
    #             help(ebp)."))
  #}
}


# Functions called in notation
fw_tf_check1 <- function(pop_data, mod_vars, pop_domains, pop_subdomains, smp_data,
                      fixed,  smp_domains, smp_subdomains, aggregate_to,
                      threshold, weights, pop_weights) {
  if (!all(mod_vars %in% colnames(pop_data))) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0("Variable ",
                        mod_vars[which(!(mod_vars %in% colnames(smp_data)))],
                        " is not contained in pop_data. Please provide valid
                        variable names for the explanatory variables."
    )))
  }
  if (!(pop_domains %in% colnames(pop_data))) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0("The domain variable ", pop_domains, " is not contained
                        in pop_data. Please provide valid variable name for
                        pop_domains.")))
  }
  if (!(pop_subdomains %in% colnames(pop_data))) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0("The subdomain variable ", pop_subdomains, " is not contained
                        in pop_data. Please provide valid variable name for
                        pop_subdomains.")))
  }
  if (!all(mod_vars %in% colnames(smp_data))) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0("Variable ",
                        mod_vars[which(!(mod_vars %in% colnames(smp_data)))],
                        " is not contained in smp_data. Please provide valid
                        variable names for the explanatory variables."
    )))
  }
  if (!(smp_domains %in% colnames(smp_data))) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0("The domain variable ", smp_domains, " is not contained
                        in smp_data. Please provide valid variable name for
                        smp_domains.")))
  }
 # if (!(smp_subdomains %in% colnames(smp_data))) {
   # stop(strwrap(prefix = " ", initial = "",
           #      paste0("The subdomain variable ", smp_subdomains, " is not contained
               #        in smp_data. Please provide valid variable name for
                  #      smp_subdomains.")))
 # }
  if (!((as.character(fixed[2])) %in% colnames(smp_data))) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0("Variable ", as.character(fixed[2]), " is not contained
                        in smp_data. Please provide valid variable name for the
                        dependent variable.")))
  }

  if (!is.numeric(smp_data[[paste(fixed[2])]])) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0(as.character(fixed[2]), " must be the name of a
                        variable that is a numeric vector.")))
  }
  if (!is.null(weights)) {
    stop(strwrap(prefix = " ", initial = "",
                 " The weighted version on EBP twofold is not available.
                 Try setting weights to NuLL"))

  }
  if (!is.null(pop_weights)) {
    stop(strwrap(prefix = " ", initial = "",
                 " The weighted version on EBP twofold is not available.
                 Try setting weights to NuLL"))

  }

  if (is.null(pop_weights)) {
    if (dim(pop_data)[1] < dim(smp_data)[1]) {
      stop(strwrap(prefix = " ", initial = "",
                 "The population data set cannot have less observations than
                 the sample data set."))
    }
  }


  if (inherits(threshold, "function") &&
    (!is.numeric(threshold(smp_data[[paste(fixed[2])]])) ||
      length(threshold(smp_data[[paste(fixed[2])]])) != 1)) {
    stop(strwrap(prefix = " ", initial = "",
                 "The threshold function must return a single numeric value
                 when evaluated with the dependent variable."))
  }
}




fw_tf_check2 <- function(pop_domains, pop_subdomains, pop_domains_vec,
                      pop_subdomains_vec, smp_domains, smp_subdomains,
                      smp_domains_vec, smp_subdomains_vec, aggregate_to,
                      aggregate_to_vec) {
  if (!(is.numeric(pop_domains_vec) ||
    any(inherits(pop_domains_vec, "factor")))) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0(pop_domains, " needs to be the name of a variable that
                        is numeric or a (ordered) factor.")))
  }
  if (!(is.numeric(pop_subdomains_vec) ||
        any(inherits(pop_subdomains_vec, "factor")))) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0(pop_subdomains, " needs to be the name of a variable that
                        is numeric or a (ordered) factor.")))
  }
  if (!(is.numeric(smp_domains_vec) ||
    any(inherits(smp_domains_vec, "factor")))) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0(smp_domains, " needs to be the name of a variable that
                        is numeric or a (ordered) factor.")))
  }
  if (!(is.numeric(smp_subdomains_vec) ||
        any(inherits(smp_subdomains_vec, "factor")))) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0(smp_subdomains, " needs to be the name of a variable that
                        is numeric or a (ordered) factor.")))
  }
  if(is.null(aggregate_to) != TRUE){
    if (!(is.numeric(aggregate_to_vec) ||
          any(inherits(aggregate_to_vec, "factor")))) {
      stop(paste0(aggregate_to, " needs to be the name of a variable that is
                  numeric or a (ordered) factor."))
    }
  }
  if ((is.numeric(pop_domains_vec) &&
    any(inherits(smp_domains_vec, "factor"))) ||
    (is.numeric(smp_domains_vec) &&
      any(inherits(pop_domains_vec, "factor")))) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0(pop_domains, " and ", smp_domains, " need to be names
                        of variables that are of the same class (factor and
                        ordered factor are considered to be the same class).
                        See also help(ebp).")))
  }
  if ((is.numeric(pop_subdomains_vec) &&
       any(inherits(smp_subdomains_vec, "factor"))) ||
      (is.numeric(smp_subdomains_vec) &&
       any(inherits(pop_subdomains_vec, "factor")))) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0(pop_subdomains, " and ", smp_subdomains, " need to be names
                        of variables that are of the same class (factor and
                        ordered factor are considered to be the same class).
                        See also help(ebp_tf).")))
  }
}


#______________commented out for dry-run - 10.10.24______________________

#fw_tf_check3 <- function(obs_dom, obs_subdom, dist_obs_dom, dist_obs_subdom,
 #                     pop_domains, pop_subdomains, smp_domains, smp_subdomains) {
 # if (sum(obs_dom) == 0 || sum(dist_obs_dom) == 0) {
  #  stop(strwrap(prefix = " ", initial = "",
       #          paste0(pop_domains, " and ", smp_domains, " do not have any
          #              value in common. Do both variables really indicate the
           #             same domains in population data and sample data,
             #           respectively?")))
 # }
  #if (sum(obs_subdom) == 0 || sum(dist_obs_subdom) == 0) {
   # stop(strwrap(prefix = " ", initial = "",
        #         paste0(pop_subdomains, " and ", smp_subdomains, " do not have any
             #           value in common. Do both variables really indicate the
              #          same subdomains in population data and sample data,
              #          respectively?")))
 # }
#}
