fh_tf_check <- function(fixed, vardir, combined_data, domains, subdomains,
                     transformation, eff_smpsize, maxit, MSE, B, seed) {
  if (is.null(fixed) || !inherits(fixed, "formula")) {
    stop("Fixed must be a formula object. See also help(fh_tf).")
  }
  if (!all(!is.na(combined_data[all.vars(fixed)[-1]]))) {
    stop(paste0("The auxiliary variables must not contain NAs."))
  }
  if (is.null(vardir) || !(vardir %in% colnames(combined_data))) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0("The sampling variances variable ", vardir, " is not
                 contained in combined_data. Please provide valid variable name
                 for the sampling variances.")))
  }
  if (is.null(combined_data) || !is.data.frame(combined_data)) {
    stop(strwrap(prefix = " ", initial = "",
                 "combined_data must be a data frame containing the direct
                 estimates, the sampling variances, the explanatory variables,
                 the domains, and the subdomains. If the arcsin transformation in combination
                 with bootstrap mse is chosen, also eff_smpsize needs to be
                 included. See also help(fh_tf)."))
  }
  if (!is.null(domains) && (!is.character(domains) || length(domains) != 1 ||
                            !(domains %in% colnames(combined_data)))) {
    stop(strwrap(prefix = " ", initial = "",
                 "domains must be a vector of length 1 and of class character
                 specifying the variable name of a numeric or factor variable
                 indicating domains in the combined_data dataframe. See also
                 help(fh_tf)."))
  }

  if (!is.null(subdomains) && (!is.character(subdomains) || length(subdomains) != 1 ||
                            !(subdomains %in% colnames(combined_data)))) {
    stop(strwrap(prefix = " ", initial = "",
                 "subdomains must be a vector of length 1 and of class character
                 specifying the variable name of a numeric or factor variable
                 indicating subdomains in the combined_data dataframe. See also
                 help(fh_tf)."))
  }

  if (is.null(transformation) || !is.character(transformation) ||
      !(transformation == "arcsin" || transformation == "log" ||
        transformation == "no")) {
    stop(strwrap(prefix = " ", initial = "",
                 "transformation must be a character. The three options are
                 ''no'', ''log'' or ''arcsin''."))
  }

  if (!is.na(eff_smpsize) && !(eff_smpsize %in% colnames(combined_data))) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0("The effective sample size variable ", eff_smpsize,
                        " is not contained in combined_data. Please provide valid
                 variable name for the effective sample size.")))
  }

  if (!is.logical(MSE) || length(MSE) != 1) {
    stop(strwrap(prefix = " ", initial = "",
                 "MSE must be a logical value. Set MSE to TRUE or FALSE. The
                 default is set to FALSE. See also help(fh_tf)."))
  }

  if (!(is.numeric(B) && (length(B) == 1 || length(B) == 2))) {
    stop(strwrap(prefix = " ", initial = "",
                 "B needs to be a single number of bootstraps for
                 the MSE estimation. See also help(fh_tf)."))
  }

  if (!is.null(seed) && (!is.numeric(seed) ||
                         !(is.numeric(seed) && length(seed) == 1))) {
    stop(strwrap(prefix = " ", initial = "",
                 "The seed must be a single value, interpreted as an integer,
                 or NULL See also help(fh_tf)."))
  }
}

