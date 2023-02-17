# Internal documentation -------------------------------------------------------

# The function notation defines the notational framework for the EBP approach
# e.g. number of households in population or sample (per domain), distinction
# between in-sample and out-of-sample
# see Molina and Rao (2003) p.370-371


framework_ebp <- function(fixed, pop_data, pop_domains, smp_data, smp_domains,
                          threshold, custom_indicator = NULL, na.rm,
                          aggregate_to = NULL, weights, pop_weights) {

  # Reduction of number of variables
  mod_vars <- all.vars(fixed)
  mod_vars <- mod_vars[mod_vars != as.character(fixed[2])]
  smp_vars <- c(as.character(fixed[2]), mod_vars, smp_domains, weights)
  pop_vars <- c(mod_vars, pop_domains, aggregate_to, pop_weights)
  smp_data <- smp_data[, smp_vars]
  weights <- weights
  pop_weights <- pop_weights
  fw_check1(
    pop_data = pop_data, mod_vars = mod_vars, pop_domains = pop_domains,
    smp_data = smp_data, aggregate_to = aggregate_to, fixed = fixed,
    smp_domains = smp_domains, threshold = threshold, weights = weights,
    pop_weights = pop_weights
  )


  pop_data <- pop_data[, pop_vars]


  # Deletion of NA
  if (na.rm == TRUE) {
    pop_data <- na.omit(pop_data)
    smp_data <- na.omit(smp_data)
  } else if (any(is.na(pop_data)) || any(is.na(smp_data))) {
    stop(strwrap(prefix = " ", initial = "",
                 "EBP does not work with missing values. Set na.rm = TRUE in
                 function ebp."))
  }


  # Order of domains
  pop_data <- pop_data[order(pop_data[[pop_domains]]), ]

  levels_tmp <- unique(pop_data[[pop_domains]])
  pop_data[[pop_domains]] <- factor(pop_data[[pop_domains]],
                                    levels = levels_tmp)
  pop_domains_vec <- pop_data[[pop_domains]]

  smp_data[[smp_domains]] <- factor(smp_data[[smp_domains]],
                                    levels = levels_tmp)

  if(is.null(aggregate_to) != TRUE){
    levels_tmp <- unique(pop_data[[aggregate_to]])
    pop_data[[aggregate_to]] <- factor(pop_data[[aggregate_to]],
                                       levels = levels_tmp)
    aggregate_to_vec <- pop_data[[aggregate_to]]
  }else{
    aggregate_to_vec <- NULL
  }


  rm(levels_tmp)
  smp_data <- smp_data[order(smp_data[[smp_domains]]), ]


  smp_domains_vec <- smp_data[[smp_domains]]
  smp_domains_vec <- droplevels(smp_domains_vec)


  fw_check2(
    pop_domains = pop_domains, pop_domains_vec = pop_domains_vec,
    smp_domains = smp_domains, smp_domains_vec = smp_domains_vec,
    aggregate_to = aggregate_to, aggregate_to_vec = aggregate_to_vec
  )


  # Number of households in population
  N_pop <- length(pop_domains_vec)
  # Number of households in sample
  N_smp <- length(smp_domains_vec)
  # Number of out-of-sample households
  N_unobs <- N_pop - N_smp
  # Number of domains in the population
  N_dom_pop <- length(unique(pop_domains_vec))
  # Number of domains in the population on aggregated level
  N_dom_pop_agg <- length(unique(aggregate_to_vec))
  # Number of domains in the sample
  N_dom_smp <- length(unique(smp_domains_vec))
  # Number of out-of-sample domains
  N_dom_unobs <- N_dom_pop - N_dom_smp
  # Number of households in population per domain
  n_pop <- as.vector(table(pop_domains_vec))
  # Number of households in sample per domain
  smp_domains_vec_tmp <- as.numeric(smp_domains_vec)
  n_smp <- as.vector(table(smp_domains_vec_tmp))

  # Indicator variables that indicate if domain is in- or out-of-sample
  obs_dom <- pop_domains_vec %in% unique(smp_domains_vec)
  dist_obs_dom <- unique(pop_domains_vec) %in% unique(smp_domains_vec)

  fw_check3(
    obs_dom = obs_dom, dist_obs_dom = dist_obs_dom, pop_domains = pop_domains,
    smp_domains = smp_domains
  )

  indicator_list <- list(
    fast_mean = function(y, w, threshold) {
      t(weighted.mean(y, w))
    },
    hcr = function(y, w, threshold) {
       t(weighted.mean(y < threshold, w))
    },
    pgap = function(y, w, threshold) {
      sum((1 - (y[y < threshold]/threshold)) * w[y < threshold])/sum(w)
    },
    gini = function(y, w, threshold) {
        n <- length(y)
        w <- w[order(y)]
        y <- sort(y)
        auc <- sum((cumsum(c(0, (y * w)[1:(n-1)])) + ((y * w) / 2)) * w)
        auc <- (auc / sum(w)) / sum((y * w))
        G <- 1 - (2* auc)
        return(G)
    },
    qsr = function(y, w, threshold) {
       quant14 <- wtd.quantile(
         x = y,
         weights = w,
         probs = c(0.2, 0.8)
       )

       iq1 <- y <= quant14[1]
       iq4 <- y > quant14[2]
       t((sum(w[iq4] * y[iq4]) / sum(w[iq4])) /
           (sum(w[iq1] * y[iq1]) / sum(w[iq1])))
    },
    quants = function(y, w, threshold) {
      t(wtd.quantile(x = y, weights = w, probs = c(.10, .25, .5, .75, .9)))
    }
  )

  indicator_names <- c(
    "Mean",
    "Head_Count",
    "Poverty_Gap",
    "Gini",
    "Quintile_Share",
    "Quantile_10",
    "Quantile_25",
    "Median",
    "Quantile_75",
    "Quantile_90"
  )


  if (!is.null(custom_indicator) && length(custom_indicator) > 0) {
    indicator_list <- c(indicator_list, custom_indicator)
    indicator_names <- c(indicator_names, names(custom_indicator))
  }

  if (is.null(threshold)) {
    threshold <- 0.6 * median(smp_data[[paste(fixed[2])]])
    message(strwrap(prefix = " ", initial = "",
                    paste0("The threshold for the HCR and the PG is
                          automatically set to 60% of the median of the
                          dependent variable and equals ", threshold)))
  }


  return(list(
    pop_data = pop_data,
    pop_domains_vec = pop_domains_vec,
    smp_data = smp_data,
    smp_domains_vec = smp_domains_vec,
    smp_domains = smp_domains,
    aggregate_to = aggregate_to,
    aggregate_to_vec = aggregate_to_vec,
    N_pop = N_pop,
    N_smp = N_smp,
    N_unobs = N_unobs,
    N_dom_pop = N_dom_pop,
    N_dom_pop_agg = N_dom_pop_agg,
    N_dom_smp = N_dom_smp,
    N_dom_unobs = N_dom_unobs,
    n_pop = n_pop,
    n_smp = n_smp,
    obs_dom = obs_dom,
    dist_obs_dom = dist_obs_dom,
    indicator_list = indicator_list,
    indicator_names = indicator_names,
    threshold = threshold,
    weights = weights,
    pop_weights = pop_weights
  ))
}
