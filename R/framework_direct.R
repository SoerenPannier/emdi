# This function prepares and adjusts the data in that way how it is used and
# needed in the following direct estimation.


framework_dir <- function(y, smp_data, smp_domains, weights,
                          threshold, custom_indicator, na.rm) {
  if (isTRUE(na.rm)) {
    indices <- !is.na(smp_data[y])
    if (!is.null(weights)) {
      indices <- indices & !is.na(smp_data[weights])
    }
    smp_data <- smp_data[indices, ]
  } else if (any(is.na(smp_data[y]))) {
    warning(strwrap(prefix = " ", initial = "",
                    "There are NA-Values in the target variable and na.rm is
                    set FALSE. Therefore, only quantiles are estimated"))
  } else if (!is.null(weights) && any(is.na(smp_data[weights]))) {
    warning(strwrap(prefix = " ", initial = "",
                    "There are NA-Values in the weights variable and na.rm is
                    set FALSE. Therefore, only quantiles are estimated"))
  }

  # two versions of y, one vector version and one character version (original)
  # for variance estimation
  # we force sample data, in laeken data can be NULL
  y_vec <- smp_data[, y]
  # Number of households in sample
  N_smp <- length(y_vec)

  if (!is.null(weights)) {
    weights_vec <- smp_data[, weights]
  } else if (is.null(weights)) {
    weights_vec <- rep.int(1, N_smp)
  }

  byDomain <- !is.null(smp_domains)
  if (byDomain) {
    smp_domains_vec <- as.factor(as.character(smp_data[, smp_domains]))
    smp_domains_vec <- droplevels(smp_domains_vec)
    rs <- levels(smp_domains_vec)
    # Number of domains in the sample
    N_dom_smp <- length(unique(smp_domains_vec))
    # Number of households in sample per domain
    smp_domains_vec_tmp <- as.numeric(smp_domains_vec)
    n_smp <- as.vector(table(smp_domains_vec_tmp))
  }

  if (is.null(threshold)) {
    if (is.null(weights)) {
      threshold <- 0.6 * median(y_vec)
      message(strwrap(prefix = " ", initial = "",
                      paste0("The threshold for the HCR and the PG is automatically
                      set to 60% of the median of the dependent variable and
                      equals ", threshold)))
    } else if (!is.null(weights)) {
      threshold <- 0.6 * wtd.quantile(
        x = y_vec,
        weights = weights_vec,
        probs = .5
      )
      message(strwrap(prefix = " ", initial = "",
                      paste0("The threshold for the HCR and the PG is
                      automatically set to 60% of the weighted median of the
                      dependent variable and equals ", threshold))
      )
    }
  }

  indicator_list <- getIndicatorList_fixed()

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
    for(i in 1:length(custom_indicator)) {
      formals(custom_indicator[[i]]) <- alist(y=, weights=, threshold=)
    }

    indicator_list <- c(indicator_list, custom_indicator)
    indicator_names <- c(indicator_names, names(custom_indicator))
  }

  return(list(
    smp_data = smp_data,
    y = y,
    y_vec = y_vec,
    weights = weights,
    weights_vec = weights_vec,
    smp_domains_vec = smp_domains_vec,
    smp_domains = smp_domains,
    byDomain = byDomain,
    rs = rs,
    N_smp = N_smp,
    N_dom_smp = N_dom_smp,
    n_smp = n_smp,
    indicator_list = indicator_list,
    indicator_names = indicator_names,
    threshold = threshold
  ))
}

getIndicatorList_fixed <- function() {
  list(
    mean_wrap = function(y,
                         weights,
                         threshold) {
      weighted.mean(x = y, w = weights)
    },
    hcr_wrap = function(y,
                        weights,
                        threshold) {
      sw <- sum(weights)
      sum(weights[y < threshold]) / sw
    },
    pgap_wrap = function(y,
                         weights,
                         threshold) {
      sw <- sum(weights)
      sum(weights * (y < threshold) * (threshold - y) / threshold) / sw
    },
    gini_wrap = function(y,
                         weights,
                         threshold) {
      ord <- order(y)
      y <- y[ord]
      weights <- weights[ord]
      wy <- weights * y
      sw <- sum(weights)
      cw <- cumsum(weights)
      ((2 * sum(wy * cw) - sum(weights^2 * y)) / (sw * sum(wy)) - 1)
    },
    qsr_wrap = function(y,
                        weights,
                        threshold) {
      quant14 <- wtd.quantile(
        x = y, weights = weights,
        probs = c(.2, .8)
      )
      iq1 <- y <= quant14[1]
      iq4 <- y > quant14[2]
      (sum(weights[iq4] * y[iq4]) /
        sum(weights[iq4])) / (sum(weights[iq1] * y[iq1]) / sum(weights[iq1]))
    },
    quant10_wrap = function(y,
                            weights,
                            threshold) {
      wtd.quantile(
        x = y, weights = weights,
        probs = .10
      )
    },
    quant25_wrap = function(y,
                            weights,
                            threshold) {
      wtd.quantile(
        x = y, weights = weights,
        probs = .25
      )
    },
    quant50_wrap = function(y,
                            weights,
                            threshold) {
      wtd.quantile(
        x = y, weights = weights,
        probs = .50
      )
    },
    quant75_wrap = function(y,
                            weights,
                            threshold) {
      wtd.quantile(
        x = y, weights = weights,
        probs = .75
      )
    },
    quant90_wrap = function(y,
                            weights,
                            threshold) {
      wtd.quantile(
        x = y, weights = weights,
        probs = .9
      )
    }
  )
}

wtd.quantile <- function(x, weights = NULL, probs = NULL) {
  n <- length(x)
  order <- order(x)
  x <- x[order]
  weights <- weights[order]
  if (is.null(weights)) {
    rw <- seq_len(n) / n
  } else {
    rw <- cumsum(weights) / sum(weights)
  }
  q <- vapply(probs, function(p) {
    if (p == 0) {
      return(x[1])
    } else if (p == 1) {
      return(x[n])
    }
    select <- min(which(rw >= p))
    if (rw[select] == p) {
      mean(x[select:(select + 1)])
    } else {
      x[select]
    }
  }, numeric(1))
  return(unname(q))
}
