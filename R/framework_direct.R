# This function prepares and adjusts the data in that way how it is used and 
# needed in the following direct estimation. 


framework_dir <- function(y, smp_data, smp_domains, weights, sort, 
                          pov_line, na.rm){
  
  
  
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
  
  if (!is.null(sort)) {
    sort <- smp_data[, sort]
  }
  byDomain <- !is.null(smp_domains)
  if (byDomain) {
    smp_domains_vec <- as.factor(smp_data[, smp_domains])
    smp_domains_vec <- droplevels(smp_domains_vec)
    rs <- levels(smp_domains_vec)
    # Number of domains in the sample
    N_dom_smp <- length(unique(smp_domains_vec))
    # Number of households in sample per domain
    smp_domains_vec_tmp <- as.numeric(smp_domains_vec)
    n_smp <- as.vector(table(smp_domains_vec_tmp))
  }
  
  
  if (isTRUE(na.rm)) {
    indices <- !is.na(y)
    y_vec <- y_vec[indices]
    if (!is.null(weights)) 
      weights_vec <- weights_vec[indices]
    if (!is.null(sort)) 
      sort <- sort[indices]
  } else if (any(is.na(y))){
    return(NA)
  } 
  
  
  if(is.null(pov_line)){
    if(is.null(weights)){
      pov_line <- 0.6 * median(y_vec)
    } else if (!is.null(weights)){
      pov_line <- 0.6 * Quant_value(y = y_vec, 
                                   weights = weights_vec,
                                   pov_line = NULL,
                                   prob = .5,
                                   na.rm = na.rm)
    }
    
  }
  
  
  indicator_names <- c("Mean",
                       "Head_Count",
                       "Poverty_Gap",
                       "Quintile_Share",
                       "Gini",
                       "Quantile_10",
                       "Quantile_25",
                       "Median",
                       "Quantile_75",
                       "Quantile_90"
  )
  
  return(list(smp_data         = smp_data,
              y                = y, 
              y_vec            = y_vec,
              weights          = weights,
              weights_vec      = weights_vec,
              smp_domains_vec  = smp_domains_vec,
              smp_domains      = smp_domains,
              byDomain         = byDomain,
              rs               = rs,
              N_smp            = N_smp,
              N_dom_smp        = N_dom_smp,
              n_smp            = n_smp,
              #indicator_list   = indicator_list,
              indicator_names  = indicator_names,
              pov_line         = pov_line 
  )
  )
  
}