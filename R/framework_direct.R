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
  
  indicator_list <- getIndicatorList()
 
  indicator_names <- c("Mean",
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
              indicator_list   = indicator_list,
              indicator_names  = indicator_names,
              pov_line         = pov_line 
  )
  )
}

getIndicatorList <- function(){
  list(
    mean_wrap = function(y, 
                          weights, 
                          pov_line){
      weighted.mean(x = y, w = weights)
    },
    hcr_wrap = function(y, 
                        weights, 
                        pov_line){
      sw <- sum(weights)
      sum(weights[y < pov_line]) / sw 
    },
    pgap_wrap =  function(y, 
                          weights, 
                          pov_line){
      
      sw <- sum(weights)
      sum(weights  * (y < pov_line) * (pov_line - y) / pov_line) / sw 
    },
    gini_wrap = function (y, 
                          weights = NULL, 
                          pov_line = NULL) {
      ord <- order(y)
      y <- y[ord]
      if (!is.null(weights)){
        weights <- weights[ord]
      }
      wy <- weights * y
      sw <- sum(weights)
      cw <- cumsum(weights)
      ((2 * sum(wy * cw) - sum(weights^2 * y))/(sw * sum(wy)) - 1)
    }
    ,
    qsr_wrap = function (y, 
                         weights, 
                         pov_line){
      ord <- order(y)
      y <- y[ord]
      weights <- weights[ord]
      quant14 <- wtd.quantile(x = y, weights = weights, 
                              probs = c(.2, .8))
      iq1 <- y <= quant14[1]
      iq4 <- y > quant14[2]
      (sum(weights[iq4] * y[iq4]) / 
          sum(weights[iq4])) / (sum(weights[iq1] *  y[iq1])/sum(weights[iq1]))
    },
    quant10_wrap = function(y, 
                             weights, 
                             pov_line){
      wtd.quantile(x = y, weights = weights, 
                   probs = .10)
    },
    quant25_wrap = function(y, 
                             weights, 
                             pov_line){
      wtd.quantile(x = y, weights = weights, 
                   probs = .25)
    },
    quant50_wrap = function(y, 
                             weights, 
                             pov_line){
      wtd.quantile(x = y, weights = weights, 
                   probs = .50)
    },
    quant75_wrap = function(y, 
                             weights, 
                             pov_line){
      wtd.quantile(x = y, weights = weights, 
                   probs = .75)
    },
    quant90_wrap = function(y, 
                             weights, 
                             pov_line){
      wtd.quantile(x = y, weights = weights, 
                   probs = .9)
    }
  )
}
