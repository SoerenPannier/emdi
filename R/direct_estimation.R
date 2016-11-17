direct <- function(y, 
                   smp_data, 
                   smp_domains = NULL, 
                   weights = NULL, 
                   sort = NULL,
                   pov_line = NULL,
                   var = TRUE, 
                   bootType = "naive", 
                   R = NULL,
                   seed = NULL,
                   X = NULL, 
                   totals = NULL, 
                   na.rm=FALSE){
  
  framework_dir <- function(y, smp_data, smp_domains, weights, sort, 
                            pov_line, na.rm){
    
    # two versions of y, one vector version and one character version (original)
    # for variance estimation
    # we force sample data, in laeken data can be NULL
    y_vec <- smp_data[, y]
    if (!is.null(weights)) {
      weights_vec <- smp_data[, weights]
    } else if (is.null(weights)) {
      weights_vec <- rep.int(1, n)
    }
  
    if (!is.null(sort)) {
      sort <- smp_data[, sort]
    }
    byStratum <- !is.null(smp_domains)
    if (byStratum) {
      smp_domains_vec <- smp_data[, smp_domains]
      smp_domains_vec <- as.factor(smp_data[, smp_domains])
      rs <- levels(smp_domains_vec)
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
      
    #order <- if(is.null(sort)){
    #  order(y_vec)
    #} else {
    #  order(y_vec, sort)
    #}
    
    if(is.null(pov_line)){
      if(is.null(weights)){
        pov_line <- 0.6*median(y_vec)
      } else if (!is.null(weights)){
        pov_line <- 0.6*weightedMedian(y_vec, weights_vec)
      }
       
    }
    
  
    # Number of households in sample
    N_smp <- length(smp_domains_vec)
    # Number of domains in the sample
    N_dom_smp <- length(unique(smp_domains_vec))
    # Number of households in sample per domain
    smp_domains_vec_tmp <- as.numeric(smp_domains_vec)
    n_smp <- as.vector(table(smp_domains_vec_tmp))

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
                byStratum        = byStratum,
                rs               = rs,
                N_smp            = N_smp,
                N_dom_smp        = N_dom_smp,
                n_smp            = n_smp,
                #indicator_list   = indicator_list,
                indicator_names  = indicator_names,
                pov_line         = pov_line 
                #order            = order
    )
    )
    
  }
  
  framework <- framework_dir(y=y, smp_data=smp_data, smp_domains=smp_domains, 
                             weights=weights, sort=sort, pov_line=pov_line, 
                             na.rm=na.rm)
  
  # Call single indicators
  HCR <- Head_Count(framework = framework,
                    var=var,
                    bootType = bootType,
                    X = X, 
                    totals = totals, 
                    R=R,  
                    seed=seed, 
                    na.rm=na.rm)
  
  Gini_coeff <- Gini(framework = framework,
                     sort = sort,
                     var=var,
                     bootType = bootType,
                     X = X, 
                     totals = totals, 
                     R=R,  
                     seed=seed, 
                     na.rm=na.rm)
  
  ind <- data.frame(Domain = framework$rs, Head_Count=HCR$valueByStratum[,2], 
                    Gini = Gini_coeff$valueByStratum[,2])
  
  if(var==TRUE){
    MSE <- data.frame(Domain=framework$rs, Head_Count=HCR$varByStratum[,2], 
                      Gini = Gini_coeff$varByStratum[,2])
    
    direct_out <- list(ind = ind, 
                       MSE = MSE,
                       framework=framework)
  } else {
    direct_out <- list(ind = ind, 
                     MSE = NULL,
                     framework=framework)
  }
  return(direct_out)
} 
  
  





  
