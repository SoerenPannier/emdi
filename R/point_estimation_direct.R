point_estim_direct <-  function(direct_estimator,
                                indicator_name,
                                framework,  
                                bootType, 
                                B,
                                seed,
                                X_calib, 
                                totals){
  # whole sample value

  value <- direct_estimator(y = framework$y_vec, 
                            weights = framework$weights_vec, 
                            pov_line = framework$pov_line)
    valueByDomain <- 
      aggregate(1:framework$N_smp, 
                list(Domain = framework$smp_domains_vec), 
                function(i, y, pov_line, weights) {
                  direct_estimator(y = y[i], 
                                   weights = weights[i], 
                                   pov_line = pov_line)
                },     
                y = framework$y_vec, 
                weights     = framework$weights_vec, 
                pov_line    = framework$pov_line)
    names(valueByDomain)[2] <- indicator_name
    names(valueByDomain)[1] <- "Domain"
    rs <- as.factor(as.matrix(valueByDomain[1])[,1])
    
    return(list(value = value, 
                valueByDomain = valueByDomain, 
                domain = rs))
}