point_estim_direct <-  function(direct_estimator,
                                indicator_name,
                                framework,  
                                boot_type, 
                                B,
                                seed,
                                X_calib, 
                                totals){
  # whole sample value
  if(inherits(framework$threshold, "function")){
    framework$threshold <- framework$threshold(framework$y_vec, framework$weights_vec)
  }
  value <- direct_estimator(y = framework$y_vec, 
                            weights = framework$weights_vec, 
                            threshold = framework$threshold)
    valueByDomain <- 
      aggregate(1:framework$N_smp, 
                list(Domain = framework$smp_domains_vec), 
                function(i, y, threshold, weights) {
                  direct_estimator(y = y[i], 
                                   weights = weights[i], 
                                   threshold = threshold)
                },     
                y = framework$y_vec, 
                weights     = framework$weights_vec, 
                threshold    = framework$threshold)
    names(valueByDomain)[2] <- indicator_name
    names(valueByDomain)[1] <- "Domain"
    rs <- as.factor(as.matrix(valueByDomain[1])[,1])
    
    return(list(value = value, 
                valueByDomain = valueByDomain, 
                domain = rs))
}