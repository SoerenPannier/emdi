

eusilcA_smp2 <- eusilcA_smp
#eusilcA_smp2$district <- droplevels(eusilcA_smp2$district)
emdi_direct <- direct(y="eqIncome", 
                      smp_data = eusilcA_smp2, 
                      smp_domains = "district", 
                      weights = NULL, 
                      threshold = 10859.24, 
                      var = T, 
                      boot_type = "naive", 
                      B = 5, 
                      seed=5, 
                      X_calib = NULL, 
                      totals = NULL, 
                      custom_indicator = #
                        list( my_max = 
                               function(y, weights, threshold){
                                        max(y)
                                 }, 
                               my_min = 
                                function(y, weights, threshold){
                                 # if(!any(is.finite(y)))
                                  #  browser()
                                  min(y)
                                  }), 
                      na.rm = TRUE)

emdi_direct$ind$Domain
emdi_direct$ind$my_max

miss <- emdi_direct$MSE$Domain[
  is.na(emdi_direct$MSE$my_max)]
table(eusilcA_smp2$district)[miss]
sum(as.numeric(table(eusilcA_smp2$district))<=1)





