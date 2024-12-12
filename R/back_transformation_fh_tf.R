
log_bt_fh_tf <- function(data, eblup, subdomains, varu, varv){
  #point_backtransformed <- exp(eblup)
  bt_in_smp <- data[data$ObsSub == "yes", ]
  # exp(eblup_transformed_scale + 0.5 * g1)
  g1_in <- (1 - bt_in_smp$gamma_ij)^2 * (varu + (1 - bt_in_smp$gamma_i) * varv) +
    bt_in_smp$gamma_ij^2 * bt_in_smp$vare
  bt_in_smp$bt_mu_ij <- exp(bt_in_smp[[eblup]] + 0.5 *  g1_in)

  bt_out_smp <- data[data$ObsSub == "no", ]
  if(dim(bt_out_smp)[1] != 0){
    g1_out <- varu * (1 + (bt_out_smp$gamma_i/bt_out_smp$gamma_i0) *
                        (1 - 2 * bt_out_smp$gamma_ij))
    bt_out_smp$bt_mu_ij <- exp(bt_out_smp[[eblup]] + 0.5 * g1_out)

    # naive back transformation for the out of sample grids in out of sample domains
    bt_out_smp$bt_mu_ij <- ifelse(bt_out_smp$gamma_ij == 0 &
                                    bt_out_smp$gamma_i0 == 0,
                                  exp(bt_out_smp[[eblup]]), bt_out_smp$bt_mu_ij)

    bt_eblup <- rbind(bt_in_smp[, c("bt_mu_ij", subdomains)],
                      bt_out_smp[, c("bt_mu_ij", subdomains)])
  }else if(dim(bt_out_smp)[1] == 0){
    bt_eblup <- rbind(bt_in_smp[, c("bt_mu_ij", subdomains)])
  }

  return(bt_eblup)
}

arcsin_bt_fh_tf <- function(data, eblup, subdomains, varu, varv) {
  bt_in_smp <- data[data$ObsSub == "yes", ]

  for (k in 1:length(bt_in_smp$mu_ij)) {
    g1_in <- (1 - bt_in_smp$gamma_ij)^2 * (varu + (1 - bt_in_smp$gamma_i) * varv) +
      bt_in_smp$gamma_ij^2 * bt_in_smp$vare
    bt_in_smp$bt_mu_ij[k] <- integrate(integrand, lower = 0, upper = pi / 2,
                                       mean = bt_in_smp$mu_ij[k],
                                       sd = sqrt(g1_in[k]))$value
  }
  bt_out_smp <- data[data$ObsSub == "no", ]
  if(dim(bt_out_smp)[1] != 0){
    for (k in 1:length(bt_out_smp$mu_ij)) {
      g1_out <- varu * (1 + (bt_out_smp$gamma_i/bt_out_smp$gamma_i0) *
                          (1 - 2 * bt_out_smp$gamma_ij))
      #-------------------------------------------------------------------------------
      # Naive back transformation for out of sample grids in out of sample domains
      if(bt_out_smp$gamma_ij[k] == 0 & bt_out_smp$gamma_i0[k] == 0){
        if(bt_out_smp$mu_ij[k] < 0) {bt_out_smp$bt_mu_ij[k] <- (sin(0))^2}
        else if(bt_out_smp$mu_ij[k] > pi/2) {bt_out_smp$bt_mu_ij[k] <- (sin(pi/2))^2}
        else if(bt_out_smp$mu_ij[k] < pi/2 & bt_out_smp$mu_ij[k] > 0) {bt_out_smp$bt_mu_ij[k] <- (sin(bt_out_smp$mu_ij[k]))^2}
      } else {
        # bias corrected back transformation for out of sample grids but in sample domains
        bt_out_smp$bt_mu_ij[k] <- integrate(integrand,
                                            lower = 0,
                                            upper = pi / 2,
                                            bt_out_smp$mu_ij[k],
                                            sqrt(g1_out[k]))$value
      }
      #-------------------------------------------------------------------------------
    }
    bt_eblup <- rbind(bt_in_smp[, c("bt_mu_ij", subdomains)],
                      bt_out_smp[, c("bt_mu_ij", subdomains)])
  } else if(dim(bt_out_smp)[1] == 0){
    bt_eblup <- rbind(bt_in_smp[, c("bt_mu_ij", subdomains)])
  }


  return(bt_eblup)
}
