mse_fh_tf <- function(fixed, vardir, domains, subdomains, nunits, trafo,
                      eff_n, B, smp_hat_varu, smp_hat_varv, smp_hat_vare,
                      smp_beta, seed, maxit, framework){

  message("\r", "Bootstrap started")
  start_time <- Sys.time()

  # Data frame for bias
  bias_subarea <- data.frame(SubArea = unique(framework$data[[subdomains]]))
  bias_area <- data.frame(Area = unique(framework$data[[domains]]))
  b <- 1
  while(b <= B) {
    #print(paste0("b = ", b))
    # Generate pseudo errors and pseudo true indicator (in transformed scale)
    current.na.action <- options('na.action')
    options(na.action = 'na.pass')
    X_pop <- model.matrix(object = fixed, data = framework$data)
    rownames(X_pop) <- framework$data[[subdomains]]
    options('na.action' = current.na.action$na.action)
    if(!is.na(eff_n)){
      new_data <- framework$data[, c(subdomains, domains, vardir, nunits, eff_n,
                                     get.vars(fixed, data = framework$data))]
    } else if(is.na(eff_n)){new_data <- framework$data[, c(subdomains, domains,
                                                           vardir, nunits,
                                                           get.vars(fixed, data = framework$data))]}


    xbeta <- X_pop %*% smp_beta

    dat_xbeta <- data.frame(Subarea = rownames(xbeta), xbeta = xbeta)
    new_data <- merge(new_data, dat_xbeta, by.x = subdomains, by.y = "Subarea", all.x = TRUE,
                      all.y = FALSE)
    new_data$pseudo_u <- rnorm(n = length(unique(new_data[[subdomains]])),
                               mean = 0, sd = sqrt(smp_hat_varu))

    pseudo_v <- data.frame(Area = unique(framework$data[[domains]]),
                           pseudo_v = rnorm(n = length(unique(new_data[[domains]])),
                                            mean = 0, sd = sqrt(smp_hat_varv)))
    new_data <- merge(new_data, pseudo_v, by.x = domains, by.y = "Area",
                      all.x = TRUE, all.y = FALSE)
    new_data <- merge(new_data, smp_hat_vare[, c(subdomains, "vare")], by = subdomains, all.x = TRUE,
                      all.y = FALSE)
    new_data$boot_true_ind_star <- new_data$xbeta + new_data$pseudo_v + new_data$pseudo_u
    for (i in 1:dim(new_data)[1]) {
      if(is.na(new_data$vare[i])){
        new_data$pseudo_e[i] <- NA
        new_data$boot_dir_ind_star[i] <- NA
      } else{
        new_data$pseudo_e[i] <- rnorm(1, mean = 0, sd = sqrt(new_data$vare[i]))
        new_data$boot_dir_ind_star[i] <- new_data$boot_true_ind_star[i] + new_data$pseudo_e[i]
      }
    }
    # Get true indicator in original scale
    if(trafo == "log"){
      new_data$boot_true_ind <- exp(new_data$boot_true_ind_star)
      new_data$boot_dir_ind <- exp(new_data$boot_dir_ind_star)
      ##########################################################################
      # Adjust boot_var_dir-----------------------------------------------------
      new_data$boot_var_dir <- new_data$boot_dir_ind^2 * new_data$vare
      boot_vardir <- "boot_var_dir"
      ##########################################################################

    } else if(trafo == "arcsin"){
      # Why do we need to truncate? -> because of monotonity
      new_data$boot_true_ind_star <- ifelse(new_data$boot_true_ind_star < 0,
                                            0, new_data$boot_true_ind_star)

      new_data$boot_true_ind_star <- ifelse(new_data$boot_true_ind_star > (pi / 2),
                                            (pi / 2), new_data$boot_true_ind_star)

      new_data$boot_true_ind <- (sin(new_data$boot_true_ind_star))^2
      new_data$boot_dir_ind_star <- ifelse(new_data$boot_dir_ind_star < 0,
                                           0, new_data$boot_dir_ind_star)

      new_data$boot_dir_ind_star <- ifelse(new_data$boot_dir_ind_star > (pi / 2),
                                           (pi / 2), new_data$boot_dir_ind_star)

      new_data$boot_dir_ind <- (sin(new_data$boot_dir_ind_star))^2
    } else if(trafo == "no"){
      new_data$boot_true_ind <- new_data$boot_true_ind_star
      new_data$boot_dir_ind <- new_data$boot_dir_ind_star
    }
    # Get true indicator at area level
    DomName <- unique(new_data[[domains]])
    new_data_area <- data.frame(Area = DomName)
    for (m in 1:length(DomName)) {
      dat <- new_data[new_data[[domains]] == DomName[m], c("boot_true_ind", nunits)]
      N <- sum(dat[[nunits]])
      new_data_area$boot_true_ind[m] <- sum(dat[["boot_true_ind"]] * dat[[nunits]])/N
    }
    # Estimate boot TF indicator
    boot_fixed <- update(fixed, "boot_dir_ind ~ .")
    boot_point_tf <- NULL

    if (trafo == "log"){
      fh_tf_fw_boot <- framework_FH_tf(data = new_data,
                                       fixed = boot_fixed, vardir = boot_vardir,
                                       domains = domains, subdomains = subdomains,
                                       trafo = trafo, eff_n = eff_n,
                                       nunits = nunits)
      try(boot_point_tf <- point_fh_tf(fixed = boot_fixed, vardir = boot_vardir,
                                       domains = domains, subdomains = subdomains,
                                       nunits = nunits, trafo = trafo, maxit = maxit,
                                       eff_n = eff_n, framework = fh_tf_fw_boot), silent = TRUE)
    } else if(trafo == "arcsin" | trafo == "no"){
      fh_tf_fw_boot <- framework_FH_tf(data = new_data,
                                       fixed = boot_fixed, vardir = vardir,
                                       domains = domains, subdomains = subdomains,
                                       trafo = trafo, eff_n = eff_n,
                                       nunits = nunits)
      try(boot_point_tf <- point_fh_tf(fixed = boot_fixed, vardir = vardir,
                                       domains = domains, subdomains = subdomains,
                                       nunits = nunits, trafo = trafo, maxit = maxit,
                                       eff_n = eff_n, framework = fh_tf_fw_boot), silent = TRUE)
    }

    if(!is.null(boot_point_tf)){
      point_subarea <- merge(new_data[, c(subdomains, "boot_true_ind")],
                             boot_point_tf$EBLUP_SubArea,
                             by.x = subdomains, by.y = "Subdomain", all.x = TRUE, all.y = FALSE)
      point_area <- merge(new_data_area, boot_point_tf$EBLUP_Area,
                          by.x = "Area", by.y = "Domain", all.x = TRUE, all.y = FALSE)


      if(trafo == "log" | trafo == "arcsin"){
        point_subarea$bias <- point_subarea$EBLUP - point_subarea$boot_true_ind
        point_area$bias <- point_area$EBLUP - point_area$boot_true_ind

        bias_subarea <- merge(bias_subarea, point_subarea[, c(subdomains, "bias")],
                              by.x = "SubArea", by.y = subdomains, all.x = TRUE, all.y = FALSE)
        colnames(bias_subarea)[b + 1] <- paste0("b", b)

        bias_area <- merge(bias_area, point_area[, c("Area", "bias")],
                           by = "Area", all.x = TRUE, all.y = FALSE)
        colnames(bias_area)[b + 1] <- paste0("b", b)

      } else if(trafo == "no"){
        point_subarea$bias <- point_subarea$EBLUP - point_subarea$boot_true_ind
        point_area$bias <- point_area$EBLUP - point_area$boot_true_ind
        bias_subarea <- merge(bias_subarea, point_subarea[, c(subdomains, "bias")],
                              by.x = "SubArea", by.y = subdomains, all.x = TRUE, all.y = FALSE)
        colnames(bias_subarea)[b + 1] <- paste0("b", b)

        bias_area <- merge(bias_area, point_area[, c("Area", "bias")],
                           by = "Area",  all.x = TRUE, all.y = FALSE)
        colnames(bias_area)[b + 1] <- paste0("b", b)
      }
      b <- b + 1
    }
    if (b %% 10 == 0) {
      if (b != B) {
        delta <- difftime(Sys.time(), start_time, units = "secs")
        remaining <- (delta / b) * (B - b)
        remaining <- unclass(remaining)
        remaining <- sprintf(
          "%02d:%02d:%02d:%02d",
          remaining %/% 86400, # days
          remaining %% 86400 %/% 3600, # hours
          remaining %% 3600 %/% 60, # minutes
          remaining %% 60 %/% 1
        ) # seconds)

        message("\r", b, " of ", B, " Bootstrap iterations completed \t
              Approximately ", remaining, " remaining \n")
        if (.Platform$OS.type == "windows") flush.console()
      }
    }
  }
  message("\r", "Bootstrap completed", "\n")
  if (.Platform$OS.type == "windows") {
    flush.console()
  }

  if(trafo == "no"){
    subarea_quality <- data.frame(Subdomain = bias_subarea$SubArea,
                                  MSE = rowMeans((bias_subarea[, 2:(B+1)])^2))
    area_quality <- data.frame(Domain = bias_area$Area,
                               MSE = rowMeans((bias_area[, 2:(B+1)])^2))
  } else if(trafo == "log" | trafo == "arcsin"){
    subarea_quality <- data.frame(Subdomain = bias_subarea$SubArea,
                                  MSE = rowMeans((bias_subarea[, 2:(B+1)])^2))
    area_quality <- data.frame(Domain = bias_area$Area,
                               MSE = rowMeans((bias_area[, 2:(B+1)])^2))
  }

  return(list(MSE_Area = area_quality,
              MSE_SubArea = subarea_quality))

}