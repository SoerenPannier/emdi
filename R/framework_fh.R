framework_FH <- function(combined_data, fixed, vardir, domains,
                         transformation, eff_smpsize, correlation, corMatrix,
                         Ci, tol, maxit) {
  # Get sample and population data
  obs_dom <- !is.na(combined_data[[paste(lhs(fixed))]])

  data <- combined_data[obs_dom == TRUE,]


  # Get response variable and model matrix from formula and data
  direct <- makeXY(fixed, data)$y
  model_X <- makeXY(fixed, data)$x
  vardir <- data[, vardir]
  direct_orig <- NULL
  vardir_orig <- NULL
  #direct_orig <- direct
  #vardir_orig <- vardir

  if (transformation == "log" | transformation == "log" | transformation == "log") {
    direct_orig <- direct
    vardir_orig <- vardir
    vardir <- (1 / direct)^2 * vardir
    direct <- log(direct)
  } else if (transformation == "arcsin" | transformation == "arcsin") {
    direct_orig <- direct
    vardir_orig <- vardir
    direct <- asin(sqrt(direct))
    vardir <-  1/ (4 * data[, eff_smpsize])
  }

  if (is.null(domains)) {
    data$domains <- 1:length(direct)
    domains <- "domains"
  }
  
  # Number of areas
  m <- length(direct)
  M <- length(combined_data[[paste(lhs(fixed))]])
  # Number of out-of-sample domains
  N_dom_unobs <- M - m
  # Number of covariates
  p <- ncol(model_X)
  
 # if (!is.null(Ci)){
 #   model_X <- as.matrix(makeXY(fixed, data)$x) ### ACHTUNG as.matrix ergänzt
 #   p <- ncol(model_X)
 #   # ??? Added this line, correct?
  #  Ci_tmp <- combined_data[[Ci]]
 #   #Ci: array of Ci matrixes pxpxm
 #   Ci_array <- array(data = 0,dim = c(p, p, M) # Vorsicht, vlt m
 #   )
    
 #   for(i in 1:M){
 #     Ci_array[p,p,i] <- Ci_tmp[i] 
 #   }
 #   Ci <- Ci_array
 # }

  if (!is.null(corMatrix)){
    corMatrix <- corMatrix
  }
  
  if (!is.null(Ci)){
    Ci <- Ci
  }
  
  framework_out <- list(obs_dom = obs_dom,
                        N_dom_smp = m,
                        N_dom_unobs = N_dom_unobs,
                        data = data,
                        formula = fixed,
                        direct = direct,
                        direct_orig = direct_orig,
                        model_X = model_X,
                        vardir = vardir,
                        vardir_orig = vardir_orig,
                        eff_smpsize = eff_smpsize,
                        domains = domains,
                        m = m,
                        M = M,
                        p = p,
                        correlation = correlation,
                        W = corMatrix,
                        Ci = Ci,
                        tol = tol,
                        maxit = maxit)

  return(framework_out)
  }
