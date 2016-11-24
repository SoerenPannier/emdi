
# Variance function for direct indicators
# Difference between indicators just in selection of getFun
direct_variance <- function (y, 
                             weights = NULL, 
                             smp_domains = NULL, 
                             design = NULL, 
                             smp_data = NULL, 
                             indicator, 
                             B = 100, 
                             bootType = c("calibrate", "naive"), 
                             X, 
                             totals = NULL, 
                             seed = NULL, 
                             na.rm = FALSE, ...) {
  
  # Domain setup - domains and if variance is calculated by domain
  rs <- indicator$strata
  byStratum <- !is.null(rs)
  
  
  # Specification for y, is different to the calculation of value
  y <- smp_data[, y]
  y <- as.numeric(as.integer(y))
  n <- length(y)
  
  # needs to be specified here again due to the different specification of y and
  # thus n
  haveWeights <- !is.null(weights)
  if (!is.null(weights)) 
    weights <- smp_data[, weights]
  if(is.null(weights)){
    weights <- rep.int(1, n)
  }
  
  # not sure how to use design but I also didnt know if we want to delete it
  haveDesign <- !is.null(design)
  if (haveDesign) {
    design <- smp_data[, design]
  }
  if (!haveDesign) {
    design <- rep.int(1, n)
  } 

  
  # error if number of iterations is not numeric
  if (!is.numeric(B) || length(B) == 0) {
    stop("'B' must be numeric")
  } else {
    B <- as.integer(B[1])
  }

  bootType <- match.arg(bootType)
  
  # if calibrate bootstrap is selected
  calibrate <- haveWeights && bootType == "calibrate"
  if (calibrate) {
    X <- as.matrix(X)
    if (!is.numeric(X)) 
      stop("'X' must be a numeric matrix")
    if (is.null(totals)) {
      totals <- apply(X, 2, function(i) sum(i * weights))
    }
    if (!is.numeric(totals)) 
      stop("'totals' must be of type numeric")
  } else {
    X <- NULL
    totals <- NULL
  }
  
  # Define part of data set that is used in the functions for calibration
  smp_data <- data.frame(y = y)
  smp_data$weight <- weights
  smp_data$stratum <- smp_domains
  
  # definition of poverty line for HCR and PG, is only calculated once and not
  # for every bootstrap
  pov_line <- indicator$pov_line
  
  # set seed for bootstrap
  if (!is.null(seed)) {
    set.seed(seed)
  } 
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
    runif(1)
  seed <- .Random.seed
  
  
  # Choose indicator
  if(class(indicator)=="HCR") {
    fun <- getFun_HCR(indicator, byStratum)
  }
  if(class(indicator)=="PG") {
    fun <- getFun_PG(indicator, byStratum)
  }
  if(class(indicator)=="Gini") {
    fun <- getFun_Gini(indicator, byStratum)
  }
  if(class(indicator)=="QSR"){
    fun <- getFun_QSR(indicator, byStratum)
  }
  bootFun <- getBootFun(calibrate, fun)
  
  # actual bootstrap
  b <- clusterBoot(smp_data, 
                   bootFun, 
                   B, 
                   strata = design, 
                   #cluster = cluster, 
                   pov_line = pov_line, 
                   aux = X, 
                   totals = totals, 
                   rs = rs, 
                   na.rm = na.rm, 
                   ...)
  
  # if variance is calculated by domain
  if (byStratum) {
    var <- apply(b$t, 2, var)
    varByStratum <- data.frame(stratum = rs, var = var[-1])
    var <- var[1]
  } else {
    var <- var(b$t[, 1])
  }
  
  # preparation of return
  indicator$varMethod <- "bootstrap"
  indicator$var <- var
  if (byStratum) {
    indicator$varByStratum <- varByStratum
  }
  indicator$seed <- seed
  return(indicator)
}


# Function in order to select between naive and calibrate bootstrap
getBootFun  <- function(calibrate, fun) {
  if (calibrate) {
    function(x, i, pov_line, aux, totals, rs, na.rm, ...) {
      x <- x[i, , drop = FALSE]
      aux <- aux[i, , drop = FALSE]
      g <- calibWeights(aux, x$weight, totals, ...)
      x$weight <- g * x$weight
      fun(x, pov_line, rs, na.rm)
    }
  } else {
    function(x, i, pov_line, aux, totals, rs, na.rm, ...) {
      x <- x[i, , drop = FALSE]
      fun(x, pov_line, rs, na.rm)
    }
  }
}


# Wrapper function for bootstrap function
clusterBoot <- function(data, statistic, ..., strata, cluster = NULL){
  if (is.null(cluster)) {
    boot(data, statistic, ..., strata = strata)
  } else {
    fun <- function(cluster, i, ..., .data, .statistic) {
      i <- do.call(c, split(1:nrow(.data), .data$cluster)[i])
      .statistic(.data, i, ...)
    }
    keep <- !duplicated(cluster)
    boot(cluster[keep], fun, ..., strata = strata[keep], 
         .data = data, .statistic = statistic)
  }
}

