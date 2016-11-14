
# Variance function for direct indicators
# Difference between indicators just in selection of getFun
direct_variance <- function (y, 
                             weights = NULL, 
                             years = NULL, 
                             smp_domains = NULL, 
                             design = NULL, 
                             smp_data = NULL, 
                             indicator, 
                             R = 100, 
                             bootType = c("calibrate", "naive"), 
                             X, 
                             totals = NULL, 
                             ciType = c("perc","norm", "basic"), 
                             alpha = 0.05, 
                             seed = NULL, 
                             na.rm = FALSE, ...) 
{
  haveWeights <- !is.null(weights)
  rs <- indicator$strata
  byStratum <- !is.null(rs)
  if (byStratum && is.null(smp_domains)) 
    stop("'smp_domains' must be supplied")
  haveDesign <- !is.null(design)

  y <- smp_data[, y]
  y <- as.numeric(as.integer(y))
  if (!is.null(weights)) 
    weights <- smp_data[, weights]

  if (byStratum) 
    smp_domains <- smp_data[, smp_domains]
  if (haveDesign) 
    design <- smp_data[, design]

  if (!is.numeric(y)) 
    stop("'y' must be a numeric vector")
  n <- length(y)
  if (haveWeights && !is.numeric(weights)) {
    stop("'weights' must be a numeric vector")
  }

  if (byStratum && !is.vector(smp_domains) && !is.factor(smp_domains)) {
    stop("'smp_domains' must be a vector or factor")
  }

  if (!haveDesign) 
    design <- rep.int(1, n)
  if (!is.numeric(R) || length(R) == 0) 
    stop("'R' must be numeric")
  else R <- as.integer(R[1])
  if (!is.numeric(alpha) || length(alpha) == 0) 
    stop("'alpha' must be numeric")
  else alpha <- alpha[1]
  bootType <- match.arg(bootType)
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
  }
  else {
    X <- NULL
    totals <- NULL
  }
  ciType <- match.arg(ciType)
  smp_data <- data.frame(y = y)
  smp_data$weight <- weights
  smp_data$stratum <- smp_domains

  pov_line <- indicator$threshold
  if (!is.null(seed)) 
    set.seed(seed)
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
    runif(1)
  seed <- .Random.seed
  
  
  # Choose indicator
  if(any(class(indicator)=="HCR")){
    fun <- getFun_HCR(indicator, byStratum)
  } 
  if(any(class(indicator)=="Gini")){
    fun <- getFun_Gini(indicator, byStratum)
  }
  
  bootFun <- getBootFun(calibrate, fun)

  b <- clusterBoot(smp_data, 
                   bootFun, 
                   R, 
                   strata = design, 
                   #cluster = cluster, 
                   pov_line = pov_line, 
                   aux = X, 
                   totals = totals, 
                   rs = rs, 
                   na.rm = na.rm, 
                   ...)
  if (byStratum) {
    var <- apply(b$t, 2, var)
    ci <- lapply(1:length(b$t0), function(i) {
      ci <- boot.ci(b, conf = 1 - alpha, type = ciType, 
                    index = i)
      switch(ciType, perc = ci$percent[4:5], norm = ci$normal[2:3], 
             basic = ci$basic[4:5], stud = ci$student[4:5], 
             bca = ci$bca[4:5])
    })
    ci <- do.call(rbind, ci)
    colnames(ci) <- c("lower", "upper")

    varByStratum <- data.frame(stratum = rs, var = var[-1])
    var <- var[1]
    ciByStratum <- data.frame(stratum = rs, ci[-1, 
                                               , drop = FALSE])
    ci <- ci[1, ]
  } else {
    var <- var(b$t[, 1])
    ci <- boot.ci(b, conf = 1 - alpha, type = ciType)
    ci <- switch(ciType, perc = ci$percent[4:5], 
                 norm = ci$normal[2:3], basic = ci$basic[4:5], 
                 stud = ci$student[4:5], bca = ci$bca[4:5])
    names(ci) <- c("lower", "upper")
  }
  indicator$varMethod <- "bootstrap"
  indicator$var <- var
  indicator$ci <- ci
  if (byStratum) {
    indicator$varByStratum <- varByStratum
    indicator$ciByStratum <- ciByStratum
  }
  indicator$alpha <- alpha
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

