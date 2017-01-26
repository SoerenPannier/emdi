direct_variance2 <- function( direct_estimator,
                              indicator_name,
                              y,
                              weights,
                              smp_data,
                              smp_domains,
                              design,
                              indicator, 
                              bootType, 
                              B = B,
                              seed,
                              X_calib, 
                              totals,
                              pov_line){

  # Domain setup - domains and if variance is calculated by domain
  rs <- indicator$domain
  byDomain <- !is.null(rs)
  
  
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
  
  bootType #<- match.arg(bootType)
  
  # if calibrate bootstrap is selected
  calibrate <- haveWeights && bootType == "calibrate"
  if (calibrate) {
    X_calib <- as.matrix(X_calib)
    if (!is.numeric(X_calib)) 
      stop("'X_calib' must be a numeric matrix")
    if (is.null(totals)) {
      totals <- apply(X_calib, 2, function(i) sum(i * weights))
    }
    if (!is.numeric(totals)) 
      stop("'totals' must be of type numeric")
  } else {
    X_calib <- NULL
    totals <- NULL
  }
  
  # Define part of data set that is used in the functions for calibration
  smp_data <- data.frame(y = y)
  smp_data$weight <- weights
  smp_data$Domain <- smp_domains
  
  # set seed for bootstrap
  if (!is.null(seed)) {
    set.seed(seed)
  } 
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
    runif(1)
  seed <- get(".Random.seed")
  
  fun <- getFun2(byDomain, direct_estimator)
  bootFun <- getBootFun2(calibrate, fun)
  # actual bootstrap
  b <- clusterBoot2( smp_data, 
                     bootFun, 
                     B, 
                     domain = design, 
                     #cluster = cluster, 
                     pov_line = pov_line, 
                     aux = X_calib, 
                     totals = totals, 
                     rs = rs)
                     #, ...)
   # browser()
  # if variance is calculated by domain
  if (byDomain) {
    var <- apply(b$t, 2, var)
    varByDomain <- data.frame(Domain = rs, var = var[-1])
    var <- var[1]
  } else {
    var <- var(b$t[, 1])
  }
  
  # preparation of return
  indicator$varMethod <- "bootstrap"
  indicator$var <- var
  if (byDomain) {
    indicator$varByDomain <- varByDomain
  }
  indicator$seed <- seed
  return(indicator)
}


getFun2 <- function(byDomain, direct_estimator){
  if(byDomain)
  {
    function(x, pov_line, rs) {
      value <- direct_estimator(x$y, x$weight, pov_line)
      valueByDomain <- sapply(rs, function(r, x, t) {
        i <- x$Domain == r
        direct_estimator(x$y[i], x$weight[i],  pov_line)
      }, x = x)
      c(value, valueByDomain)
    }
  }
  else{
    function(x, pov_line, rs, na.rm) {
      direct_estimator(x$y, x$weight, pov_line)
    }
  }
}

# Function in order to select between naive and calibrate bootstrap
getBootFun2  <- function(calibrate, fun) {
  if (calibrate) {
    function(x, i, pov_line, aux, totals, rs, ...) {
      x <- x[i, , drop = FALSE]
      aux <- aux[i, , drop = FALSE]
      g <- calibWeights(aux, x$weight, totals, ...)
      x$weight <- g * x$weight
      fun(x, pov_line, rs)
    }
  } else {
    function(x, i, pov_line, aux, totals, rs, ...) {
      x <- x[i, , drop = FALSE]
      fun(x, pov_line, rs)
    }
  }
}

# Wrapper function for bootstrap function
clusterBoot2 <- function(data, statistic, ..., domain, pov_line, cluster = NULL){
  if (is.null(cluster)) {
    boot(data, statistic,  pov_line = pov_line, ..., domain = domain)
  } else {
    fun <- function(cluster, i, ..., .data, .statistic) {
      i <- do.call(c, split(1:nrow(.data), .data$cluster)[i])
      .statistic(.data, i, ...)
    }
    keep <- !duplicated(cluster)
    boot(cluster[keep], fun, ..., domain = domain[keep], pov_line = pov_line,
         .data = data, .statistic = statistic)
  }
}


calibWeights <- function (X_calib, 
                          d, 
                          totals, 
                          q = NULL, 
                          method = c("raking", "linear", "logit"), 
                          bounds = c(0, 10), 
                          maxit = 500, 
                          tol = 1e-06, 
                          eps = .Machine$double.eps) 
{
  X_calib <- as.matrix(X_calib)
  d <- as.numeric(d)
  totals <- as.numeric(totals)
  haveNA <- c(any(is.na(X_calib)), any(is.na(d)), any(is.na(totals)), 
              !is.null(q) && any(is.na(q)))
  if (any(haveNA)) {
    argsNA <- c("'X_calib'", "'d'", "'totals'", "'q'")[haveNA]
    stop("missing values in the following arguments", paste(argsNA, 
                                                            collapse = ", "))
  }
  n <- nrow(X_calib)
  if (length(d) != n) 
    stop("length of 'd' not equal to number of rows in 'X_calib'")
  p <- ncol(X_calib)
  if (length(totals) != p) {
    stop("length of 'totals' not equal to number of columns in 'X_calib'")
  }
  if (is.null(q)) 
    q <- rep.int(1, n)
  else {
    q <- as.numeric(q)
    if (length(q) != n) {
      stop("length of 'q' not equal to number of rows in 'X_calib'")
    }
    if (any(is.infinite(q))) 
      stop("infinite values in 'q'")
  }
  method <- match.arg(method)
  if (method == "linear") {
    lambda <- ginv(t(X_calib * d * q) %*% X_calib, tol = eps) %*% (totals - 
                                                         as.vector(t(d) %*% X_calib))
    g <- 1 + q * as.vector(X_calib %*% lambda)
  }
  else {
    lambda <- matrix(0, nrow = p)
    tolNotReached <- function(X_calib, w, totals, tol) {
      max(abs(crossprod(X_calib, w) - totals)/totals) >= tol
    }
    if (method == "raking") {
      g <- rep.int(1, n)
      w <- d
      i <- 1
      while (!any(is.na(g)) && tolNotReached(X_calib, w, totals, 
                                             tol) && i <= maxit) {
        phi <- t(X_calib) %*% w - totals
        T <- t(X_calib * w)
        dphi <- T %*% X_calib
        lambda <- lambda - ginv(dphi, tol = eps) %*% 
          phi
        g <- exp(as.vector(X_calib %*% lambda) * q)
        w <- g * d
        i <- i + 1
      }
      if (any(is.na(g)) || i > maxit) {
        warning("no convergence")
        g <- NULL
      }
    }
    else {
      if (length(bounds) < 2) 
        stop("'bounds' must be a vector of length 2")
      else bounds <- bounds[1:2]
      if (bounds[1] >= 1) 
        stop("the lower bound must be smaller than 1")
      if (bounds[2] <= 1) 
        stop("the lower bound must be larger than 1")
      A <- diff(bounds)/((1 - bounds[1]) * (bounds[2] - 
                                              1))
      getG <- function(u, bounds) {
        (bounds[1] * (bounds[2] - 1) + bounds[2] * (1 - 
                                                      bounds[1]) * u)/(bounds[2] - 1 + (1 - bounds[1]) * 
                                                                         u)
      }
      g <- getG(rep.int(1, n), bounds)
      X1 <- X_calib
      d1 <- d
      totals1 <- totals
      q1 <- q
      g1 <- g
      indices <- 1:n
      anyOutOfBounds <- function(g, bounds) {
        any(g < bounds[1]) || any(g > bounds[2])
      }
      i <- 1
      while (!any(is.na(g)) && (tolNotReached(X_calib, g * d, 
                                              totals, tol) || anyOutOfBounds(g, bounds)) && 
             i <= maxit) {
        if (anyOutOfBounds(g, bounds)) {
          g[g < bounds[1]] <- bounds[1]
          g[g > bounds[2]] <- bounds[2]
          tmp <- which(g > bounds[1] & g < bounds[2])
          if (length(tmp) > 0) {
            indices <- tmp
            X1 <- X_calib[indices, ]
            d1 <- d[indices]
            if (length(indices) < n) {
              totals1 <- totals - as.vector(t(g[-indices] * 
                                                d[-indices]) %*% X_calib[-indices, , drop = FALSE])
            }
            q1 <- q[indices]
            g1 <- g[indices]
          }
        }
        w1 <- g1 * d1
        phi <- t(X1) %*% w1 - totals1
        T <- t(X1 * w1)
        dphi <- T %*% X1
        lambda <- lambda - ginv(dphi, tol = eps) %*% 
          phi
        u <- exp(A * as.vector(X1 %*% lambda) * q1)
        g1 <- getG(u, bounds)
        g[indices] <- g1
        i <- i + 1
      }
      if (any(is.na(g)) || i > maxit) {
        warning("no convergence")
        g <- NULL
      }
    }
  }
  return(g)
}