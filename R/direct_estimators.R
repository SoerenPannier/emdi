



# Head Count Ratio -------------------------------------------------------------

Head_Count <- function(y, 
                       smp_data, 
                       smp_domains = NULL, 
                       weights = NULL, 
                       pov_line = NULL, 
                       var = TRUE, 
                       bootType = "naive", 
                       R = NULL,
                       seed = NULL,
                       X = NULL, 
                       totals = NULL, 
                       na.rm=FALSE){
  
  # Needs to be character for the variance function
  smp_domains_chr <- smp_domains
  weights_chr <- weights
  
  # logical if domains selected
  byStratum <- !is.null(smp_domains)
  
  # smp_domains needs to be factor for this function
  smp_domains <- as.factor(smp_data[, smp_domains])
  rs <- levels(smp_domains)
  
  # sample size
  n <- length(smp_data[,y])
  #ts <- pov_line
  if (!is.null(weights)){
    weights <- smp_data[, weights]
  } 
  if (is.null(weights)){
    weights <- weights <- rep.int(1, n)
  } 
    
  # Overall HCR 
  value <- Head_Count_value(y = smp_data[,y], 
                            weights = weights, 
                            pov_line = pov_line, 
                            na.rm = na.rm)
  
  # if sample domains are given
  if (byStratum) {
    
    valueByStratum <- aggregate(1:n, list(stratum = smp_domains), 
                                HCR2, y = smp_data[,y], weights = weights, 
                                pov_line = pov_line, na.rm = na.rm)
    names(valueByStratum)[2] <- "value"
  } else {
    valueByStratum <- NULL
  }
  
  res <- list(value = value, 
              valueByStratum = valueByStratum, 
              strata = rs, 
              pov_line = pov_line)
  
  class(res) <- c("HCR")
  
  if (var == TRUE) {
    res <- direct_variance(y=y, 
                        weights=weights_chr, 
                        smp_data=smp_data,  
                        smp_domains = smp_domains_chr, 
                        indicator=res,
                        bootType=bootType, 
                        R = R,
                        seed = seed,
                        X = X, 
                        totals = totals,
                        na.rm=na.rm)
  } 
  return(res)
}


# Calculate weighted HCR - if weights = NULL, unweighted -----------------------
Head_Count_value <- function(y, 
                             weights, 
                             pov_line, 
                             na.rm){
  if (is.null(weights)) {
    weights <- rep.int(1, length(y))
  } 
  if (isTRUE(na.rm)) {
    indices <- !is.na(y)
    y <- y[indices]
    weights <- weights[indices]
  }
  else if (any(is.na(y))) 
    return(NA)
  
  sw <- sum(weights)
  sapply(pov_line, function(t) sum(weights[y < t]))/sw*100
}


# Function to calculate sample HCR ---------------------------------------------
HCR2 <- function(i, 
                 y, 
                 weights, 
                 pov_line = pov_line, 
                 na.rm) {
  
  Head_Count_value(y = y[i], 
                   weights = weights[i], 
                   pov_line = pov_line, 
                   na.rm = na.rm)
}


# Get HCR function for bootstrap variance
getFun_HCR <- function (indicator, byStratum) 
{
  if (byStratum) {
    function(x, pov_line, rs, na.rm) {
      
      #pov_line <- p * weightedMedian(x$y, x$weight)
      value <- Head_Count_value(x$y, x$weight, pov_line, 
                                na.rm = na.rm)
      valueByStratum <- sapply(rs, function(r, x, t) {
        i <- x$stratum == r
        Head_Count_value(x$y[i], x$weight[i], t, na.rm = na.rm)
      }, x = x, t = pov_line)
      c(value, valueByStratum)
    }
  }
  else {
    function(x, pov_line, rs, na.rm) {
      #pov_line <- p * weightedMedian(x$y, x$weight)
      value <- Head_Count_value(x$y, x$weight, pov_line, 
                                na.rm = na.rm)
    }
  }
}


# Poverty_Gap ------------------------------------------------------------------

Poverty_Gap <- function(y, 
                       smp_data, 
                       smp_domains = NULL, 
                       weights = NULL, 
                       pov_line = NULL, 
                       var = TRUE, 
                       bootType = "naive", 
                       R = NULL,
                       seed = NULL,
                       X = NULL, 
                       totals = NULL, 
                       na.rm=FALSE){
  
  # Needs to be character for the variance function
  smp_domains_chr <- smp_domains
  weights_chr <- weights
  
  # logical if domains selected
  byStratum <- !is.null(smp_domains)
  
  # smp_domains needs to be factor for this function
  smp_domains <- as.factor(smp_data[, smp_domains])
  rs <- levels(smp_domains)
  
  # sample size
  n <- length(smp_data[,y])
  #ts <- pov_line
  
  if (!is.null(weights)){
    weights <- smp_data[, weights]
  } 
  if (is.null(weights)){
    weights <- rep.int(1, n)
  } 
  
  # Overall PG 
  value <- Poverty_Gap_value(y = smp_data[,y], 
                            weights = weights, 
                            pov_line = pov_line, 
                            na.rm = na.rm)
  
  # if sample domains are given
  if (byStratum) {
    
    valueByStratum <- aggregate(1:n, list(stratum = smp_domains), 
                                PG2, y = smp_data[,y], weights = weights, 
                                pov_line = pov_line, na.rm = na.rm)
    names(valueByStratum)[2] <- "value"
  } else {
    valueByStratum <- NULL
  }
  
  res <- list(value = value, 
              valueByStratum = valueByStratum, 
              strata = rs, 
              pov_line = pov_line)
  
  class(res) <- c("PG")
  
  if (var == TRUE) {
    res <- direct_variance(y=y, 
                           weights=weights_chr, 
                           smp_data=smp_data,  
                           smp_domains = smp_domains_chr, 
                           indicator=res,
                           bootType=bootType, 
                           R = R,
                           seed = seed,
                           X = X, 
                           totals = totals,
                           na.rm=na.rm)
  } 
  return(res)
}


# Calculate weighted HCR - if weights = NULL, unweighted -----------------------
Poverty_Gap_value <- function(y, 
                             weights, 
                             pov_line, 
                             na.rm){
  if (is.null(weights)) {
    weights <- rep.int(1, length(y))
  } 
  if (isTRUE(na.rm)) {
    indices <- !is.na(y)
    y <- y[indices]
    weights <- weights[indices]
  }
  else if (any(is.na(y))) 
    return(NA)
  
  sw <- sum(weights)
  sapply(pov_line, function(t) sum(weights*(y < pov_line) * (pov_line - y) / pov_line))/sw*100
}


# Function to calculate sample HCR ---------------------------------------------
PG2 <- function(i, 
                 y, 
                 weights, 
                 pov_line = pov_line, 
                 na.rm) {
  
   Poverty_Gap_value(y = y[i], 
                   weights = weights[i], 
                   pov_line = pov_line, 
                   na.rm = na.rm)
}


# Get HCR function for bootstrap variance
getFun_PG <- function (indicator, byStratum) 
{
  if (byStratum) {
    function(x, pov_line, rs, na.rm) {
      
      #pov_line <- p * weightedMedian(x$y, x$weight)
      value <- Poverty_Gap_value(x$y, x$weight, pov_line, 
                                na.rm = na.rm)
      valueByStratum <- sapply(rs, function(r, x, t) {
        i <- x$stratum == r
        Head_Count_value(x$y[i], x$weight[i], t, na.rm = na.rm)
      }, x = x, t = pov_line)
      c(value, valueByStratum)
    }
  }
  else {
    function(x, pov_line, rs, na.rm) {
      #pov_line <- p * weightedMedian(x$y, x$weight)
      value <- Poverty_Gap_value(x$y, x$weight, pov_line, 
                                na.rm = na.rm)
    }
  }
}




# Gini coeffcient --------------------------------------------------------------
Gini <- function(y, 
                 smp_data, 
                 smp_domains, 
                 weights, 
                 sort = NULL,  
                 var = TRUE, 
                 bootType = c("calibrate", "naive"), 
                 R = NULL,
                 seed = NULL,
                 X, 
                 totals = NULL,
                 na.rm=FALSE){
  
  # logical if sample domains are present
  byStratum <- !is.null(smp_domains)
  
  
  # needs to be character for variance function
  smp_domains_chr <- smp_domains
  
  # needs to be factor for indicator calculation
  smp_domains <- as.factor(smp_data[, smp_domains])
  rs <- levels(smp_domains)
  
  # needs to be character for variance function
  y_chr <- y
  
  #  needs to be vector for value calculation
  y <- smp_data[, y]
  n <- length(y)
  
  weights_chr <- weights
  
  if (!is.null(weights)){
    weights <- smp_data[, weights]
  } 
  if (!is.null(sort)) {
    sort <- smp_data[, sort]
  }
  
  #  overall value
  value <- Gini_value(y = y, weights = weights, sort= sort, na.rm = na.rm)
  
  # if smp_domains are present
  if (byStratum) {
    valueByStratum <- aggregate(1:n, list(stratum = smp_domains), Gini2, y = y, 
                                weights = weights, sort = sort, na.rm = na.rm)
    names(valueByStratum)[ncol(valueByStratum)] <- "value"
    rs <- levels(smp_domains)
  } else {
    valueByStratum <- rs <- NULL
  }
  res <- list(value = value, valueByStratum = valueByStratum, 
              strata = rs)
  
  class(res) <- c("Gini")
  
  if(var == TRUE){
    res <- direct_variance(y = y_chr, 
                        weights=weights_chr, 
                        smp_data=smp_data,  
                        smp_domains = smp_domains_chr, 
                        indicator=res, 
                        bootType=bootType, 
                        R = R,
                        seed = seed,
                        X = X, 
                        totals = totals, 
                        na.rm =na.rm)
    
  }
  
  
  return(res)
  
}


Gini_value <- function (y, weights = NULL, sort = NULL, na.rm = FALSE) 
{
  if (isTRUE(na.rm)) {
    indices <- !is.na(y)
    y <- y[indices]
    if (!is.null(weights)) 
      weights <- weights[indices]
    if (!is.null(sort)) 
      sort <- sort[indices]
  }
  else if (any(is.na(y))) 
    return(NA)
  order <- if(is.null(sort)){
    order(y)
  } else {
    order(y, sort)
  }
  y <- y[order]
  if (is.null(weights)){
    weights <- rep.int(1, length(y))
  } else {
    weights <- weights[order]
  }
  wy <- weights * y
  sw <- sum(weights)
  cw <- cumsum(weights)
  100 * ((2 * sum(wy * cw) - sum(weights^2 * y))/(sw * sum(wy)) - 1)
}


Gini2 <- function(i, y, weights, sort, na.rm) {
  Gini_value(y = y[i], weights = weights[i], sort = sort[i], na.rm = na.rm)
}

getFun_Gini <- function (indicator, byStratum)
{
  if (byStratum) {
    function(x, pov_line, rs, na.rm) {
      value <- Gini_value(x$y, x$weight, na.rm = na.rm)
      valueByStratum <- sapply(rs, function(r, x, t) {
        i <- x$stratum == r
        Gini_value(x$y[i], x$weight[i], na.rm = na.rm)
      }, x = x)
      c(value, valueByStratum)
    }
  }
  else {
    function(x, pov_line, rs, na.rm) {
      Gini_value(x$y, x$weight, na.rm = na.rm)
    }
  }
}


# yome Quintile Share Ratio --------------------------------------------------

Quintile_Share <- function(y, 
                        weights = NULL, 
                        sort = NULL, 
                        #years = NULL, 
                        smp_domains = NULL, 
                        #design = NULL, 
                        #cluster = NULL, 
                        smp_data = NULL, 
                        var = NULL, 
                        bootType = c("calibrate", "naive"), 
                        R = NULL,
                        seed = NULL,
                        X, 
                        totals = NULL,
                        #alpha = 0.05, 
                        na.rm = FALSE, ...) {
  byStratum <- !is.null(smp_domains)
  y_chr <- y  
  y <- smp_data[, y]
    if (!is.null(weights))
      weights_chr <- weights
      weights <- smp_data[, weights]
    if (!is.null(sort)) 
      sort <- smp_data[, sort]
    if (byStratum)
      smp_domains_chr <-  smp_domains
      smp_domains <- smp_data[, smp_domains]

  n <- length(y)
  if (is.null(weights)) 
    weights <- weights <- rep.int(1, n)
  else if (!is.numeric(weights)) 
    stop("'weights' must be a numeric vector")
  if (!is.null(sort) && !is.vector(sort) && !is.ordered(sort)) {
    stop("'sort' must be a vector or ordered factor")
  }

  if (byStratum) {
    if (!is.vector(smp_domains) && !is.factor(smp_domains)) {
      stop("'smp_domains' must be a vector or factor")
    }
    else smp_domains <- as.factor(smp_domains)
  }


    value <- Quintile_Share_value(y, weights, sort, na.rm = na.rm)
    
  if (byStratum) {

    valueByStratum <- aggregate(1:n, list(stratum = smp_domains), qrR, 
                                y = y, weights = weights, 
                                sort = sort, na.rm = na.rm)
    names(valueByStratum)[ncol(valueByStratum)] <- "value"
    rs <- levels(smp_domains)
  }
  else valueByStratum <- rs <- NULL
  res <- list(value = value, valueByStratum = valueByStratum, 
                strata = rs)
  
  class(res) <- c("QSR")
  if (var==TRUE) {
    res <- direct_variance(y = y_chr, 
                           weights=weights_chr, 
                           smp_data=smp_data,  
                           smp_domains = smp_domains_chr, 
                           indicator=res, 
                           bootType=bootType, 
                           R = R,
                           seed = seed,
                           X = X, 
                           totals = totals, 
                           na.rm =na.rm)}
  return(res)
}

qrR <- function(i, y, weights, sort, na.rm) {
  Quintile_Share_value(y[i], weights[i], sort[i], na.rm)
}

Quintile_Share_value <- function (x, weights = NULL, sort = NULL, na.rm = FALSE) 
{
  if (isTRUE(na.rm)) {
    indices <- !is.na(x)
    x <- x[indices]
    if (!is.null(weights)) 
      weights <- weights[indices]
    if (!is.null(sort)) 
      sort <- sort[indices]
  }
  else if (any(is.na(x))) 
    return(NA)
  if (is.null(weights)) {
    weights <- rep.int(1, length(x))
  }
  order <- if (is.null(sort)) 
    order(x)
  else order(x, sort)
  x <- x[order]
  weights <- weights[order]
  
  #q <- incQuintile(x, weights, sort)
  #iq1 <- x <= q[1]
  #iq4 <- x > q[2]
  iq1 <- x <= weightedQuantile(x, weights, probs = 0.2, sorted = TRUE, 
                               na.rm = na.rm)
  
  iq4 <- x > weightedQuantile(x, weights, probs = 0.8, sorted = TRUE, 
                              na.rm = na.rm)
  
  
  (sum(weights[iq4] * x[iq4])/sum(weights[iq4]))/(sum(weights[iq1] * 
                                                        x[iq1])/sum(weights[iq1]))
}



weightedQuantile <- function (x, weights = NULL, probs = seq(0, 1, 0.25), sorted = FALSE, 
                              na.rm = FALSE){
  if (!is.numeric(x)) 
    stop("'x' must be a numeric vector")
  n <- length(x)
  if (n == 0 || (!isTRUE(na.rm) && any(is.na(x)))) {
    return(rep.int(NA, length(probs)))
  }
  if (!is.null(weights)) {
    if (!is.numeric(weights)) 
      stop("'weights' must be a numeric vector")
    else if (length(weights) != n) {
      stop("'weights' must have the same length as 'x'")
    }
    else if (!all(is.finite(weights))) 
      stop("missing or infinite weights")
    if (any(weights < 0)) 
      warning("negative weights")
    if (!is.numeric(probs) || all(is.na(probs)) || isTRUE(any(probs < 
                                                              0 | probs > 1))) {
      stop("'probs' must be a numeric vector with values in [0,1]")
    }
    if (all(weights == 0)) {
      warning("all weights equal to zero")
      return(rep.int(0, length(probs)))
    }
  }
  if (isTRUE(na.rm)) {
    indices <- !is.na(x)
    x <- x[indices]
    if (!is.null(weights)) 
      weights <- weights[indices]
  }
  if (!isTRUE(sorted)) {
    order <- order(x)
    x <- x[order]
    weights <- weights[order]
  }
  if (is.null(weights)) 
    rw <- (1:n)/n
  else rw <- cumsum(weights)/sum(weights)
  q <- sapply(probs, function(p) {
    if (p == 0) 
      return(x[1])
    else if (p == 1) 
      return(x[n])
    select <- min(which(rw >= p))
    if (rw[select] == p) 
      mean(x[select:(select + 1)])
    else x[select]
  })
  return(unname(q))
}




getFun_QSR <- function (indicator, byStratum) 
{
  if (byStratum) {
    function(x, p, rs, na.rm) {
      value <- Quintile_Share_value(x$y, x$weight, na.rm = na.rm)
      valueByStratum <- sapply(rs, function(r, x, t) {
        i <- x$stratum == r
        Quintile_Share_value(x$y[i], x$weight[i], na.rm = na.rm)
      }, x = x)
      c(value, valueByStratum)
    }
  }
  else {
    function(x, p, rs, na.rm) {
      Quintile_Share_value(x$y,x$weight, na.rm = na.rm)
    }
  }
}