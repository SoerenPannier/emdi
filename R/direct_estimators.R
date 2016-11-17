



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


# Income Quintile Share Ratio --------------------------------------------------

function (y, 
          weights = NULL, 
          sort = NULL, 
          #years = NULL, 
          smp_domains = NULL, 
          #design = NULL, 
          #cluster = NULL, 
          data = NULL, 
          var = NULL, 
          #alpha = 0.05, 
          na.rm = FALSE, ...) 
{
  #byYear <- !is.null(years)
  byStratum <- !is.null(smp_domains)
  
  
    y <- data[, y]
    if (!is.null(weights)) 
      weights <- data[, weights]
    if (!is.null(sort)) 
      sort <- data[, sort]
    #if (byYear) 
    #  years <- data[, years]
    if (byStratum) 
      smp_domains <- data[, smp_domains]
    #if (!is.null(var)) {
    #  if (!is.null(design)) 
    #    design <- data[, design]
     # if (!is.null(cluster)) 
    #    cluster <- data[, cluster]
    }
    
  if (!is.numeric(y)) {
    stop("'y' must be a numeric vector")
  }
  n <- length(y)
  if (is.null(weights)) {
    weights <- weights <- rep.int(1, n)
  } else if (!is.numeric(weights)) {
    stop("'weights' must be a numeric vector")
  } 
  if (!is.null(sort) && !is.vector(sort) && !is.ordered(sort)) {
    stop("'sort' must be a vector or ordered factor")
  }
  #if (byYear && !is.numeric(years)) {
  #  stop("'years' must be a numeric vector")
  #}
  if (byStratum) {
    if (!is.vector(smp_domains) && !is.factor(smp_domains)) {
      stop("'smp_domains' must be a vector or factor")
    } else {
      smp_domains <- as.factor(smp_domains)
    } 
  }

    if (length(weights) != n) {
      stop("'weights' must have the same length as 'x'")
    }
    if (!is.null(sort) && length(sort) != n) {
      stop("'sort' must have the same length as 'x'")
    }
    if (byYear && length(years) != n) {
      stop("'years' must have the same length as 'x'")
    }
    if (byStratum && length(smp_domains) != n) {
      stop("'smp_domains' must have the same length as 'x'")
    }

  if (byYear) {
    ys <- sort(unique(years))
    qr <- function(y, y, weights, sort, years, na.rm) {
      i <- years == y
      quintileRatio(y[i], weights[i], sort[i], na.rm = na.rm)
    }
    value <- sapply(ys, qr, y = y, weights = weights, 
                    sort = sort, years = years, na.rm = na.rm)
    names(value) <- ys
  }
  else {
    ys <- NULL
    value <- quintileRatio(y, weights, sort, na.rm = na.rm)
  }
  if (byStratum) {
    qrR <- function(i, y, weights, sort, na.rm) {
      quintileRatio(y[i], weights[i], sort[i], na.rm = na.rm)
    }
    valueByStratum <- aggregate(1:n, if (byYear) 
      list(year = years, stratum = smp_domains)
      else list(stratum = smp_domains), qrR, y = y, weights = weights, 
      sort = sort, na.rm = na.rm)
    names(valueByStratum)[ncol(valueByStratum)] <- "value"
    rs <- levels(smp_domains)
  }
  else valueByStratum <- rs <- NULL
  res <- constructQsr(value = value, valueByStratum = valueByStratum, 
                      years = ys, strata = rs)
  if (!is.null(var)) {
    res <- variance(y, weights, years, smp_domains, design, 
                    cluster, indicator = res, alpha = alpha, na.rm = na.rm, 
                    type = var, ...)
  }
  return(res)
}