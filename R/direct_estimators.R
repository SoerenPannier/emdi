
# Head Count Ratio -------------------------------------------------------------
Head_Count <- function(framework,  
                       var = TRUE, 
                       bootType = "naive", 
                       B = NULL,
                       seed = NULL,
                       X = NULL, 
                       totals = NULL, 
                       na.rm=FALSE){
  
  # HCR for whole sample  
  value <- Head_Count_value(y = framework$y_vec, 
                            weights = framework$weights_vec, 
                            pov_line = framework$pov_line, 
                            na.rm = na.rm)
  
  # if sample domains are given
  if (framework$byStratum) {
    valueByStratum <- aggregate(1:framework$N_smp, list(stratum = framework$smp_domains_vec), 
                                HCR2, y = framework$y_vec, weights = framework$weights_vec, 
                                pov_line = framework$pov_line, na.rm = na.rm)
    names(valueByStratum)[2] <- "Head_Count"
    names(valueByStratum)[1] <- "Domain"
    rs <- as.factor(as.matrix(valueByStratum[1])[,1])
    
  } else {
    valueByStratum <- NULL
  }
  
  res <- list(value = value, 
              valueByStratum = valueByStratum, 
              strata = framework$rs, 
              pov_line = framework$pov_line)
  
  class(res) <- c("HCR")
  
  if (var == TRUE) {
  
    res <- direct_variance(# is a character here and is converted to numeric 
                           # in the variance function 
                           y=framework$y,  
                           # is a character here and is converted to numeric
                           # in the variance function 
                           weights=framework$weights, 
                           # needs to be given such that above arguments can
                           # be converted
                           smp_data=framework$smp_data,  
                           smp_domains = framework$smp_domains_vec, 
                           indicator=res,
                           bootType=bootType, 
                           B = B,
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
      value <- Head_Count_value(x$y, x$weight, pov_line, 
                                na.rm = na.rm)
    }
  }
}


# Poverty_Gap ------------------------------------------------------------------

Poverty_Gap <- function(framework, 
                        var = TRUE, 
                        bootType = "naive", 
                        B = NULL,
                        seed = NULL,
                        X = NULL, 
                        totals = NULL, 
                        na.rm=FALSE){
  
  # PG using complete sample
  value <- Poverty_Gap_value(y = framework$y_vec, 
                             weights = framework$weights_vec, 
                             pov_line = framework$pov_line, 
                             na.rm = na.rm)
  
  # if sample domains are given
  if (framework$byStratum) {
    
    valueByStratum <- aggregate(1:framework$N_smp, list(stratum = framework$smp_domains_vec), 
                                PG2, y = framework$y_vec, weights = framework$weights_vec, 
                                pov_line = framework$pov_line, na.rm = na.rm)
    names(valueByStratum)[2] <- "Poverty_Gap"
    names(valueByStratum)[1] <- "Domain"
    rs <- as.factor(as.matrix(valueByStratum[1])[,1])
    
  } else {
    valueByStratum <- NULL
  }
  
  res <- list(value = value, 
              valueByStratum = valueByStratum, 
              strata = framework$rs, 
              pov_line =  framework$pov_line)
  
  class(res) <- c("PG")
  
  if (var == TRUE) {
    res <- direct_variance(# is a character here
                           y=framework$y, 
                           # is a character here
                           weights=framework$weights, 
                           smp_data=framework$smp_data,  
                           smp_domains = framework$smp_domains_vec, 
                           indicator=res,
                           bootType=bootType, 
                           B = B,
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
      
      value <- Poverty_Gap_value(x$y, x$weight, pov_line, 
                                 na.rm = na.rm)
      valueByStratum <- sapply(rs, function(r, x, t) {
        i <- x$stratum == r
        Poverty_Gap_value(x$y[i], x$weight[i], t, na.rm = na.rm)
      }, x = x, t = pov_line)
      c(value, valueByStratum)
    }
  }
  else {
    function(x, pov_line, rs, na.rm) {

      value <- Poverty_Gap_value(x$y, x$weight, pov_line, 
                                 na.rm = na.rm)
    }
  }
}


# Gini coefficient -------------------------------------------------------------

Gini <- function(framework, 
                 sort = NULL,  
                 var = TRUE, 
                 bootType = c("calibrate", "naive"), 
                 B = NULL,
                 seed = NULL,
                 X, 
                 totals = NULL,
                 na.rm=FALSE){
  
  #  Gini using full sample
  value <- Gini_value(y = framework$y_vec, weights = framework$weights_vec, 
                      sort = sort, na.rm = na.rm)
  
  # if smp_domains are present
  if (framework$byStratum) {
    valueByStratum <- aggregate(1:framework$N_smp, list(stratum = framework$smp_domains_vec), 
                                Gini2, y = framework$y_vec, 
                                weights = framework$weights_vec, sort = sort, na.rm = na.rm)
    names(valueByStratum)[ncol(valueByStratum)] <- "Gini"
    names(valueByStratum)[1] <- "Domain"
    rs <- as.factor(as.matrix(valueByStratum[1])[,1])
  } else {
    valueByStratum <- NULL
  }
  res <- list(value = value, valueByStratum = valueByStratum, 
              strata = framework$rs)
  
  class(res) <- c("Gini")
  
  if(var == TRUE){
    res <- direct_variance(# here a character
                           y = framework$y, 
                           # here a character
                           weights=framework$weights, 
                           smp_data=framework$smp_data,  
                           smp_domains = framework$smp_domains_vec, 
                           indicator=res, 
                           bootType=bootType, 
                           B = B,
                           seed = seed,
                           X = X, 
                           totals = totals, 
                           na.rm =na.rm)
    
  }
  
  
  return(res)
  
}


Gini_value <- function (y, weights = NULL, sort = NULL, na.rm = FALSE) {

  order <- if(is.null(sort)){
    order(y)
  } else {
    order(y, sort)
  }
  y <- y[order]
  if (!is.null(weights)){
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

# Quintile Share Ratio ---------------------------------------------------------

Quintile_Share <- function(framework, 
                           sort = NULL, 
                           var = NULL, 
                           bootType = c("calibrate", "naive"), 
                           B = NULL,
                           seed = NULL,
                           X, 
                           totals = NULL,
                           na.rm = FALSE, ...) {

  
  # Quintile Share Ratio using full sample
  value <- Quintile_Share_value(x=framework$y_vec, weights=framework$weights_vec, 
                                sort = sort, na.rm = na.rm)
  
  if (framework$byStratum) {
    
    valueByStratum <- aggregate(1:framework$N_smp, list(stratum = framework$smp_domains_vec), qrR, 
                                y = framework$y_vec, weights = framework$weights_vec, 
                                sort = sort, na.rm = na.rm)
    names(valueByStratum)[ncol(valueByStratum)] <- "Quintile_Share"
    names(valueByStratum)[1] <- "Domain"
    rs <- as.factor(as.matrix(valueByStratum[1])[,1])
  } else {
    valueByStratum <- rs <- NULL
  }
  res <- list(value = value, valueByStratum = valueByStratum, 
              strata = framework$rs)
  
  class(res) <- c("QSR")
  if (var==TRUE) {
    res <- direct_variance(# here a character
                           y = framework$y, 
                           # here a character
                           weights=framework$weights, 
                           smp_data=framework$smp_data,  
                           smp_domains = framework$smp_domains_vec, 
                           indicator=res, 
                           bootType=bootType, 
                           B = B,
                           seed = seed,
                           X = X, 
                           totals = totals, 
                           na.rm =na.rm)}
  return(res)
}

qrR <- function(i, y, weights, sort, na.rm) {
  Quintile_Share_value(y[i], weights[i], sort[i], na.rm)
}

Quintile_Share_value <- function (x, weights = NULL, sort = NULL, na.rm = FALSE){

  order <- if (is.null(sort)) 
    order(x)
  else order(x, sort)
  x <- x[order]
  weights <- weights[order]
  
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
