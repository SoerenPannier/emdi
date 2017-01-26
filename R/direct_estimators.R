
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
  if (framework$byDomain) {
    valueByDomain <- 
      aggregate(1:framework$N_smp, list(Domain = framework$smp_domains_vec), 
                                HCR2, y = framework$y_vec, 
                weights = framework$weights_vec, 
                                pov_line = framework$pov_line, na.rm = na.rm)
    names(valueByDomain)[2] <- "Head_Count"
    names(valueByDomain)[1] <- "Domain"
    rs <- as.factor(as.matrix(valueByDomain[1])[,1])
    
  } else {
    valueByDomain <- NULL
  }
  
  res <- list(value = value, 
              valueByDomain = valueByDomain, 
              domain = framework$rs, 
              pov_line = framework$pov_line)
  
  class(res) <- c("HCR")
  
  if (var == TRUE) {
  
    res <- direct_variance(# is a character here and is converted to numeric 
                           # in the variance function 
                           y = framework$y,  
                           # is a character here and is converted to numeric
                           # in the variance function 
                           weights = framework$weights, 
                           # needs to be given such that above arguments can
                           # be converted
                           smp_data = framework$smp_data,  
                           smp_domains = framework$smp_domains_vec, 
                           indicator = res,
                           bootType = bootType, 
                           B = B,
                           seed = seed,
                           X = X, 
                           totals = totals,
                           na.rm = na.rm)
  } 
  return(res)
}


# Calculate weighted HCR - if weights = NULL, unweighted
Head_Count_value <- function(y, 
                             weights, 
                             pov_line, 
                             na.rm){
  sw <- sum(weights)
  sapply(pov_line, function(t) sum(weights[y < t]))/sw*100
}


# Function to calculate sample HCR
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
getFun_HCR <- function (indicator, byDomain) 
{
  if (byDomain) {
    function(x, pov_line, rs, na.rm, prob = NULL) {
      value <- Head_Count_value(x$y, x$weight, pov_line, 
                                na.rm = na.rm)
      valueByDomain <- sapply(rs, function(r, x, t) {
        i <- x$Domain == r
        Head_Count_value(x$y[i], x$weight[i], t, na.rm = na.rm)
      }, x = x, t = pov_line)
      c(value, valueByDomain)
    }
  }
  else {
    function(x, pov_line, rs, na.rm, prob = NULL) {
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
  if (framework$byDomain) {
    
    valueByDomain <- 
      aggregate(1:framework$N_smp, list(Domain = framework$smp_domains_vec), 
                                PG2, y = framework$y_vec, 
                weights = framework$weights_vec, 
                                pov_line = framework$pov_line, na.rm = na.rm)
    names(valueByDomain)[2] <- "Poverty_Gap"
    names(valueByDomain)[1] <- "Domain"
    rs <- as.factor(as.matrix(valueByDomain[1])[,1])
    
  } else {
    valueByDomain <- NULL
  }
  
  res <- list(value = value, 
              valueByDomain = valueByDomain, 
              domain = framework$rs, 
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


# Calculate weighted HCR - if weights = NULL, unweighted
Poverty_Gap_value <- function(y, 
                              weights, 
                              pov_line, 
                              na.rm){
  
  sw <- sum(weights)
  sapply(pov_line, function(t) sum(weights  *(y < pov_line) * (pov_line - y) / 
                                     pov_line)) / sw * 100
}


# Function to calculate sample HCR 
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
getFun_PG <- function (indicator, byDomain) 
{
  if (byDomain) {
    function(x, pov_line, rs, na.rm, prob = NULL) {
      
      value <- Poverty_Gap_value(x$y, x$weight, pov_line, 
                                 na.rm = na.rm)
      valueByDomain <- sapply(rs, function(r, x, t) {
        i <- x$Domain == r
        Poverty_Gap_value(x$y[i], x$weight[i], t, na.rm = na.rm)
      }, x = x, t = pov_line)
      c(value, valueByDomain)
    }
  }
  else {
    function(x, pov_line, rs, na.rm, prob = NULL) {

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
  if (framework$byDomain) {
    valueByDomain <- 
      aggregate(1:framework$N_smp, list(Domain = framework$smp_domains_vec), 
                                Gini2, y = framework$y_vec, 
                                weights = framework$weights_vec, 
                sort = sort, na.rm = na.rm)
    names(valueByDomain)[ncol(valueByDomain)] <- "Gini"
    names(valueByDomain)[1] <- "Domain"
    rs <- as.factor(as.matrix(valueByDomain[1])[,1])
  } else {
    valueByDomain <- NULL
  }
  res <- list(value = value, valueByDomain = valueByDomain, 
              domain = framework$rs)
  
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

getFun_Gini <- function (indicator, byDomain)
{
  if (byDomain) {
    function(x, pov_line, rs, na.rm, prob = NULL) {
      value <- Gini_value(x$y, x$weight, na.rm = na.rm)
      valueByDomain <- sapply(rs, function(r, x, t) {
        i <- x$Domain == r
        Gini_value(x$y[i], x$weight[i], na.rm = na.rm)
      }, x = x)
      c(value, valueByDomain)
    }
  }
  else {
    function(x, pov_line, rs, na.rm, prob = NULL) {
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
  
  if (framework$byDomain) {
    
    valueByDomain <- 
      aggregate(1:framework$N_smp, list(Domain = framework$smp_domains_vec), qrR, 
                                y = framework$y_vec, weights = framework$weights_vec, 
                                sort = sort, na.rm = na.rm)
    names(valueByDomain)[ncol(valueByDomain)] <- "Quintile_Share"
    names(valueByDomain)[1] <- "Domain"
    rs <- as.factor(as.matrix(valueByDomain[1])[,1])
  } else {
    valueByDomain <- rs <- NULL
  }
  res <- list(value = value, valueByDomain = valueByDomain, 
              domain = framework$rs)
  
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
  
  quant14 <- Quant_value(y = x, 
                         weights = weights,
                         pov_line = NULL,
                         prob = c(0.2, 0.8),
                         na.rm = na.rm)
  
  iq1 <- x <= quant14[1]
  iq4 <- x > quant14[2]
   
  (sum(weights[iq4] * x[iq4])/sum(weights[iq4]))/(sum(weights[iq1] * 
                                                        x[iq1])/sum(weights[iq1]))
}

getFun_QSR <- function (indicator, byDomain) 
{
  if (byDomain) {
    function(x, p, rs, na.rm, prob = NULL) {
      value <- Quintile_Share_value(x$y, x$weight, na.rm = na.rm)
      valueByDomain <- sapply(rs, function(r, x, t) {
        i <- x$Domain == r
        Quintile_Share_value(x$y[i], x$weight[i], na.rm = na.rm)
      }, x = x)
      c(value, valueByDomain)
    }
  }
  else {
    function(x, p, rs, na.rm, prob = NULL) {
      Quintile_Share_value(x$y,x$weight, na.rm = na.rm)
    }
  }
}

# Mean -------------------------------------------------------------------------

Mean <- function(framework, 
                 var = TRUE, 
                 bootType = "naive", 
                 B = NULL,
                 seed = NULL,
                 X = NULL, 
                 totals = NULL, 
                 na.rm=FALSE){
  
  # Mean using complete sample
  value <- Mean_value(y = framework$y_vec, 
                      weights = framework$weights_vec, 
                      pov_line = framework$pov_line, 
                      na.rm = na.rm)
  
  # if sample domains are given
  if (framework$byDomain) {
    valueByDomain <- 
      aggregate(1:framework$N_smp, list(Domain = framework$smp_domains_vec), 
                Mean2, y = framework$y_vec, 
                weights = framework$weights_vec, 
                pov_line = framework$pov_line, na.rm = na.rm)
    names(valueByDomain)[2] <- "Mean"
    names(valueByDomain)[1] <- "Domain"
    rs <- as.factor(as.matrix(valueByDomain[1])[,1])
    
  } else {
    valueByDomain <- NULL
  }
  
  res <- list(value = value, 
              valueByDomain = valueByDomain, 
              domain = framework$rs, 
              pov_line =  framework$pov_line)
  
  class(res) <- c("Mean")
  
  if (var == TRUE) {
    res <- direct_variance(# is a character here
      y = framework$y, 
      # is a character here
      weights = framework$weights, 
      smp_data = framework$smp_data,  
      smp_domains = framework$smp_domains_vec, 
      indicator = res,
      bootType = bootType, 
      B = B,
      seed = seed,
      X = X, 
      totals = totals,
      na.rm = na.rm)
  } 
  return(res)
}

# Calculate weighted Mean - if weights = NULL, unweighted
Mean_value <- function(y, 
                       weights, 
                       pov_line, 
                       na.rm){
  weighted.mean(x = y, na.rm = na.rm, w = weights )
}

# Function to calculate sample Means
Mean2 <- function(i, 
                  y, 
                  weights, 
                  pov_line = pov_line, 
                  na.rm) {
  
  Mean_value(y = y[i], 
             weights = weights[i], 
             pov_line = pov_line, 
             na.rm = na.rm)
}


# Get Mean function for bootstrap variance
getFun_Mean <- function (indicator, byDomain) 
{
  if (byDomain) {
    function(x, pov_line, rs, na.rm, prob = NULL) {
      
      value <- Mean_value(x$y, x$weight, pov_line, 
                          na.rm = na.rm)
      valueByDomain <- sapply(rs, function(r, x, t) {
        i <- x$Domain == r
        Mean_value(x$y[i], x$weight[i], t, na.rm = na.rm)
      }, x = x, t = pov_line)
      c(value, valueByDomain)
    }
  }
  else {
    function(x, pov_line, rs, na.rm, prob = NULL) {
      
      value <- Mean_value(x$y, x$weight, pov_line, 
                          na.rm = na.rm)
    }
  }
}

# Quantiles ---------------------------------------------------------------------

Quants <- function(framework, 
                   var = TRUE, 
                   bootType = "naive", 
                   B = NULL,
                   seed = NULL,
                   X = NULL, 
                   totals = NULL, 
                   na.rm = FALSE,
                   prob){
  
  # Mean using complete sample
  value <- Quant_value(y = framework$y_vec, 
                       weights = framework$weights_vec, 
                       pov_line = framework$pov_line,
                       prob = prob,
                       na.rm = na.rm)
  
  # if sample domains are given
  if (framework$byDomain) {
    valueByDomain <- 
      aggregate(1:framework$N_smp, list(Domain = framework$smp_domains_vec), 
                Quant2, y = framework$y_vec, 
                weights = framework$weights_vec, 
                pov_line = framework$pov_line, 
                na.rm = na.rm,
                prob = prob)
    names(valueByDomain)[2] <- ifelse(prob != 0.5,
                                       paste0("Quant_", as.integer(prob * 100)),
                                       "Median")
    names(valueByDomain)[1] <- "Domain"
    rs <- as.factor(as.matrix(valueByDomain[1])[,1])
    
  } else {
    valueByDomain <- NULL
  }
  
  res <- list(value = value, 
              valueByDomain = valueByDomain, 
              domain = framework$rs, 
              pov_line =  framework$pov_line)
  
  class(res) <- c("Quant")
  
  if (var == TRUE) {
    res <- direct_variance(# is a character here
      y = framework$y, 
      # is a character here
      weights = framework$weights, 
      smp_data = framework$smp_data,  
      smp_domains = framework$smp_domains_vec, 
      indicator = res,
      bootType = bootType, 
      B = B,
      seed = seed,
      X = X, 
      totals = totals,
      na.rm = na.rm,
      prob = prob)
  } 
  return(res)
}


# Calculate weighted Quantile - if weights = NULL
Quant_value <- function(y, 
                        weights, 
                        pov_line,
                        prob,
                        na.rm){
  wtd.quantile(x = y, na.rm = na.rm, weights = weights, probs = prob)
}


# Function to calculate sample Quantiles
Quant2 <- function(i, 
                   y, 
                   weights, 
                   pov_line = pov_line, 
                   na.rm,
                   prob) {
  
  Quant_value(y = y[i], 
              weights = weights[i], 
              pov_line = pov_line, 
              prob = prob,
              na.rm = na.rm)
}


# Get Mean function for bootstrap variance
getFun_Quant <- function (indicator, byDomain) 
{
  if (byDomain) {
    function(x, pov_line, rs, na.rm, prob) {
      
      value <- Quant_value(x$y, x$weight, pov_line, prob = prob,
                           na.rm = na.rm)
      valueByDomain <- sapply(rs, function(r, x, t) {
        i <- x$Domain == r
        Quant_value(x$y[i], x$weight[i], t, prob = prob, na.rm = na.rm)
      }, x = x, t = pov_line)
      c(value, valueByDomain)
    }
  }
  else {
    function(x, pov_line, rs, na.rm, prob) {
      
      value <- Quant_value(x$y, x$weight, pov_line, prob = prob,
                           na.rm = na.rm)
    }
  }
}
