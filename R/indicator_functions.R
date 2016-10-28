# Indicator functions that are called in ebp (only internal) -------------------

# Reference indicators
# Mean
# deletion of .Internal since not allowed for CRAN upload
fast_mean <- function(X) {
  mean(X)
}

# Poverty indicators
# Head Count Ratio
hcr_function <- function(y, pov_line) {
  mean(y < pov_line)
}

# Poverty Gap
pgap_function <- function(y, pov_line) {
  mean((y < pov_line) * (pov_line - y) / pov_line)
}

# Inequality indicators

# Income Quintile Share Ratio
qsr_function <- function(y) {
  sum(y[(y > quantile(y,0.8,type = 2))]) / sum(y[(y <= quantile(y,0.2,type = 2))])
}

#TODO REMOVE WHEN DIRECT FINISHED
# qsr_function2 <- function(y) {
#   I1<-(y <= quantile(y,0.2,type = 7))
#   I2<-(y > quantile(y,0.8,type = 7))
#   cat("anzk",sum(I1), "anzg",sum(I2))
#   (sum(1*y[I2])/sum(rep(1, length(y)))) / 
#     (sum(1*y[I1])/sum(rep(1, length(y))))
# }
# 
# qsr_function3 <- function(x) {
#   weights <- rep.int(1, length(x))
#   q <- incQuintile(x, weights)
#   iq1 <- x <= q[1]
#   iq4 <- x > q[2]
#   (sum(weights[iq4] * x[iq4])/sum(weights[iq4]))/(sum(weights[iq1] * 
#                                                         x[iq1])/sum(weights[iq1]))
#   
# }


#  Gini coefficient
gini_function <- function(X) {
  n <- length(X)
  X <- sort(X)
  G <- sum(X * 1L:n)
  G <- 2 * G / sum(X) - (n + 1L)
  return(G / n)
}


