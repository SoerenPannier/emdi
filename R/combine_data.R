#' Combines sample and population data
#'
#' This function combines different data sets.
#'
#' @param pop_data a data frame with population data.
#' @param pop_domains a character string indicating the domain variable that is
#' included in \code{pop_data}.
#' @param smp_data a data frame with sample data.
#' @param smp_domains a character string indicating the domain variable that is 
#' included in \code{smp_data}.
#' @return a combined data set.
#' @export


combine_data <- function(pop_data, pop_domains, smp_data, smp_domains) {


  smp_domains_vec <- smp_data[, smp_domains]
  pop_domains_vec <- pop_data[, pop_domains]

  obs_dom <- pop_domains_vec %in% smp_domains_vec
  smp_data[, smp_domains] <- NULL
  pop_data[, pop_domains] <- NULL


  smp_data[, pop_domains] <- pop_domains_vec[obs_dom == TRUE]
  pop_data[, pop_domains] <- pop_domains_vec
  data <- merge(smp_data, pop_data, by = pop_domains, all = TRUE)

  return(data)
}
