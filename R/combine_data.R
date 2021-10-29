#' Combines sample and population data
#'
#' This function combines the aggregated population information with the
#' aggregated sample data. The merge is based on the domains. Out-of-sample
#' domains will have NA values for the variables from the sample data.
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

  if (all(smp_domains_vec %in% pop_domains_vec) == FALSE) {
    stop("All sample domains need to be available in population domains.")
  }
  if (all(pop_domains_vec %in% smp_domains_vec) == FALSE) {
    message(strwrap(prefix = " ", initial = "",
                    "Non-sampled domains exist."))
  }

  data <- merge(smp_data, pop_data,
    by.x = smp_domains, by.y = pop_domains,
    all = TRUE
  )

  return(data)
}
