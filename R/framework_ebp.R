# Internal documentation -------------------------------------------------------

# The function notation defines the notational framework for the EBP approach
# e.g. number of households in population or sample (per domain), distinction
# between in-sample and out-of-sample
# see Molina and Rao (2003) p.370-371

# External documentation -------------------------------------------------------

# #' Determines a notational framework for the EBP approach
# #'
# #' Function \code{notation} arranges the two data sets by domains, defines
# #' domain vectors and calculates the determinants of the notational framework
# #' for the EBP approach.
# #' @param fixed a two-sided linear formula object describing the
# #' fixed-effects part of the nested error linear regression model with the
# #' dependent variable on the left of a ~ operator and the explanatory
# #' variables on the right, separated by + operators. The argument corresponds
# #' to the argument \code{fixed} in function \code{\link[nlme]{lme}}.
# #' @param pop_data a data frame that needs to comprise the variables
# #' named on the right of the ~ operator in \code{fixed} (see \code{\link{ebp}}),
# #' i.e. the explanatory variables, and \code{pop_domains}.
# #' @param pop_domains a character string containing the name of a variable that
# #' indicates domains in the population data. The variable can be numeric or
# #' a factor but needs to be of the same class as the variable named in
# #' \code{smp_domains}.
# #' @param smp_data a data frame that needs to comprise all variables named in
# #' \code{fixed} (see \code{\link{ebp}}) and \code{smp_domains}.
# #' @param smp_domains a character string containing the name of a variable
# #' that indicates domains in the sample data. The variable can be numeric or a
# #' factor but needs to be of the same class as the variable named in
# #' \code{pop_domains}.
# #' @param pov_line a number defining a poverty line. A poverty line is
# #' needed for calculation e.g. of head count ratios and poverty gaps. For
# #' instance, a poverty line could be set to 60\% of the median of the welfare
# #' variable that is selected as dependent variable similary to the
# #' At-risk-of-poverty rate used in the EU (see also
# #' \cite{Social Protection Committee 2001}).
# #' @param custom_indicator a list of functions containing the indicators 
# #' to be calculated additionaly,such functions must and must only depend on the 
# #' population income vector \code{y} and the povertyline \code{pov_line}
# #' @param na.rm if TRUE, observations with \code{NA} values are deleted from the 
# #' population and sample data. For the EBP procedure complete observations  
# #' are required. 
# #' @return a list with 17 named elements:
# #' \tabular{rll}{
# #'                    \tab  \code{pop_data} \tab an arranged data set of
# #'                     population data \cr
# #'                     \tab \code{pop_domains_vec} \tab an arranged vector
# #'                     of the domain indicator variable \cr
# #'                     \tab \code{smp_data} \tab an arranged data set of sample data \cr
# #'                     \tab \code{smp_domains_vec} \tab an arranged vector of
# #'                     the domain indicator variable \cr
# #'                     \tab \code{smp_domains} \tab a character naming the
# #'                     domain indicator variable (see also \code{smp_domains} in
# #'                     \code{\link{ebp}}) \cr
# #'                     \tab \code{N_pop} \tab total number of units in population \cr
# #'                     \tab \code{N_smp} \tab total number of units in sample \cr
# #'                     \tab \code{N_unobs} \tab number of out-of-sample units \cr
# #'                     \tab \code{N_dom_pop} \tab number of domains in the population \cr
# #'                     \tab \code{N_dom_smp} \tab number of domains in the sample \cr
# #'                     \tab \code{N_dom_unobs} \tab number of out-of-sample domains \cr
# #'                     \tab \code{n_pop} \tab number of households in population per
# #'                     domain \cr
# #'                     \tab \code{n_smp} \tab number of households in sample per
# #'                     domain \cr
# #'                     \tab \code{obs_dom} \tab logical vector if population
# #'                     domain is also in sample \cr
# #'                     \tab \code{dist_obs_dom} \tab logical vector if
# #'                     population domain is unique in sample \cr
# #'                     \tab \code{indicator_list} \tab a list of all poverty indicators to be computed \cr
# #'                     \tab \code{indicator_names} \tab names corresponding to the values computed by the indicators \cr
# #'  }
# #' @seealso \code{\link{emdiObject}}, \code{\link{ebp}}
# #' @examples
# #' # Loading data
# #' data("Xoutsamp_AuxVar")
# #' data("incomedata")
# #'
# #' framework <- notation(Xoutsamp_AuxVar, "provlab", incomedata, "provlab")
# #' @export

notation <- function(fixed, pop_data, pop_domains, smp_data, smp_domains, 
                     pov_line, custom_indicator = NULL, na.rm) {

  #smp_domains_orig <- smp_domains
  
  # Reduction of number of variables
  mod_vars <- gsub(" ", "",unlist(strsplit(paste(fixed[3]), "[+]")), 
                   fixed = TRUE)
  
 
  pop_vars <- c(mod_vars, pop_domains)
  pop_data <- pop_data[, pop_vars]
  smp_vars <- c(as.character(fixed[2]), mod_vars, smp_domains)
  smp_data <- smp_data[, smp_vars]
  
  # Deletion of NA
  if(na.rm == T){
    pop_data <- na.omit(pop_data)
    smp_data <- na.omit(smp_data)
  } else if(any(is.na(pop_data)) || any(is.na(smp_data))){
    stop('EBP does not work with missing values. Set na.rm = TRUE in function 
          ebp.')
  }
  
  
  # Order of domains
  pop_data <- pop_data[order(pop_data[[pop_domains]]),]
  pop_data[[pop_domains]] <- factor(pop_data[[pop_domains]], levels = unique(pop_data[[pop_domains]]))
  pop_domains_vec <- pop_data[[pop_domains]]

  smp_data <- smp_data[order(smp_data[[smp_domains]]),]
  smp_data[[smp_domains]] <- factor(smp_data[[smp_domains]], levels = unique(pop_data[[pop_domains]]))
  smp_domains_vec <- smp_data[[smp_domains]]
  smp_domains_vec <- droplevels(smp_domains_vec)
  
  
  fw_check1(pop_domains_vec = pop_domains_vec, smp_domains_vec = smp_domains_vec)


  # Number of households in population
  N_pop <- length(pop_domains_vec)
  # Number of households in sample
  N_smp <- length(smp_domains_vec)
  # Number of out-of-sample households
  N_unobs= N_pop - N_smp
  # Number of domains in the population
  N_dom_pop <- length(unique(pop_domains_vec))
  # Number of domains in the sample
  N_dom_smp <- length(unique(smp_domains_vec))
  # Number of out-of-sample domains
  N_dom_unobs <- N_dom_pop - N_dom_smp
  # Number of households in population per domain
  n_pop <- as.vector(table(pop_domains_vec))
  # Number of households in sample per domain
  smp_domains_vec_tmp <- as.numeric(smp_domains_vec)
  n_smp <- as.vector(table(smp_domains_vec_tmp))

  # Indicator variables that indicate if domain is in- or out-of-sample
  obs_dom <- pop_domains_vec %in% unique(smp_domains_vec)
  dist_obs_dom <- unique(pop_domains_vec) %in% unique(smp_domains_vec)
  
  fw_check2(obs_dom = obs_dom, dist_obs_dom = dist_obs_dom)

  indicator_list <- list(
    fast_mean = function(y, pov_line) {t(mean(y))},
    hcr = function(y, pov_line) {t(mean(y < pov_line))},
    pgap = function(y, pov_line) {t(mean((y < pov_line) * (pov_line - y) / pov_line))},
    gini = function(y, pov_line) {
      n <- length(y)
      y <- sort(y)
      G <- sum(y * 1L:n)
      G <- 2 * G / sum(y) - (n + 1L)
      G <- t(G / n)
      return(G)
    }
    ,
    qsr = function(y, pov_line) {   
      weights <- rep.int(1, length(y))
      quant14 <- wtd.quantile(x = y, 
                            weights = weights,
                            probs = c(0.2, 0.8))
    
      iq1 <- y <= quant14[1]
      iq4 <- y > quant14[2]
      t((sum(weights[iq4] * y[iq4])/sum(weights[iq4]))/
          (sum(weights[iq1] * y[iq1])/sum(weights[iq1])))
      },
    quants = function(y, pov_line) {t(quantile(y, probs = c(.10,.25, .5, .75, .9)))}
  )
  
  indicator_names <- c("Mean",
                      "Head_Count",
                      "Poverty_Gap",
                      "Gini",
                      "Quintile_Share",
                      "Quantile_10",
                      "Quantile_25",
                      "Median",
                      "Quantile_75",
                      "Quantile_90"
                      )


  if(!is.null(custom_indicator) && length(custom_indicator) > 0){
    indicator_list = c(indicator_list, custom_indicator)
    indicator_names = c(indicator_names, names(custom_indicator))
  }

  if(is.null(pov_line)){
    pov_line <- 0.6*median(smp_data[[paste(fixed[2])]])
    cat("The poverty line for the HCR and the PG is automatically set to 60% of 
        the median of the dependent variable and equals",pov_line, "\n")
  } else {
    pov_line <- pov_line
  }

  return(list(pop_data         = pop_data,
              pop_domains_vec  = pop_domains_vec,
              smp_data         = smp_data,
              smp_domains_vec  = smp_domains_vec,
              smp_domains      = smp_domains,
              N_pop            = N_pop,
              N_smp            = N_smp,
              N_unobs          = N_unobs,
              N_dom_pop        = N_dom_pop,
              N_dom_smp        = N_dom_smp,
              N_dom_unobs      = N_dom_unobs,
              n_pop            = n_pop,
              n_smp            = n_smp,
              obs_dom          = obs_dom,
              dist_obs_dom     = dist_obs_dom,
              indicator_list   = indicator_list,
              indicator_names  = indicator_names,
              pov_line         = pov_line
              )
         )
}



