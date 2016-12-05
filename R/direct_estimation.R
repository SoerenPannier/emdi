#' Direct estimation of disaggregated indicators
#'
#' Function \code{direct} estimates indicators only based on sample information. 
#' The variance is estimated via a naive or calibrated bootstrap. The estimation
#' is adapted from the estimation of direct indicators in package 
#' laeken. 
#'
#' @param y a character indicating the variable that is used for estimating the 
#' indicators.
#' @param smp_data survey data containing the above variable as well as sampling
#' domains, and weights and sort if selected.
#' @param smp_domains a character string containing the name of a variable
#' that indicates domains in the sample data. The variable can be numeric or a
#' factor.
#' @param weights a character containing the name of a variable for 
#' the sampling weights in the sample data. This argument is optional and defaults
#' to \code{NULL}. 
#' @param sort a character containing the name of a variable that should be used
#' to sort the data. Defaults to \code{NULL}.
#' @param pov_line a number defining a poverty line. A poverty line is
#' needed for calculation e.g. of head count ratios and poverty gaps. The 
#' argument defaults to \code{NULL}. In this case the poverty line is set to 60\% 
#' of the median of the variable that is selected as y similary to the 
#' At-risk-of-poverty rate used in the EU (see also \cite{Social Protection Committee 2001}). 
#' However, any desired poverty line can be chosen.
#' @param var if TRUE, estimates for the variance are calcualted using a 
#' naive or calibrated bootstrap. Defaults to \code{FALSE}.
#' @param bootType a character containing the name of the bootstrap specification. 
#' Either a \code{"naive"} or a \code{"calibrate"} bootstrap can be used. See 
#' also \code{\link[laeken]{bootVar}}. Defaults to \code{naive}.
#' @param B a number determining the number of bootstrap populations for the 
#' bootstrap variance. Defaults to \code{NULL}.
#' @param seed an integer to set the seed for the random number generator. Random 
#' number generation is used in the bootstrap approach. If no seed is set, seed
#' is chosen randomly.
#' @param X a matrix including calibration variables if the calibrated bootstrap
#' is chosen. Defaults to NULL.
#' @param totals a numeric vector providing the population totals. Defaults to 
#' \code{NULL}. In this case, the sampling weights are used to calculate the totals.
#' @param na.rm if TRUE, observations with \code{NA} values are deleted from the 
#' sample data. Defaults to \code{FALSE}. 
#' @return An object of class "emdi" that provides direct estimators for regional
#' disaggregated indicators and optionally corresponding variance estimates. Generic
#' functions such as \code{\link{estimators}}, \code{\link{print}}, 
#' \code{\link{plot}}, and \code{\link{summary}} have methods that can be used
#' to obtain further information. See \code{\link{emdiObject}} for descriptions
#' of components of objects of class "emdi".
#' @details The set of predefined indicators includes the mean, median, four further quantiles
#' (10\%, 25\%, 75\% and 90\%), head count ratio, poverty gap, Gini coefficient
#' and the quintile share ratio.
#' @references
#' Alfons, A. and Templ, M. (2013). Estimation of Social Exclusion Indicators
#' from complex Surveys: The R package laeken. Journal of Statistical Software, 
#' 54(15), 1-25.  \cr \cr
#' Social Protection Committee (2001). Report on indicators in the field of
#' poverty and social exclusions, Technical Report, European Union.
#' @seealso \code{\link{emdiObject}}, \code{\link[nlme]{lme}},
#' \code{\link{estimators.emdi}}, \code{\link{print.emdi}}, \code{\link{plot.emdi}},
#' \code{\link{summary.emdi}}
#' @examples
#' # Loading sample data
#' data("eusilcA_smp")
#'
#' # Example without weights and naive bootstrap
#' emdi_direct <- direct(y="eqIncome", smp_data=eusilcA_smp, smp_domains="district", 
#' weights=NULL, pov_line=10859.24, var=TRUE, bootType = "naive", B=50, 
#' seed=123, X = NULL, totals = NULL, na.rm=TRUE)
#' @export



direct <- function(y, 
                   smp_data, 
                   smp_domains = NULL, 
                   weights = NULL, 
                   sort = NULL,
                   pov_line = NULL,
                   var = FALSE, 
                   bootType = "naive", 
                   B = NULL,
                   seed = NULL,
                   X = NULL, 
                   totals = NULL, 
                   na.rm=FALSE)
 {
  
  
  # Save call 
  call <- match.call()
  
  framework_dir <- function(y, smp_data, smp_domains, weights, sort, 
                            pov_line, na.rm){
    

    
    # two versions of y, one vector version and one character version (original)
    # for variance estimation
    # we force sample data, in laeken data can be NULL
    y_vec <- smp_data[, y]
    # Number of households in sample
    N_smp <- length(y_vec)
    
    
    
    if (!is.null(weights)) {
      weights_vec <- smp_data[, weights]
    } else if (is.null(weights)) {
      weights_vec <- rep.int(1, N_smp)
    }
  
    if (!is.null(sort)) {
      sort <- smp_data[, sort]
    }
    byStratum <- !is.null(smp_domains)
    if (byStratum) {
      smp_domains_vec <- as.factor(smp_data[, smp_domains])
      smp_domains_vec <- droplevels(smp_domains_vec)
      rs <- levels(smp_domains_vec)
      # Number of domains in the sample
      N_dom_smp <- length(unique(smp_domains_vec))
      # Number of households in sample per domain
      smp_domains_vec_tmp <- as.numeric(smp_domains_vec)
      n_smp <- as.vector(table(smp_domains_vec_tmp))
    }

    
    if (isTRUE(na.rm)) {
      indices <- !is.na(y)
      y_vec <- y_vec[indices]
      if (!is.null(weights)) 
        weights_vec <- weights_vec[indices]
      if (!is.null(sort)) 
        sort <- sort[indices]
    } else if (any(is.na(y))){
      return(NA)
    } 
    
    
    if(is.null(pov_line)){
      if(is.null(weights)){
        pov_line <- 0.6*median(y_vec)
      } else if (!is.null(weights)){
        pov_line <- 0.6*weightedMedian(y_vec, weights_vec)
      }
       
    }
  

    indicator_names <- c("Mean",
                         "Head_Count",
                         "Poverty_Gap",
                         "Quintile_Share",
                         "Gini",
                         "Quantile_10",
                         "Quantile_25",
                         "Median",
                         "Quantile_75",
                         "Quantile_90"
                         )
    
    return(list(smp_data         = smp_data,
                y                = y, 
                y_vec            = y_vec,
                weights          = weights,
                weights_vec      = weights_vec,
                smp_domains_vec  = smp_domains_vec,
                smp_domains      = smp_domains,
                byStratum        = byStratum,
                rs               = rs,
                N_smp            = N_smp,
                N_dom_smp        = N_dom_smp,
                n_smp            = n_smp,
                #indicator_list   = indicator_list,
                indicator_names  = indicator_names,
                pov_line         = pov_line 
    )
    )
    
  }
  framework <- framework_dir(y=y, smp_data=smp_data, smp_domains=smp_domains, 
                             weights=weights, sort=sort, pov_line=pov_line, 
                             na.rm=na.rm)
  
  # Call single indicators
  HCR <- Head_Count(framework = framework,
                    var=var,
                    bootType = bootType,
                    X = X, 
                    totals = totals, 
                    B=B,  
                    seed=seed, 
                    na.rm=na.rm)
  
  PG <- Poverty_Gap(framework = framework,
                    var=var,
                    bootType = bootType,
                    X = X, 
                    totals = totals, 
                    B=B,  
                    seed=seed, 
                    na.rm=na.rm)
  
  Gini_coeff <- Gini(framework = framework,
                     sort = sort,
                     var=var,
                     bootType = bootType,
                     X = X, 
                     totals = totals, 
                     B=B,  
                     seed=seed, 
                     na.rm=na.rm)
  
  QSR <- Quintile_Share(framework = framework,
                        sort = sort,
                        var=var,
                        bootType = bootType,
                        X = X, 
                        totals = totals, 
                        B=B,  
                        seed=seed, 
                        na.rm=na.rm)
  
  
  ind <- data.frame(Domain = framework$rs, 
                    Head_Count=HCR$valueByStratum[,2], 
                    Poverty_Gap=PG$valueByStratum[,2],
                    Gini = Gini_coeff$valueByStratum[,2], 
                    Quintile_Share = QSR$valueByStratum[,2])
  
  if(var==TRUE){
    MSE <- data.frame(Domain=framework$rs, 
                      Head_Count=HCR$varByStratum[,2],
                      Poverty_Gap=PG$varByStratum[,2],
                      Gini = Gini_coeff$varByStratum[,2], 
                      Quintile_Share = QSR$varByStratum[,2])
  
    direct_out <- list(ind = ind, 
                       MSE = MSE,
                       framework=framework, 
                       call = call)
  } else {
    direct_out <- list(ind = ind, 
                     MSE = NULL,
                     framework=framework,
                     call = call)
  }
  
  class(direct_out) <- c("emdi", "direct")
  return(direct_out)
} 
  
  





  
