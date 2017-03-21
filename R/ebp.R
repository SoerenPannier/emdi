#' Empirical Best Prediction for disaggregated indicators
#'
#' Function \code{ebp} estimates indicators using the Empirical Best Prediction
#' approach by \cite{Molina and Rao (2010)}. Point predictions of indicators are
#' obtained by Monte-Carlo approximations. Additionally, mean squared error (MSE)
#' estimation can be conducted by using a parametric bootstrap approach (see
#' also \cite{Gonzalez-Manteiga et al. (2008)}). The unit level model of
#' \cite{Battese, Harter and Fuller (1988)} is fitted by REML method and one of
#' three different transformation types for the dependent variable can be chosen.
#'
#' @param fixed a two-sided linear formula object describing the
#' fixed-effects part of the nested error linear regression model with the
#' dependent variable on the left of a ~ operator and the explanatory
#' variables on the right, separated by + operators. The argument corresponds
#' to the argument \code{fixed} in function \code{\link[nlme]{lme}}.
#' @param pop_data a data frame that needs to comprise the variables
#' named on the right of the ~ operator in \code{fixed}, i.e. the explanatory
#' variables, and \code{pop_domains}.
#' @param pop_domains a character string containing the name of a variable that
#' indicates domains in the population data. The variable can be numeric or
#' a factor but needs to be of the same class as the variable named in
#' \code{smp_domains}.
#' @param smp_data a data frame that needs to comprise all variables named in
#' \code{fixed} and \code{smp_domains}.
#' @param smp_domains a character string containing the name of a variable
#' that indicates domains in the sample data. The variable can be numeric or a
#' factor but needs to be of the same class as the variable named in
#' \code{pop_domains}.
#' @param threshold a number defining a threshold. Alternatively, a threshold may 
#' be defined as a \code{function} of \code{y} returning a numeric value. Such a 
#' function will be evaluated once for the point estimation and in each iteration 
#' of the parametric bootstrap. A threshold is needed for calculation e.g. of 
#' head count ratios and poverty gaps. The  argument defaults to \code{NULL}. 
#' In this case the threshold is set to 60\% of the median of the variable that 
#' is selected as dependent variable similary to the At-risk-of-poverty rate 
#' used in the EU (see also \cite{Social Protection Committee 2001}). However, 
#' any desired threshold can be chosen.
#' @param transformation a character string. Three different transformation
#' types for the dependent variable can be chosen (i) no transformation ("no");
#' (ii) log transformation ("log"); (iii) Box-Cox transformation ("box.cox").
#' Defaults to \code{"box.cox"}.
#' @param interval a numeric vector containing a lower and upper limit
#' determining an interval for the estimation of the optimal parameter. Defaults
#' to c(-1,2). If the convergence fails, it is often advisable to choose a smaller
#' more suitable interval. For right skewed distributions the negative values may be
#' excluded, also values larger than 1 are seldom observed. 
#' @param L a number determining the number of Monte-Carlo simulations.Defaults
#' to 50.
#' @param MSE if TRUE, MSE estimates using a parametric bootstrap approach
#' are calculated (see also \cite{Gonzalez-Manteiga et al. (2008)}). Defaults
#' to \code{FALSE}.
#' @param B a number determining the number of bootstrap populations in the
#' parametric bootstrap approach (see also \cite{Gonzalez-Manteiga et al. (2008)})
#' used in the MSE estimation. Defaults to 50.
#' @param parallel_mode modus of parallelization, defaults to an automatic selection 
#' of a suitable mode, depending on the operating system, if the number of cpus is 
#' chosen higher than 1. For details see \code{\link[parallelMap]{parallelStart}}
#' @param cpus number determining the kernels that are used for the 
#' parallelization. Defaults to 1. For details see \code{\link[parallelMap]{parallelStart}}
#' @param custom_indicator a list of functions containing the indicators to be
#' calculated additionaly. Such functions must and must only depend on the
#' target variable \code{y} and the threshold \code{threshold}. 
#' Defaults to \code{NULL}.
#' @param na.rm if TRUE, observations with \code{NA} values are deleted from the 
#' population and sample data. For the EBP procedure complete observations  
#' are required. Defaults to \code{FALSE}. 
#' @return An object of class "emdi" that provides estimators for regional
#' disaggregated indicators and optionally corresponding MSE estimates. Generic
#' functions such as \code{\link{estimators}}, \code{\link{print}}, 
#' \code{\link{plot}}, and \code{\link{summary}} have methods that can be used
#' to obtain further information. See \code{\link{emdiObject}} for descriptions
#' of components of objects of class "emdi".
#' @details For Monte-Carlo approximations and in the parametric bootstrap
#' approach random number generation is used. In order to specify seeds use
#' \code{\link{set.seed}}. \cr \cr
#' The set of predefined indicators includes the mean, median, four further quantiles
#' (10\%, 25\%, 75\% and 90\%), head count ratio, poverty gap, Gini coefficient
#' and the quintile share ratio.
#' @references
#' Battese, G.E., Harter, R.M. and Fuller, W.A. (1988). An Error-Components
#' Model for Predictions of County Crop Areas Using Survey and Satellite Data.
#' Journal of the American Statistical Association, Vol.83, No. 401, 28-36. \cr \cr
#' Gonzalez-Manteiga, W. et al. (2008). Bootstrap mean squared error of
#' a small-area EBLUP. Journal of Statistical Computation and Simulation,
#' 78:5, 443-462. \cr \cr
#' Molina, I. and Rao, J.N.K. (2010). Small area estimation of poverty
#' indicators. The Canadian Journal of Statistics, Vol. 38, No.3, 369-385. \cr \cr
#' Social Protection Committee (2001). Report on indicators in the field of
#' poverty and social exclusions, Technical Report, European Union.
#' @seealso \code{\link{emdiObject}}, \code{\link[nlme]{lme}},
#' \code{\link{estimators.emdi}}, \code{\link{print.emdi}}, \code{\link{plot.emdi}},
#' \code{\link{summary.emdi}}
#' @examples
#' \dontrun{
#' # Loading data - population and sample data
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'
#' # Example with default setting but na.rm=TRUE
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl + 
#' unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
#' house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
#' na.rm = TRUE)
#' 
#' 
#' # Example with MSE and two additional indicators
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + 
#' self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
#' fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#' threshold = function(y){0.5 * median(y)}, transformation = "box.cox", 
#' L= 50, MSE = TRUE, B = 50, custom_indicator = 
#' list( my_max = function(y, threshold){max(y)},
#' my_min = function(y, threshold){min(y)}), na.rm = TRUE, cpus = 1)
#' }
#' @export
#' @import nlme
#' @import parallelMap
#' @importFrom parallel detectCores
#' @importFrom Hmisc wtd.quantile 
#' @importFrom stats as.formula dnorm lm median model.matrix na.omit optimize 
#' qnorm quantile residuals rnorm sd
#' @importFrom utils flush.console

ebp <- function(fixed,
                pop_data,
                pop_domains,
                smp_data,
                smp_domains,
                L = 50,
                threshold = NULL,
                transformation = "box.cox",
                interval = c(-1,2),
                MSE = FALSE,
                B = 50,
                parallel_mode = ifelse(grepl("windows",.Platform$OS.type,), 
                                       "socket", "multicore"),
                cpus = 1,
                custom_indicator = NULL, 
                na.rm = FALSE) {


  ebp_check1(fixed = fixed, pop_data = pop_data, pop_domains = pop_domains,
             smp_data = smp_data, smp_domains = smp_domains, L = L) 
  
  ebp_check2(threshold = threshold, transformation = transformation, 
             interval = interval, MSE = MSE, B = B, 
             custom_indicator = custom_indicator, cpus = cpus)
    


  # Save function call ---------------------------------------------------------

  call <- match.call()

  # Data manipulation and notational framework ---------------------------------

  # The function framework_ebp can be found in script framework_ebp.R
  framework <- framework_ebp( pop_data         = pop_data,
                              pop_domains      = pop_domains,
                              smp_data         = smp_data,
                              smp_domains      = smp_domains,
                              custom_indicator = custom_indicator,
                              fixed            = fixed,
                              threshold         = threshold,
                              na.rm            = na.rm
                              )


  
  # Point Estimation -----------------------------------------------------------

  # The function point_estim can be found in script point_estimation.R
  point_estim <- point_estim(framework      = framework,
                                  fixed          = fixed,
                                  transformation = transformation,
                                  interval       = interval,
                                  L              = L,
                                  keep_data      = FALSE
                                  )



  # MSE Estimation -------------------------------------------------------------

  if (MSE == TRUE) {

  # The function parametric_bootstrap can be found in script mse_estimation.R
    mse_estimates <- parametric_bootstrap(framework      = framework,
                                          point_estim    = point_estim,
                                          fixed          = fixed,
                                          transformation = transformation,
                                          interval       = interval,
                                          L              = L,
                                          B              = B,
                                          parallel_mode  = parallel_mode,
                                          cpus           = cpus
                                          )



    ebp_out <- list(ind             = point_estim$ind,
                    MSE             = mse_estimates,
                    transform_param = point_estim[c("optimal_lambda","shift_par")],
                    model           = point_estim$model,
                    framework       = framework[c("N_dom_unobs",
                                                  "N_dom_smp",
                                                  "N_smp",
                                                  "N_pop",
                                                  "smp_domains",
                                                  "smp_data",
                                                  "smp_domains_vec",
                                                  "pop_domains_vec")],
                    transformation  = transformation,
                    method          = "reml",
                    fixed           = fixed,
                    call            = call
                    )
  } else {

    ebp_out <- list(ind             = point_estim$ind,
                    MSE             = NULL,
                    transform_param = point_estim[c("optimal_lambda","shift_par")],
                    model           = point_estim$model,
                    framework       = framework[c("N_dom_unobs",
                                                  "N_dom_smp",
                                                  "N_smp",
                                                  "N_pop",
                                                  "smp_domains",
                                                  "smp_data",
                                                  "smp_domains_vec",
                                                  "pop_domains_vec")],
                    transformation  = transformation,
                    method          = "reml",
                    fixed           = fixed,
                    call            = call
                    )
  }

  class(ebp_out) <- c("emdi", "model")
  return(ebp_out)
}
