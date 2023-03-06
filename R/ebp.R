#' Empirical Best Prediction for Disaggregated Indicators
#'
#' Function \code{ebp} estimates indicators using the Empirical Best Prediction
#' approach by \cite{Molina and Rao (2010)}. Point predictions of indicators
#' are obtained by Monte-Carlo approximations. Additionally, mean squared error
#' (MSE) estimation can be conducted by using a parametric bootstrap approach
#' (see also \cite{Gonzalez-Manteiga et al. (2008)}). The unit-level model of
#' \cite{Battese, Harter and Fuller (1988)} is fitted by the restricted maximum
#' likelihood (REML) method and one of
#' five different transformation types for the dependent variable can be chosen.
#' This approach can be extended to data under informative sampling using
#' weights and is based on \cite{Guadarrama et al. (2018)}. Model  estimation
#' combines the uni-level model of \cite{Battese, Harter and Fuller (1988)} and
#' the approach of \cite{You and Rao (2002)} using survey weights. At the
#' moment, only the log-transformation is supported for this method.
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
#' @param threshold a number defining a threshold. Alternatively, a threshold
#' may be defined as a \code{function} of \code{y} returning a numeric value.
#' Such a function will be evaluated once for the point estimation and in each
#' iteration of the parametric bootstrap. A threshold is needed for calculation
#' e.g. of head count ratios and poverty gaps. The  argument defaults to
#' \code{NULL}. In this case, the threshold is set to 60\% of the median of the
#' variable that is selected as dependent variable similary to the
#' at-risk-of-poverty rate used in the EU (see also
#' \cite{Social Protection  Committee 2001}). However, any desired threshold can
#' be chosen.
#' @param transformation a character string. Five different transformation
#' types for the dependent variable can be chosen (i) no transformation ("no");
#' (ii) log transformation ("log"); (iii) Box-Cox transformation ("box.cox");
#' (iv) Dual transformation ("dual"); (v) Log-Shift transformation
#' ("log.shift"). Defaults to \code{"box.cox"}.
#' @param interval a string equal to 'default' or a numeric vector containing a
#' lower and upper limit determining an interval for the estimation of the
#' optimal parameter. The interval is passed to function
#' \code{\link[stats]{optimize}} for the optimization. Defaults to 'default'
#' which equals c(-1,2) for Box-Cox, c(0,2) for Dual and an interval based on
#' the range of y for Log-Shift transformation. If the convergence fails, it is
#' often advisable to choose a smaller more suitable interval. For right skewed
#' distributions, the negative values may be excluded, also values larger than
#' 1 are seldom observed.
#' @param L a number determining the number of Monte-Carlo simulations that
#' must be at least 1. Defaults to 50. For practical applications, values
#' larger than 200 are recommended (see also
#' \cite{Molina, I. and Rao, J.N.K. (2010)}).
#' @param MSE if \code{TRUE}, MSE estimates using a parametric bootstrap
#' approach are calculated (see also \cite{Gonzalez-Manteiga et al. (2008)}).
#' Defaults to \code{FALSE}.
#' @param B a number determining the number of bootstrap populations in the
#' parametric bootstrap approach (see also
#' \cite{Gonzalez-Manteiga et al. (2008)}) used in the MSE estimation. The
#' number must be greater than 1. Defaults to 50. For practical applications,
#' values larger than 200 are recommended (see also
#' \cite{Molina, I. and Rao, J.N.K. (2010)}).
#' @param seed an integer to set the seed for the random number generator. For
#' the usage of random number generation, see Details. If seed is set to
#' \code{NULL}, seed is chosen randomly. Defaults to \code{123}.
#' @param boot_type character string to choose between different MSE estimation
#' procedures,currently a \code{"parametric"} and a semi-parametric
#' \code{"wild"} bootstrap are possible. Defaults to \code{"parametric"}.
#' @param parallel_mode modus of parallelization, defaults to an automatic
#' selection of a suitable mode, depending on the operating system, if the
#' number of \code{cpus} is chosen higher than 1. For details, see
#' \code{\link[parallelMap]{parallelStart}}.
#' @param cpus number determining the kernels that are used for the
#' parallelization. Defaults to 1. For details, see
#' \code{\link[parallelMap]{parallelStart}}.
#' @param custom_indicator a list of functions containing the indicators to be
#' calculated additionally. Such functions must and must only depend on the
#' target variable \code{y}, \code{pop_weight} and the \code{threshold}.
#' Defaults to \code{NULL}.
#' @param na.rm if \code{TRUE}, observations with \code{NA} values are deleted
#' from the population and sample data. For the EBP procedure complete
#' observations are required. Defaults to \code{FALSE}.
#' @param weights a character string containing the name of a variable that
#' indicates weights in the sample data. If a character string is provided
#' a weighted version of the ebp will be used. The variable has to be numeric.
#' Defaults to \code{NULL}.
#' @param pop_weights a character string containing the name of a variable that
#' indicates population weights in the populatation data. If a character string
#' is provided weighted indicators are estimated using population weights.
#' The variable has to be numeric. Defaults to \code{NULL}.
#' @param aggregate_to a character string containing the name of a variable from
#' population data that indicates the target domain level for which the
#' results are to be displayed. The variable can be numeric or a factor.
#' Defaults to \code{NULL}.
#' @return An object of class "ebp", "emdi" that provides estimators for
#' regional disaggregated indicators and optionally corresponding MSE estimates.
#' Several generic functions have methods for the returned object. For a full
#' list and descriptions of the components of objects of class "emdi",
#' see \code{\link{emdiObject}}.
#' @details For Monte-Carlo approximations and in the parametric bootstrap
#' approach random number generation is used. Thus, a seed is set by the
#' argument \code{seed}. \cr \cr
#' The set of predefined indicators includes the mean, median, four further
#' quantiles (10\%, 25\%, 75\% and 90\%), head count ratio, poverty gap, Gini
#' coefficient and the quintile share ratio. \cr \cr
#' Since the sample observations often cannot be identified in practical
#' applications, a modified approach by Guadarrama et al. (2016) called census
#' EBP is implemented for the point estimation. For the MSE estimation, the
#' bootstrap sample is not extracted from the superpopulation, but generated by
#' the estimated model parameters. The lower the ratio between the sample and
#' the population size, the closer are the results to the proposed approach by
#' Molina and Rao (2010).
#' @references
#' Battese, G.E., Harter, R.M. and Fuller, W.A. (1988). An Error-Components
#' Model for Predictions of County Crop Areas Using Survey and Satellite Data.
#' Journal of the American Statistical Association, Vol.83, No. 401,
#' 28-36.\cr \cr
#' Gonzalez-Manteiga, W. et al. (2008). Bootstrap mean squared error of
#' a small-area EBLUP. Journal of Statistical Computation and Simulation,
#' 78:5, 443-462. \cr \cr
#' Guadarrama, M., Molina, I. and Rao, J.N.K. (2016). A comparison of small area
#' estimation methods for poverty mapping. Joint Issue: Statistics in Transition
#' New Series Survey Methodology, Vol.17, No. 1, 41–66. \cr \cr
#' Guadarrama, M., Molina, I. and Rao, J.N.K. (2018). Small area estimation of
#' general parameters under complex sampling designs. Computational Statistics &
#' Data Analysis, Vol. 121, 20-40. \cr \cr
#' Kreutzmann, A., Pannier, S., Rojas-Perilla, N., Schmid, T., Templ, M.
#' and Tzavidis, N. (2019). The R Package emdi for Estimating and
#' Mapping Regionally Disaggregated Indicators, Journal of Statistical Software,
#' Vol. 91, No. 7, 1--33, <doi:10.18637/jss.v091.i07> \cr \cr
#' Molina, I. and Rao, J.N.K. (2010). Small area estimation of poverty
#' indicators. The Canadian Journal of Statistics, Vol. 38, No.3,
#' 369-385. \cr \cr
#' Social Protection Committee (2001). Report on indicators in the field of
#' poverty and social exclusions, Technical Report, European Union.
#' You, Y., Rao, J.N.K. (2002).  A pseudo-empirical best linear unbiased
#' prediction approach to small area estimation using survey weights. The
#' Canadian Journal of Statistics. Vol. 30, No. 3, 431–439.
#' @seealso \code{\link{emdiObject}}, \code{\link[nlme]{lme}},
#' \code{\link{estimators.emdi}},  \code{\link{plot.emdi}},
#' \code{\link{emdi_summaries}}
#' @examples
#' \donttest{
#' # Loading data - population and sample data
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'
#' # Example 1: With default setting but na.rm=TRUE
#' emdi_model <- ebp(
#'   fixed = eqIncome ~ gender + eqsize + cash + self_empl +
#'     unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow +
#'     house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#'   pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#'   na.rm = TRUE
#' )
#'
#'
#' # Example 2: With MSE, two additional indicators and function as threshold -
#' # Please note that the example runs for several minutes. For a short check
#' # change L and B to lower values.
#' emdi_model <- ebp(
#'   fixed = eqIncome ~ gender + eqsize + cash +
#'     self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent +
#'     fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#'   pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#'   threshold = function(y) {
#'     0.6 * median(y)
#'   }, transformation = "log",
#'   L = 50, MSE = TRUE, boot_type = "wild", B = 50, custom_indicator =
#'     list(
#'       my_max = function(y) {
#'         max(y)
#'       },
#'       my_min = function(y) {
#'         min(y)
#'       }
#'     ), na.rm = TRUE, cpus = 1
#' )
#'
#' # Example 3: With default setting but na.rm=TRUE under informative sampling.
#' emdi_model <- ebp(
#'   fixed = eqIncome ~ gender + eqsize + cash + self_empl +
#'     unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow +
#'     house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#'   pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#'   weights = "weight", transformation = "log", na.rm = TRUE
#' )
#'
#' # Example 4: With default setting and random effect on the district level
#' # while the output is at state level
#' emdi_model <- ebp(
#'   fixed = eqIncome ~ gender + eqsize + cash + self_empl +
#'     unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow +
#'     house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#'   pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#'   na.rm = TRUE, aggregate_to = "state"
#' )
#'
#' # Example 5: With default setting using pop_weights to get weighted
#' # indicators according to equivalized household size
#' emdi_model <- ebp(
#'   fixed = eqIncome ~ gender + eqsize + cash + self_empl +
#'     unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow +
#'     house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#'   pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#'   na.rm = TRUE, pop_weight = "eqsize"
#' )
#' }
#' @export
#' @importFrom nlme fixed.effects VarCorr lme random.effects
#' @importFrom parallelMap parallelStop parallelLapply parallelLibrary
#' @importFrom parallel detectCores clusterSetRNGStream
#' @importFrom stats as.formula dnorm lm median model.matrix na.omit optimize
#' qnorm quantile residuals rnorm sd
#' @importFrom utils flush.console
#' @importFrom stats fitted

ebp <- function(fixed,
                pop_data,
                pop_domains,
                smp_data,
                smp_domains,
                L = 50,
                threshold = NULL,
                transformation = "box.cox",
                interval = "default",
                MSE = FALSE,
                B = 50,
                seed = 123,
                boot_type = "parametric",
                parallel_mode = ifelse(grepl("windows", .Platform$OS.type),
                  "socket", "multicore"
                ),
                cpus = 1,
                custom_indicator = NULL,
                na.rm = FALSE,
                weights = NULL,
                pop_weights = NULL,
                aggregate_to = NULL
                ) {
  ebp_check1(
    fixed = fixed, pop_data = pop_data, pop_domains = pop_domains,
    smp_data = smp_data, smp_domains = smp_domains, L = L
  )

  ebp_check2(
    threshold = threshold, transformation = transformation,
    interval = interval, MSE = MSE, boot_type = boot_type, B = B,
    custom_indicator = custom_indicator, cpus = cpus, seed = seed,
    na.rm = na.rm, weights = weights, pop_weights = pop_weights
  )

  # Save function call ---------------------------------------------------------

  call <- match.call()
  if (inherits(call$fixed, "name")) {
    call$fixed <- fixed
  }
  # Data manipulation and notational framework ---------------------------------
  if (!is.null(seed)) {
    if (cpus > 1 && parallel_mode != "socket") {
      RNG_kind <- RNGkind()
      set.seed(seed, kind = "L'Ecuyer")
    } else {
      set.seed(seed)
    }
  }

  # The function framework_ebp can be found in script framework_ebp.R
  framework <- framework_ebp(
    pop_data = pop_data,
    pop_domains = pop_domains,
    smp_data = smp_data,
    smp_domains = smp_domains,
    aggregate_to = aggregate_to,
    custom_indicator = custom_indicator,
    fixed = fixed,
    threshold = threshold,
    na.rm = na.rm,
    weights = weights,
    pop_weights = pop_weights
  )



  # Point Estimation -----------------------------------------------------------
  # The function point_estim can be found in script point_estimation.R
  point_estim <- point_estim(
    framework = framework,
    fixed = fixed,
    transformation = transformation,
    interval = interval,
    L = L,
    keep_data = TRUE
  )



  # MSE Estimation -------------------------------------------------------------

  if (MSE == TRUE) {

    # The function parametric_bootstrap can be found in script mse_estimation.R
    mse_estimates <- parametric_bootstrap(
      framework = framework,
      point_estim = point_estim,
      fixed = fixed,
      transformation = transformation,
      interval = interval,
      L = L,
      B = B,
      boot_type = boot_type,
      parallel_mode = parallel_mode,
      cpus = cpus
    )



    ebp_out <- list(
      ind = point_estim$ind,
      MSE = mse_estimates,
      transform_param = point_estim[c(
        "optimal_lambda",
        "shift_par"
      )],
      model = point_estim$model,
      framework = framework[c(
        "N_dom_unobs",
        "N_dom_smp",
        "N_smp",
        "N_pop",
        "smp_domains",
        "smp_data",
        "smp_domains_vec",
        "pop_domains_vec"
      )],
      transformation = transformation,
      method = "reml",
      fixed = fixed,
      call = call,
      successful_bootstraps = NULL
    )
  } else {
    ebp_out <- list(
      ind = point_estim$ind,
      MSE = NULL,
      transform_param = point_estim[c(
        "optimal_lambda",
        "shift_par"
      )],
      model = point_estim$model,
      framework = framework[c(
        "N_dom_unobs",
        "N_dom_smp",
        "N_smp",
        "N_pop",
        "smp_domains",
        "smp_data",
        "smp_domains_vec",
        "pop_domains_vec",
        "response"
      )],
      transformation = transformation,
      method = "reml",
      fixed = fixed,
      call = call,
      successful_bootstraps = NULL
    )
  }

  if (cpus > 1 && parallel_mode != "socket") {
    RNGkind(RNG_kind[1]) # restoring RNG type
  }
  class(ebp_out) <- c("ebp", "emdi")
  return(ebp_out)
}
