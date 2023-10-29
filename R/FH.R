#' Standard and Extended Fay-Herriot Models for Disaggregated Indicators
#'
#' Function \code{fh} estimates indicators using the Fay-Herriot approach by
#' \cite{Fay and Herriot (1979)}. Empirical best linear unbiased predictors
#' (EBLUPs) and mean squared error (MSE) estimates are provided. Additionally,
#' different extensions of the standard Fay-Herriot model are available: \cr
#' Adjusted estimation methods for the variance of the random effects (see
#' \cite{Li and Lahiri (2010)} and \cite{Yoshimori and Lahiri (2014)}) are
#' offered. Log and arcsin transformation for the dependent variable and two
#' types of backtransformation can be chosen - a crude version and the one
#' introduced by \cite{Slud and Maiti (2006)} for log transformed  variables
#' and a naive and bias-corrected version following \cite{Hadam et al. (2020)}
#' for arcsin transformed variables. A spatial extension to the Fay-Herriot
#' model following \cite{Petrucci and Salvati (2006)} is also included. In
#' addition, it is possible to estimate a robust version of the standard and of
#' the spatial model (see \cite{Warnholz (2016)}). Finally, a Fay-Herriot model
#' can be estimated when the auxiliary information is measured with error
#' following \cite{Ybarra and Lohr (2008)}.
#'
#' @param fixed a two-sided linear formula object describing the
#' fixed-effects part of the linear mixed regression model with the
#' dependent variable on the left of a ~ operator and the explanatory
#' variables on the right, separated by + operators.
#' @param vardir a character string indicating the name of the variable
#' containing the domain-specific sampling variances of the direct estimates
#' that are included in \cr \code{combined_data}.
#' @param combined_data a data set containing all the input variables that are
#' needed for the estimation of the Fay-Herriot model: the direct estimates,
#' the sampling variances, the explanatory variables and the domains. In
#' addition, the effective sample size needs to be included, if the arcsin
#' transformation is chosen.
#' @param domains a character string indicating the domain variable that is
#' included in \code{combined_data}. If \code{NULL}, the domains are numbered
#' consecutively.
#' @param method a character string describing the method for the estimation of
#' the variance of the random effects. Methods that can be chosen
#' (i) restricted maximum likelihood (REML) method ("\code{reml}"),
#' (ii) maximum likelihood method ("\code{ml}"),
#' (iii) adjusted REML following \cite{Li and Lahiri (2010)} ("\code{amrl}"),
#' (iv) adjusted ML following \cite{Li and Lahiri (2010)} ("\code{ampl}"),
#' (v) adjusted REML following \cite{Yoshimori and Lahiri (2014)}
#' ("\code{amrl_yl}"), (vi) adjusted ML following
#' \cite{Yoshimori and Lahiri (2014)} ("\code{ampl_yl}"), (vii) robustified
#' maximum likelihood with robust EBLUP prediction following
#' \cite{Warnholz (2017)} ("\code{reblup}"), (viii) robustified maximum
#' likelihood with robust and bias-corrected EBLUP prediction following
#' \cite{Warnholz (2017)} ("\code{reblupbc}"), (ix) estimation of the
#' measurement error model of \cite{Ybarra and Lohr (2008)} ("\code{me}").
#' Defaults to "\code{reml}".
#' @param interval optional argument, if method "\code{reml}" and
#' "\code{ml}" in combination with \code{correlation} equals "\code{no}"
#' is chosen or for the adjusted variance estimation methods "\code{amrl}",
#' "\code{amrl_yl}", "\code{ampl}" and "\code{ampl_yl}". Is internally
#' set to \code{c(0, var(direct estimates))}. If a transformation is
#' applied, the interval is internally set to
#' \code{c(0, var(transformed(direct estimates)))}. If desired, \code{interval}
#' can be specified to a numeric vector containing a lower and upper limit for
#' the estimation of the variance of the random effects. Defaults to
#' \code{NULL}.
#' @param k numeric tuning constant. Required argument when the robust version
#' of the standard or spatial Fay-Herriot model is chosen. Defaults to
#' \code{1.345}. For detailed information, please refer to
#' \cite{Warnholz (2016)}.
#' @param mult_constant numeric multiplier constant used in the bias corrected
#' version of the robust estimation methods. Required argument when the robust
#' version of the standard or spatial Fay-Herriot model is chosen. Default is to
#' make no correction for realizations of direct estimator within
#' \code{mult_constant = 1} times the standard deviation of direct estimator.
#' For detailed information, please refer to \cite{Warnholz (2016)}.
#' @param transformation a character that determines the type of transformation
#' of the dependent variable and of the sampling variances. Methods that can be
#' chosen (i) no transformation ("\code{no}"), (ii) log transformation
#' ("\code{log}") of the dependent variable and of the sampling variances,
#' (iii) arcsin transformation ("\code{arcsin}") of the dependent variable and
#' of the sampling variances following. Defaults to "\code{no}". For more
#' information, how the direct estimate and its variance are transformed, please
#' see the package vignette "A Framework for Producing Small Area Estimates
#' Based on Area-Level Models in R".
#' @param backtransformation a character that determines the type of
#' backtransformation of the EBLUPs and MSE estimates. Required argument when a
#' transformation is chosen. Available methods are (i) crude bias-correction
#' following \cite{Rao (2015)} when the log transformation is chosen
#' ("\code{bc_crude}"), (ii) bias-correction following
#' \cite{Slud and Maiti (2006)} when the log transformations is chosen
#' ("\code{bc_sm}"), (iii) naive back transformation when the arcsin
#' transformation is chosen ("\code{naive}"), (iii) bias-corrected back
#' transformation following \cite{Hadam et al. (2020)} when the arcsin
#' transformation is chosen ("\code{bc}"). Defaults to \code{NULL}.
#' @param eff_smpsize a character string indicating the name of the variable
#' containing the effective sample sizes that are included in
#' \code{combined_data}. Required argument when the arcsin transformation is
#' chosen. Defaults to \code{NULL}.
#' @param correlation a character determining the correlation structure of the
#' random effects. Possible correlations are
#' (i) no correlation ("\code{no}"),
#' (ii) incorporation of a spatial correlation in the random effects
#' ("\code{spatial}"). Defaults to "\code{no}".
#' @param corMatrix matrix or data frame with dimensions number of areas times
#' number of areas containing the row-standardized proximities between the
#' domains. Values must lie between \code{0} and \code{1}. The columns and rows
#' must be sorted like the domains in \code{fixed}. For an example how to
#' create the proximity matrix, please refer to the vignette. Required argument
#' when the correlation is set to "\code{spatial}". Defaults to \code{NULL}.
#' @param Ci array with dimension number of estimated regression coefficients
#' times number of estimated regression coefficients times number of areas
#' containing the variance-covariance matrix of the explanatory variables for
#' each area. For an example of how to create the array, please refer to the
#' vignette. Required argument within the Ybarra-Lohr model
#' (\code{method = me}). Defaults to \code{NULL}.
#' @param tol a number determining the tolerance value for the estimation of the
#' variance of the random effects. Required argument when method "\code{reml}"
#' and "\code{ml}" in combination with \code{correlation =}"\code{spatial}" are
#' chosen or for the variance estimation methods "\code{reblup}",
#' "\code{reblupbc}" and "\code{me}". Defaults to 0.0001.
#' @param maxit a number determining the maximum number of iterations for the
#' estimation of the variance of the random effects. Required argument when
#' method "\code{reml}" and  "\code{ml}" in combination with \code{correlation}
#' equals "\code{spatial}" is chosen or for the variance estimation methods
#' "\code{reblup}", "\code{reblupbc}" and "\code{me}". Defaults to 100.
#' @param MSE if \code{TRUE}, MSE estimates are calculated. Defaults
#' to \code{FALSE}.
#' @param mse_type a character string determining the estimation method of the
#' MSE. Methods that can be chosen
#' (i) analytical MSE depending on the estimation method of the variance of the
#' random effect ("\code{analytical}"),
#' (ii) a jackknife MSE ("\code{jackknife}"),
#' (iii) a weighted jackknife MSE ("\code{weighted_jackknife}"),
#' (iv) bootstrap ("\code{boot}"),
#' (v)  approximation of the MSE based on a pseudo linearisation
#' ("\code{pseudo}"),
#' (vi) naive parametric bootstrap for the spatial Fay-Herriot model
#' ("\code{spatialparboot}"),
#' (vii) bias corrected parametric bootstrap for the spatial Fay-Herriot model
#' ("\code{spatialparbootbc}"),
#' (viii) naive nonparametric bootstrap for the spatial Fay-Herriot model
#' ("\code{spatialnonparboot}"),
#' (ix) bias corrected nonparametric bootstrap for the spatial Fay-Herriot model
#' ("\code{spatialnonparbootbc}").
#' Options (ii)-(iv) are of interest when the arcsin transformation is selected.
#' Option (ii) must be chosen when an Ybarra-Lohr model is selected
#' (\code{method = me}). Options (iv) and (v) are the MSE options for the
#' robust extensions of the Fay-Herriot model. For an extensive overview of the
#' possible MSE options, please refer to the vignette. Required argument when
#' \code{MSE = TRUE}. Defaults to "\code{analytical}".
#' @param B either a single number or a numeric vector with two elements. The
#' single number or the first element defines the number of bootstrap iterations
#' when a bootstrap MSE estimator is chosen. When the standard FH
#' model is applied and the information criteria by Marhuenda et al. (2014)
#' should be computed, the second element of \code{B} is needed and must be
#' greater than 1. Defaults to c(50,0). For practical applications, values
#' larger than 200 are recommended.
#' @param seed an integer to set the seed for the random number generator. For
#' the usage of random number generation see details. If seed is set to
#' \code{NULL}, seed is chosen randomly. Defaults to \code{123}.
#' @return An object of class "fh", "emdi" that provides estimators
#' for regional disaggregated indicators like means and ratios and optionally
#' corresponding MSE estimates. Several generic functions have methods for the
#' returned object. For a full list and descriptions of the components of
#' objects of class "emdi", see \code{\link{emdiObject}}.
#' @details In the bootstrap approaches, random number generation is used. Thus,
#' a seed is set by the argument \code{seed}. \cr \cr
#' Out-of-sample EBLUPs are available for all area-level models except for the
#' \code{bc_sm} backtransformation and for the robust models. \cr
#' Out-of-sample MSEs are available for the analytical MSE estimator of the
#' standard Fay-Herriot model with reml and ml variance estimation, the crude
#' backtransformation in case of log transformation and the bootstrap MSE
#' estimator for the arcsin transformation. \cr \cr
#' For a description of how to create the proximity matrix for the
#' spatial Fay-Herriot model, see the package vignette. If the presence
#' of out-of-sample domains, the proximity matrix needs to be
#' subsetted to the in-sample domains.
#' @references
#' Chen S., Lahiri P. (2002), A weighted jackknife MSPE estimator in small-area
#' estimation, "Proceeding of the Section on Survey Research Methods", American
#' Statistical Association, 473 - 477. \cr \cr
#' Datta, G. S. and Lahiri, P. (2000), A unified measure of uncertainty of
#' Statistica Sinica 10(2), 613-627. \cr \cr
#' Fay, R. E. and Herriot, R. A. (1979), Estimates of income for small places:
#' An application of James-Stein procedures to census data, Journal of the
#' American Statistical Association 74(366), 269-277. \cr \cr
#' González-Manteiga, W., Lombardía, M. J., Molina, I., Morales, D. and
#' Santamaría, L. (2008) Analytic and bootstrap approximations of prediction
#' errors under a multivariate Fay-Herriot model. Computational Statistics &
#' Data Analysis, 52, 5242–5252. \cr \cr
#' Hadam, S., Wuerz, N. and Kreutzmann, A.-K. (2020), Estimating
#' regional unemployment with mobile network data for Functional Urban Areas in
#' Germany, Refubium - Freie Universitaet Berlin Repository, 1-28. \cr \cr
#' Jiang, J., Lahiri, P., Wan, S.-M. and Wu, C.-H. (2001), Jackknifing in the
#' Fay–Herriot model with an example. In Proc. Sem. Funding Opportunity in
#' Survey Research, Washington DC: Bureau of Labor Statistics, 75–97. \cr \cr
#' Jiang, J., Lahiri, P.,Wan, S.-M. (2002), A unified jackknife theory for
#' empirical best prediction with M-estimation, Ann. Statist., 30,
#' 1782-810. \cr \cr
#' Li, H. and Lahiri, P. (2010), An adjusted maximum likelihood method for
#' solving small area estimation problems, Journal of Multivariate Analyis 101,
#' 882-902. \cr \cr
#' Marhuenda, Y., Morales, D. and Pardo, M.C. (2014). Information criteria for
#' Fay-Herriot model selection. Computational Statistics and Data Analysis 70,
#' 268-280. \cr \cr
#' Neves, A., Silva, D. and Correa, S. (2013), Small domain estimation for the
#' Brazilian service sector survey, ESTADISTICA 65(185), 13-37. \cr \cr
#' Prasad, N. and Rao, J. (1990), The estimation of the mean squared error of
#' small-area estimation, Journal of the American Statistical
#' Association 85(409), 163-171. \cr \cr
#' Petrucci, A., Salvati, N. (2006), Small Area Estimation for Spatial
#' Correlation in Watershed Erosion Assessment, Journal of Agricultural,
#' Biological and Environmental Statistics, 11(2), 169–182. \cr \cr
#' Rao, J. N. K. (2003), Small Area Estimation, New York: Wiley. \cr \cr
#' Rao, J. N. K. and Molina, I. (2015), Small area estimation,
#' New York: Wiley. \cr \cr
#' Slud, E. and Maiti, T. (2006), Mean-squared error estimation in transformed
#' Fay-Herriot models, Journal of the Royal Statistical Society:Series B 68(2),
#' 239-257.\cr \cr
#' Warnholz, S. (2016), saeRobust: Robust small area estimation.
#' R package. \cr \cr
#' Warnholz, S. (2016b). Small area estimation using robust extensions to area
#' level models. Ph.D. thesis, Freie Universitaet Berlin. \cr \cr
#' Ybarra, L. and Lohr, S. (2008), Small area estimation when auxiliary
#' information is measured with error, Biometrika, 95(4), 919-931.\cr \cr
#' Yoshimori, M. and Lahiri, P. (2014), A new adjusted maximum likelihood method
#' for the Fay-Herriot small area model, Journal of Multivariate Analysis 124,
#' 281-294.
#' @examples
#' \donttest{
#' # Loading data - population and sample data
#' data("eusilcA_popAgg")
#' data("eusilcA_smpAgg")
#'
#' # Combine sample and population data
#' combined_data <- combine_data(
#'   pop_data = eusilcA_popAgg,
#'   pop_domains = "Domain",
#'   smp_data = eusilcA_smpAgg,
#'   smp_domains = "Domain"
#' )
#'
#' # Example 1: Standard Fay-Herriot model and analytical MSE
#' fh_std <- fh(
#'   fixed = Mean ~ cash + self_empl, vardir = "Var_Mean",
#'   combined_data = combined_data, domains = "Domain", method = "ml",
#'   MSE = TRUE
#' )
#'
#' # Example 2: arcsin transformation of the dependent variable
#' fh_arcsin <- fh(
#'   fixed = MTMED ~ cash + age_ben + rent + house_allow,
#'   vardir = "Var_MTMED", combined_data = combined_data, domains = "Domain",
#'   method = "ml", transformation = "arcsin", backtransformation = "bc",
#'   eff_smpsize = "n", MSE = TRUE, mse_type = "boot", B = c(50, 0)
#' )
#'
#' # Example 3: Spatial Fay-Herriot model
#' # Load proximity matrix
#' data("eusilcA_prox")
#' fh_spatial <- fh(
#'   fixed = Mean ~ cash + self_empl, vardir = "Var_Mean",
#'   combined_data = combined_data, domains = "Domain", method = "reml",
#'   correlation = "spatial", corMatrix = eusilcA_prox, MSE = TRUE,
#'   mse_type = "analytical"
#' )
#'
#' # Example 4: Robust Fay-Herriot model
#' fh_robust <- fh(
#'   fixed = Mean ~ cash + self_empl, vardir = "Var_Mean",
#'   combined_data = combined_data, domains = "Domain", method = "reblupbc",
#'   k = 1.345, mult_constant = 1, MSE = TRUE, mse_type = "pseudo"
#' )
#'
#' # Example 5: Ybarra-Lohr model
#' # Create MSE array
#' P <- 1
#' M <- length(eusilcA_smpAgg$Mean)
#' Ci_array <- array(data = 0, dim = c(P + 1, P + 1, M))
#' for (i in 1:M) {
#'   Ci_array[2, 2, i] <- eusilcA_smpAgg$Var_Cash[i]
#' }
#' fh_yl <- fh(
#'   fixed = Mean ~ Cash, vardir = "Var_Mean",
#'   combined_data = eusilcA_smpAgg, domains = "Domain", method = "me",
#'   Ci = Ci_array, MSE = TRUE, mse_type = "jackknife"
#' )
#' }
#' @export
#' @importFrom formula.tools lhs lhs<-
#' @importFrom stats coefficients integrate median model.frame model.matrix
#' @importFrom stats model.response optimize pnorm rnorm terms update
#' @importFrom stats complete.cases var




fh <- function(fixed, vardir, combined_data, domains = NULL, method = "reml",
               interval = NULL, k = 1.345, mult_constant = 1,
               transformation = "no", backtransformation = NULL,
               eff_smpsize = NULL, correlation = "no", corMatrix = NULL,
               Ci = NULL, tol = 0.0001, maxit = 100,
               MSE = FALSE, mse_type = "analytical", B = c(50, 0), seed = 123) {

  # Agrument checking ----------------------------------------------------------
  fh_combinations(
    fixed = fixed, vardir = vardir, combined_data = combined_data,
    domains = domains, method = method, interval = interval,
    k = k, mult_constant = mult_constant,
    transformation = transformation,
    backtransformation = backtransformation,
    eff_smpsize = eff_smpsize, correlation = correlation,
    corMatrix = corMatrix, Ci = Ci, tol = tol, maxit = maxit,
    MSE = MSE, mse_type = mse_type, B = B, seed = seed
  )

  fh_check(
    fixed = fixed, vardir = vardir, combined_data = combined_data,
    domains = domains, method = method, interval = interval, k = k,
    mult_constant = mult_constant, transformation = transformation,
    backtransformation = backtransformation, eff_smpsize = eff_smpsize,
    correlation = correlation, corMatrix = corMatrix,
    Ci = Ci, tol = tol, maxit = maxit, MSE = MSE, mse_type = mse_type,
    B = B, seed = seed
  )






  # Save function call ---------------------------------------------------------
  call <- match.call()

  # Set seed if desired
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Notational framework -------------------------------------------------------
  framework <- framework_FH(
    combined_data = combined_data, fixed = fixed,
    vardir = vardir, domains = domains,
    transformation = transformation,
    eff_smpsize = eff_smpsize,
    correlation = correlation,
    corMatrix = corMatrix, Ci = Ci, tol = tol,
    maxit = maxit
  )


  # Limits for interval
  if (is.null(interval)) {
    upper <- var(framework$direct)
    interval <- c(0, upper)
  }

  # Number of bootstrap iterations
  if (length(B) == 1) {
    B <- c(B, 0)
  }

  if (!(method == "reblup" || method == "reblupbc")) {
    # Estimate sigma u ---------------------------------------------------------
    sigmau2 <- wrapper_estsigmau2(
      framework = framework, method = method,
      interval = interval
    )


    if (method != "me") {
      if (correlation == "no") {
        # Standard EBLUP -------------------------------------------------------
        eblup <- eblup_FH(
          framework = framework, sigmau2 = sigmau2,
          combined_data = combined_data
        )

        Gamma <- data.frame(
          Domain =
            framework$combined_data[[framework$domains]]
        )
        Gamma$Gamma <- NA
        Gamma$Gamma[framework$obs_dom == TRUE] <- as.numeric(eblup$gamma)
        # Criteria for model selection -----------------------------------------
        criteria <- model_select(
          framework = framework, sigmau2 = sigmau2,
          method = method, interval = interval,
          eblup = eblup, B = B[2], vardir = vardir,
          transformation = transformation,
          combined_data = framework$combined_data
        )
      }
      if ((method == "ml" || method == "reml") && correlation == "spatial") {
        # Spatial EBLUP --------------------------------------------------------
        eblup <- eblup_SFH(
          framework = framework, sigmau2 = sigmau2,
          combined_data = combined_data
        )
        # Criteria for model selection -----------------------------------------
        criteria <- model_select(
          framework = framework, sigmau2 = sigmau2,
          method = method, interval = interval,
          eblup = eblup, B = B[2], vardir = vardir,
          transformation = transformation,
          combined_data = framework$combined_data
        )
      }
    } else if (method == "me") {
      # Standard EBLUP ---------------------------------------------------------
      eblup <- eblup_YL(
        framework = framework, sigmau2 = sigmau2,
        combined_data = combined_data
      )

      Gamma <- data.frame(Domain = framework$combined_data[[framework$domains]])
      Gamma$Gamma <- NA
      Gamma$Gamma[framework$obs_dom == TRUE] <- as.numeric(eblup$gamma)
      # Criteria for model selection -----------------------------------------
      criteria <- model_select(
        framework = framework, sigmau2 = sigmau2,
        method = method, interval = interval,
        eblup = eblup, B = B[2], vardir = vardir,
        transformation = transformation,
        combined_data = framework$combined_data
      )
    }



    if (transformation == "no") {

      # Analytical MSE
      if (MSE == TRUE) {
        mse_data <- wrapper_MSE(
          framework = framework,
          combined_data = framework$combined_data,
          sigmau2 = sigmau2, vardir = vardir, Ci = Ci,
          eblup = eblup, transformation = transformation,
          method = method, interval = interval,
          mse_type = mse_type, B = B[1]
        )
        MSE <- mse_data$mse_data
        MSE_method <- mse_data$MSE_method
        if (mse_type == "spatialnonparboot" ||
          mse_type == "spatialnonparbootbc" ||
          mse_type == "spatialparboot" ||
          mse_type == "spatialparbootbc") {
          successful_bootstraps <- mse_data$successful_bootstraps
        }
      } else {
        MSE <- NULL
        MSE_method <- "no mse estimated"
      }


      if (method != "me") {
        if (!is.null(MSE) && (mse_type == "spatialnonparboot" ||
          mse_type == "spatialnonparbootbc" ||
          mse_type == "spatialparboot" ||
          mse_type == "spatialparbootbc")) {
          sigmau2 <- data.frame(
            correlation = sigmau2$rho,
            variance = sigmau2$sigmau2,
            convergence = sigmau2$convergence
          )
          row.names(sigmau2) <- ""

          out <- list(
            ind = eblup$eblup_data,
            MSE = MSE,
            transform_param = NULL,
            model = list(
              coefficients = eblup$coefficients,
              variance = sigmau2,
              random_effects = eblup$random_effects,
              fitted = eblup$fitted,
              real_residuals = eblup$real_res,
              std_real_residuals = eblup$std_real_res,
              model_select = criteria,
              correlation = correlation,
              seed = seed,
              beta_vcov = eblup$beta_vcov
            ),
            framework = framework[c(
              "direct", "vardir", "N_dom_smp",
              "N_dom_unobs", "combined_data",
              "domains"
            )],
            transformation = list(
              transformation = transformation,
              backtransformation =
                backtransformation
            ),
            method = list(
              method = method,
              MSE_method = MSE_method
            ),
            fixed = fixed,
            call = call,
            successful_bootstraps = successful_bootstraps
          )
        } else if (correlation == "spatial") {
          sigmau2 <- data.frame(
            correlation = sigmau2$rho,
            variance = sigmau2$sigmau2,
            convergence = sigmau2$convergence
          )
          row.names(sigmau2) <- ""
          out <- list(
            ind = eblup$eblup_data,
            MSE = MSE,
            transform_param = NULL,
            model = list(
              coefficients = eblup$coefficients,
              variance = sigmau2,
              random_effects = eblup$random_effects,
              fitted = eblup$fitted,
              real_residuals = eblup$real_res,
              std_real_residuals = eblup$std_real_res,
              model_select = criteria,
              correlation = correlation,
              seed = seed,
              beta_vcov = eblup$beta_vcov
            ),
            framework = framework[c(
              "direct", "vardir", "N_dom_smp",
              "N_dom_unobs", "combined_data",
              "domains", "W"
            )],
            transformation = list(
              transformation = transformation,
              backtransformation =
                backtransformation
            ),
            method = list(
              method = method,
              MSE_method = MSE_method
            ),
            fixed = fixed,
            call = call,
            successful_bootstraps = NULL
          )
        } else {
          if ((isTRUE(all.equal(round(sigmau2, 3), interval[1]))) ||
            (isTRUE(all.equal(round(sigmau2, 3), interval[2])))) {
            warning(strwrap(prefix = " ", initial = "",
                            "The estimate of the variance of the random effects
                            falls at the interval limit. It is recommended to
                            choose a larger interval for the estimation of the
                            variance of the random effects (specify interval
                            input argument)."
                            ))
          }
          out <- list(
            ind = eblup$eblup_data,
            MSE = MSE,
            transform_param = NULL,
            model = list(
              coefficients = eblup$coefficients,
              variance = sigmau2,
              random_effects = eblup$random_effects,
              fitted = eblup$fitted,
              real_residuals = eblup$real_res,
              std_real_residuals = eblup$std_real_res,
              gamma = Gamma,
              model_select = criteria,
              correlation = correlation,
              seed = seed,
              beta_vcov = eblup$beta_vcov
            ),
            framework = framework[c(
              "direct", "vardir", "N_dom_smp",
              "N_dom_unobs", "combined_data",
              "domains"
            )],
            transformation = list(
              transformation = transformation,
              backtransformation =
                backtransformation
            ),
            method = list(
              method = method,
              MSE_method = MSE_method
            ),
            fixed = fixed,
            call = call,
            successful_bootstraps = NULL
          )
        }
      } else if (method == "me") {
        out <- list(
          ind = eblup$eblup_data,
          MSE = MSE,
          transform_param = NULL,
          model = list(
            coefficients = eblup$coefficients,
            variance = sigmau2$sigmau_YL,
            random_effects =
              as.matrix(eblup$random_effects),
            fitted = eblup$fitted,
            real_residuals = eblup$real_res,
            std_real_residuals = eblup$std_real_res,
            gamma = Gamma,
            model_select = criteria,
            correlation = correlation,
            beta_vcov = eblup$beta_vcov
          ),
          framework = framework[c(
            "direct", "vardir", "N_dom_smp",
            "N_dom_unobs", "combined_data",
            "domains", "Ci"
          )],
          transformation = list(
            transformation = transformation,
            backtransformation = backtransformation
          ),
          method = list(
            method = method,
            MSE_method = MSE_method
          ),
          fixed = fixed,
          call = call,
          successful_bootstraps = NULL
        )
      }
    } else if (transformation != "no") {
      if ((isTRUE(all.equal(sigmau2, interval[1]))) ||
        (isTRUE(all.equal(sigmau2, interval[2])))) {
        warning(strwrap(prefix = " ", initial = "",
                        "The estimate of the variance of the random effects
                        falls at the interval limit. It is recommended to
                        choose a larger interval for the estimation of the
                        variance of the random effects (specify interval input
                        argument)."))
      }

      # Shrinkage factor
      Gamma <- data.frame(Domain = framework$combined_data[[framework$domains]])
      Gamma$Gamma <- NA
      Gamma$Gamma[framework$obs_dom == TRUE] <- as.numeric(eblup$gamma)

      # Back-transformation
      result_data <- backtransformed(
        framework = framework,
        sigmau2 = sigmau2, vardir = vardir,
        eblup = eblup,
        transformation = transformation,
        backtransformation = backtransformation,
        combined_data = framework$combined_data,
        method = method, interval = interval,
        MSE = MSE,
        mse_type = mse_type,
        B = B[1]
      )

      out <- list(
        ind = result_data$eblup_data,
        MSE = result_data$mse_data,
        transform_param = NULL,
        model = list(
          coefficients = eblup$coefficients,
          variance = sigmau2,
          random_effects =
            as.matrix(eblup$random_effects[, 1]),
          fitted = eblup$fitted,
          real_residuals = eblup$real_res[, 1],
          std_real_residuals = eblup$std_real_res[, 1],
          gamma = Gamma,
          model_select = criteria,
          correlation = correlation,
          seed = seed,
          beta_vcov = eblup$beta_vcov
        ),
        framework = framework[c(
          "direct", "vardir", "N_dom_smp",
          "N_dom_unobs", "combined_data",
          "domains"
        )],
        transformation = list(
          transformation = transformation,
          backtransformation =
            backtransformation
        ),
        method = list(
          method = method,
          MSE_method = result_data$MSE_method
        ),
        fixed = fixed,
        call = call,
        successful_bootstraps = NULL
      )
    }
  } else if (method == "reblup" || method == "reblupbc") {

    # Standard EBLUP -----------------------------------------------------------
    eblup <- eblup_robust(
      framework = framework, vardir = vardir,
      combined_data = combined_data,
      method = method, k = k, mult_constant = mult_constant,
      correlation = correlation, corMatrix = corMatrix
    )

    # MSE ----------------------------------------------------------------------
    if (MSE == TRUE) {
      mse_data <- wrapper_MSE(
        framework = framework,
        combined_data = framework$combined_data,
        vardir = vardir, eblup = eblup,
        mse_type = mse_type, method = method, B = B[1]
      )
      MSE <- mse_data$mse_data
      MSE_method <- mse_data$MSE_method
    } else {
      MSE <- NULL
      MSE_method <- "no mse estimated"
    }

    if (correlation == "spatial") {
      sigmau2 <- data.frame(
        correlation = unname(eblup$variance[1]),
        variance = unname(eblup$variance[2])
      )
      row.names(sigmau2) <- ""
    } else if (correlation == "no") {
      sigmau2 <- unname(eblup$variance[1])
    }

    out <- list(
      ind = eblup$eblup_data,
      MSE = MSE,
      transform_param = NULL,
      model = list(
        coefficients = eblup$coefficients,
        variance = sigmau2,
        random_effects = as.matrix(eblup$random_effects),
        fitted = eblup$fitted,
        real_residuals = as.matrix(eblup$real_res),
        std_real_residuals = as.matrix(eblup$std_real_res),
        correlation = correlation,
        k = k,
        mult_constant = mult_constant,
        seed = seed,
        beta_vcov = eblup$beta_vcov
      ),
      framework = framework[c(
        "direct", "vardir", "N_dom_smp",
        "N_dom_unobs", "combined_data",
        "domains", "W"
      )],
      transformation = list(
        transformation = transformation,
        backtransformation = backtransformation
      ),
      method = list(
        method = method,
        MSE_method = MSE_method
      ),
      fixed = fixed,
      call = call,
      successful_bootstraps = NULL
    )
  }


  class(out) <- c("fh", "emdi")

  return(out)
}
