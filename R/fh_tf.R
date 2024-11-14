#' Two-fold Fay-Herriot Models for Indicators at a disaggregated regional level
#' @param fixed a two-sided linear formula object describing the
#' fixed-effects part of the linear mixed regression model with the
#' dependent variable on the left of a ~ operator and the explanatory
#' variables on the right, separated by + operators.
#' @param vardir a character string indicating the name of the variable
#' containing the domain-specific sampling variances of the direct estimates
#' that are included in \cr \code{combined_data}.
#' @param combined_data a data set containing all the input variables that are
#' needed for the estimation of the two-fold Fay-Herriot model: the direct estimates,
#' the sampling variances, the explanatory variables, the domains, the subdomains,
#' and the population size. In addition, the effective sample size needs to be
#' included, if the arcsin transformation is chosen.
#' @param domains a character string indicating the domain variable that is
#' included in \code{combined_data}.
#' @param subdomains a character string indicating the subdomain variable that is
#' included in \code{combined_data}.
#' @param subdomsize a character string indicating the variable which includes
#' the population size at subdomain level in \code{combined_data}.
#' @param transformation a character that determines the type of transformation
#' of the dependent variable and of the sampling variances. Methods that can be
#' chosen (i) no transformation ("\code{no}"), (ii) log transformation
#' ("\code{log}") of the dependent variable and of the sampling variances,
#' (iii) arcsin transformation ("\code{arcsin}") of the dependent variable and
#' of the sampling variances following. Defaults to "\code{no}".
#' @param eff_smpsize a character string indicating the name of the variable
#' containing the effective sample sizes that are included in
#' \code{combined_data}. Required argument when the arcsin transformation is
#' chosen. Defaults to \code{NULL}.
#' @param maxit a number determining the maximum number of iterations for the
#' variance components estimation of the random effects. Defaults to 50.
#' @param MSE if \code{TRUE}, MSE estimates are calculated based on parametric
#' bootstrap method. Defaults to \code{FALSE}.
#' @param B a single number which defines the number of bootstrap iterations
#' for the MSE estimation.
#' @param seed an integer to set the seed for the random number generator. For
#' the usage of random number generation see details. Defaults to \code{123}.
#' @return An object of class "fh_tf", "emdi" that provides estimators
#' for regional indicators at a disaggregated level like means and ratios and optionally
#' corresponding MSE estimates.
#' @export
#' @importFrom magic adiag
#' @importFrom formula.tools get.vars


fh_tf <- function(fixed, vardir, combined_data, domains, subdomains,
                    subdomsize, transformation = "no", maxit = 50,
                    eff_smpsize = NA, MSE = FALSE, B = 50, seed = 123){

  fh_tf_check(fixed = fixed, vardir = vardir, combined_data = combined_data,
              domains = domains, subdomains = subdomains,
              transformation = transformation, eff_smpsize = eff_smpsize,
              maxit = maxit, MSE = MSE, B = B, seed = seed)

  call <- match.call()

  fh_tf_fw <- framework_FH_tf(data = combined_data, fixed = fixed, vardir = vardir,
                              domains = domains, subdomains = subdomains,
                              trafo = transformation, eff_n = eff_smpsize,
                              nunits = subdomsize)

  point_estim <- point_fh_tf(fixed = fixed, vardir = vardir, domains = domains,
                          subdomains = subdomains, nunits = subdomsize,
                          trafo = transformation, maxit = maxit,
                          eff_n = eff_smpsize, framework = fh_tf_fw)

  if(MSE == TRUE){
    # MSE estimation based on Gonzalez-Manteiga et al.
    smp_hat_varu <- point_estim$varu
    smp_hat_varv <- point_estim$varv
    smp_hat_vare <- fh_tf_fw$data[, c(domains, subdomains, "vare")]
    smp_beta <- matrix(point_estim$beta$coefficients)
    rownames(smp_beta) <- rownames(point_estim$beta)

    set.seed(seed)
    mse_estim <- mse_fh_tf(fixed = fixed, vardir = vardir, domains = domains,
                           subdomains = subdomains, nunits = subdomsize,
                           trafo = transformation, seed = seed, maxit = maxit,
                           eff_n = eff_smpsize, B = B,
                           smp_hat_varu = smp_hat_varu,
                           smp_hat_varv = smp_hat_varu,
                           smp_hat_vare = smp_hat_vare, smp_beta = smp_beta,
                           framework = fh_tf_fw)
  }

  ind_subdomain<- fh_tf_fw$data[, c(subdomains, as.character(fixed[[2]]))]
  ind_subdomain <- merge(ind_subdomain, point_estim$EBLUP_SubArea,
                         by.x = subdomains, by.y = "Subdomain")
  colnames(ind_subdomain) <- c("Subdomain", "Direct", "FH_TF")
  colnames(point_estim$EBLUP_Area) <- c("Domain", "FH_TF")

  mse_subdomain <- fh_tf_fw$data[, c(subdomains, vardir)]
  mse_subdomain <- merge(mse_subdomain, mse_estim$MSE_SubArea,
                         by.x = subdomains, by.y = "Subdomain")
  colnames(mse_subdomain) <- c("Subdomain", "Direct", "FH_TF")
  colnames(mse_estim$MSE_Area) <- c("Domain", "FH_TF")

  model <- list(coefficients = point_estim$beta,
                variances = c(Subdomain = point_estim$varu, Domain = point_estim$varv),
                random_effects_Domain = point_estim$v_tilde,
                random_effects_Subdomain = point_estim$u_tilde,
                fitted = point_estim$fitted,
                real_residuals = point_estim$resid,
                std_real_residuals = point_estim$std_resid,
                gammas = point_estim$gammas
                )

  if (MSE == FALSE){
    out <- list(ind_Domain = point_estim$EBLUP_Area,
                ind_Subdomain = ind_subdomain,
                model = model,
                framework = fh_tf_fw,
                transformation = transformation,
                fixed = fixed,
                call = call,
                R2 = point_estim$R2)
  } else if(MSE == TRUE){
    out <- list(ind_Domain = point_estim$EBLUP_Area,
                ind_Subdomain = ind_subdomain,
                MSE_Domain = mse_estim$MSE_Area,
                MSE_Subdomain = mse_subdomain,
                model = model,
                framework = fh_tf_fw,
                transformation = transformation,
                fixed = fixed,
                call = call,
                R2 = point_estim$R2)
  }
  class(out) <- c("fh_tf", "emdi")
  return(out)
}
