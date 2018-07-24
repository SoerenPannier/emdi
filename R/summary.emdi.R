#' Summarizes an emdiObject
#'
#' Additional information about the data and model in small area estimation 
#' methods and components of an emdi object are extracted. The returned object 
#' is suitable for printing  with the \code{print.summary.emdi} method.
#' @param object an object of type "emdi", representing point and MSE
#' estimates. Objects differ depending on the estimation method: direct 
#' vs. model-based.
#' @param ... additional arguments that are not used in this method.
#' @return an object of type "summary.emdi" with following
#' components:
#' \item{out_of_smp}{if model-based estimation, number of out-of-sample domains 
#' equivalent to \code{N_dom_unobs} (see \code{\link{emdiObject}}).}
#' \item{in_smp}{number of in-sample domains equivalent to \code{N_dom_smp}
#'              (see \code{\link{emdiObject}}).}
#' \item{size_smp}{number of units in sample equivalent to \code{N_smp}
#' (see \code{\link{emdiObject}}).}
#' \item{size_pop}{if model-based estimation, number of units in population equivalent to \code{N_pop}
#' (see \code{\link{emdiObject}}).}
#' \item{size_dom}{a data frame with rows Sample_domains and Population_domains 
#' (if model-based estimation) representing summary statisitics of the sample 
#' sizes across domains of sample and population data, respectively.}
#' \item{transform}{if model-based estimation, a data frame with columns 
#'                  Transformation, Method, Optimal_lambda and Shift_parameter representing the
#'                  chosen transformation type and estimation method for lambda
#'                  as well as their results.}
#' \item{normality}{if model-based estimation, a data frame with columns Skewness, 
#'                  Kurtosis, Shapiro_W and Shapiro_p where the latter two represent
#'                  the results of a Shapiro-Wilks-Test for normality.
#'                  Rows correspond to Pearson residuals and random effects
#'                  of the nested error regression model. The functions
#'                  \code{\link{skewness}} and \code{\link{kurtosis}} are from
#'                  the package \pkg{moments}. Details for the Shapiro-Wilks-Test
#'                  are provided by \code{\link{shapiro.test}}.}
#' \item{icc}{if model-based estimation, the value of the intraclass coefficient.}
#' \item{coeff_determ}{if model-based estimation, a data frame with colums 
#'                     Marginal_R2 and Conditional_R2 representing two R2 measures
#'                     for linear mixed models from the \pkg{MuMIn} package
#'                     obtained by function \code{\link[MuMIn]{r.squaredGLMM}}.}                  
#' \item{call}{a list containing an image of the function call that produced the
#'             object.}
#' @seealso \code{\link{emdiObject}}, \code{\link{direct}}, \code{\link{ebp}}, 
#' \code{\link[MuMIn]{r.squaredGLMM}}, \code{\link[moments]{skewness}}, 
#' \code{\link[moments]{kurtosis}}, \code{\link[stats]{shapiro.test}}
#' @examples
#' \dontrun{
#' # Loading data - population and sample data
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'   
#' # Example with two additional indicators
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + 
#' self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
#' fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#' threshold = function(y){0.6 * median(y)}, L = 50, MSE = TRUE, B = 50, 
#' custom_indicator = list( my_max = function(y, threshold){max(y)},
#' my_min = function(y, threshold){min(y)}), na.rm = TRUE, cpus = 1)
#' 
#' # Receive first overview
#' summary(emdi_model)
#' }
#' @export
#' @importFrom moments skewness kurtosis
#' @importFrom MuMIn r.squaredGLMM
#'
summary.emdi <- function(object, ...) {
  
  if(!inherits(object, "emdi")){
    stop('First object needs to be of class emdi.')
  }
  
  if(inherits(object, "model")){
    call_emdi <- object$call
    
    N_dom_unobs <- object$framework$N_dom_unobs
    N_dom_smp <-   object$framework$N_dom_smp
    
    smp_size <- object$framework$N_smp
    pop_size <- object$framework$N_pop
    
    smp_size_dom <- summary(as.data.frame(table(object$framework$smp_domains_vec))[,"Freq"])
    pop_size_dom <- summary(as.data.frame(table(object$framework$pop_domains_vec))[,"Freq"])
    sizedom_smp_pop <- rbind(Sample_domains = smp_size_dom,
                             Population_domains = pop_size_dom)
    
    if (object$transformation == "box.cox") {
      transform_method <- data.frame(Transformation  = object$transformation,
                                     Method          = object$method,
                                     Optimal_lambda  = object$transform_param$optimal_lambda,
                                     Shift_parameter = round(object$transform_param$shift_par,3),
                                     row.names       = ""
      )
    } else if (object$transformation == "log") {
      transform_method <- data.frame(Transformation  = object$transformation,
                                     Shift_parameter = round(object$transform_param$shift_par,3),
                                     row.names       = ""
      )
    }
    else if (object$transformation == "no") {
      transform_method <- NULL
      #                      data.frame(Transformation  = object$transformation,
      #                                 Method          = "NULL",
      #                                 Optimal_lambda  = "NULL",
      #                                 Shift_parameter = "NULL",
      #                                 row.names       = ""
      #                                 )
    }
    
    skewness_res <- skewness(residuals(object$model, level=0, type="pearson"))
    kurtosis_res <- kurtosis(residuals(object$model, level=0, type="pearson"))
    
    skewness_ran <- skewness(ranef(object$model)$'(Intercept)')
    kurtosis_ran <- kurtosis(ranef(object$model)$'(Intercept)')
    
    if(length(residuals(object$model, level=0, type="pearson"))>3 & 
       length(residuals(object$model, level=0, type="pearson")) <5000){
      shapiro_p_res <- 
        shapiro.test(residuals(object$model, level = 0, type = "pearson"))[[2]]
      shapiro_W_res <- 
        shapiro.test(residuals(object$model, level = 0, type = "pearson"))[[1]]
    } else {
      warning("Number of observations exceeds 5000 or is lower then 3 and thus the
              Shapiro-Wilk test is not applicable for the residuals.")
      shapiro_p_res <- NA
      shapiro_W_res <- NA
    }
    
    if(length(ranef(object$model)$'(Intercept)') > 3 & 
       length(ranef(object$model)$'(Intercept)') < 5000){
      shapiro_p_ran <- shapiro.test(ranef(object$model)$'(Intercept)')[[2]]
      shapiro_W_ran <- shapiro.test(ranef(object$model)$'(Intercept)')[[1]]
    } else {
      warning("Number of domains exceeds 5000 or is lower then 3 and thus the
              Shapiro-Wilk test is not applicable for the random effects.")
      shapiro_p_ran <- NA
      shapiro_W_ran <- NA
    }
    
    norm <- data.frame(Skewness  = c(skewness_res,skewness_ran),
                       Kurtosis  = c(kurtosis_res, kurtosis_ran),
                       Shapiro_W = c(shapiro_W_res, shapiro_W_ran),
                       Shapiro_p = c(shapiro_p_res, shapiro_p_ran),
                       row.names = c("Error", "Random_effect")
    )
    
    r_squared <- r.squaredGLMM(object$model)
    if (is.matrix(r_squared)) {
      r_marginal <- r_squared["delta", 1]
      r_conditional <- r_squared["delta", 2]
    } else {
      r_marginal <- r_squared[1]
      r_conditional <- r_squared[2]
    }
    icc_mixed <- icc(object$model)
    
    coeff_det <- data.frame(#R2             = r_squared,
      #ICC            = icc_mixed,
      Marginal_R2    = r_marginal,
      Conditional_R2 = r_conditional,
      row.names      = ""
    )
    
    sum_emdi <- list(out_of_smp   = N_dom_unobs,
                     in_smp       = N_dom_smp,
                     size_smp     = smp_size,
                     size_pop     = pop_size,
                     size_dom     = sizedom_smp_pop,
                     transform    = transform_method,
                     normality    = norm,
                     icc          = icc_mixed,
                     coeff_determ = coeff_det,
                     call         = call_emdi
    )
  }
  if(inherits(object, "direct")){
    call_emdi <- object$call
    
    N_dom_smp <-   object$framework$N_dom_smp
    
    smp_size <- object$framework$N_smp
    
    smp_size_tab <- table(object$framework$smp_domains_vec)
    
    smp_size_dom <- 
      rbind(Sample_domains = summary(as.numeric(smp_size_tab)))
    
    sum_emdi <- list(in_smp       = N_dom_smp,
                     size_smp     = smp_size,
                     size_dom     = smp_size_dom,
                     call         = call_emdi,
                     smp_size_tab = smp_size_tab
    )
  }
  
  class(sum_emdi) <- "summary.emdi"
  sum_emdi
}


#' Prints a summary.emdi object
#'
#' The elements described in summary.emdi are printed.
#' @param x an object of type "summary.emdi", generally resulting
#' from applying summary to an object of type "emdi"
#' @param ... optional arguments passed to print.default; see the documentation on
#' that method functions.
#' @seealso
#' \code{\link{summary.emdi}}
#' @export

print.summary.emdi <- function(x,...) {
  if(!is.null(x$size_pop)){
    cat("Empirical Best Prediction\n")
    cat("\n")
    cat("Call:\n ")
    print(x$call)
    cat("\n")
    cat("Out-of-sample domains: ", x$out_of_smp, "\n")
    cat("In-sample domains: ", x$in_smp, "\n")
    cat("\n")
    cat("Sample sizes:\n")
    cat("Units in sample: ", x$size_smp, "\n")
    cat("Units in population: ", x$size_pop, "\n")
    print(x$size_dom)
    cat("\n")
    cat("Explanatory measures:\n")
    print(x$coeff_determ)
    cat("\n")
    cat("Residual diagnostics:\n")
    print(x$normality)
    cat("\n")
    cat("ICC: ", x$icc, "\n")
    cat("\n")
    if(is.null(x$transform)){
      cat("Transformation: No transformation \n")
    } else {
      cat("Transformation:\n")
      print(x$transform)
    }
  } else {
    cat("Direct estimation\n")
    cat("\n")
    cat("Call:\n ")
    print(x$call)
    cat("\n")
    cat("In-sample domains: ", x$in_smp, "\n")
    cat("\n")
    cat("Sample sizes:\n")
    cat("Units in sample: ", x$size_smp, "\n")
    print(x$size_dom)
    cat("\n")
    cat("Units in each Domain:")
    print(x$smp_size_tab)
  }
}

# Auxiliary functions (taken from Results_Mexico_neueEBP.R)---------------------

#  ICC

icc <- function(model){
  u <- as.numeric(VarCorr(model)[1,1])
  e <- model$sigma^2
  u / (u + e)
}



