#' Step function
#'
#' This function selects a Fay-Herriot model by different criteria in a stepwise 
#' algorithm.
#'
#' @param object an object of type "model","fh" that contains the chosen 
#' information criteria.
#' @param scope formula or a list including two formulas (\code{lower} and 
#' \code{upper}) specifying the models considered in the step function. 
#' Defaults to \code{NULL}.
#' @param criteria a character string describing the model selection criterion. 
#' Criteria that can be chosen are "\code{AIC}", "\code{AICc}", "\code{AICb1}", 
#' "\code{AICb2}", "\code{BIC}", "\code{KIC}", "\code{KICc}", "\code{KICb1}", 
#' or "\code{KICb2}". Defaults to "\code{AIC}".
#' @param direction a character string describing the direction of stepwise 
#' algorithm. Directions that can be chosen are "\code{both}", "\code{backward}" 
#' or "\code{forward}". Defaults to "\code{both}". If no \code{scope} argument is 
#' provided, the default is "\code{backward}".
#' @param trace if \code{TRUE}, information about the single steps is 
#' provided during the stepwise procedure. Defaults to \code{TRUE}.
#' @param steps a number determining the maximum number of steps. Defaults to 1000.
#' @return Information about the resulting "best" model due to the chosen 
#' information criterion: 
#' \item{\code{call}}{a list containing an image of the function call that
#'                    produced the object.}
#' \item{\code{coefficients}}{data frame containing the estimated regression 
#' coefficients, the standard errors and the \code{t}- and \code{p}-values of 
#' the explanatory variables.} 
#' @export
#' @importFrom stats factor.scope 

step_fh <- function (object, scope = NULL, criteria = "AIC", 
                     direction = "both", trace = TRUE,
                     steps = 1000) 
{

  step_fh_check(object = object, scope = scope, criteria = criteria,
                direction = direction, trace = trace, steps = steps)
  
  cut.string <- function(string) {
    if (length(string) > 1L) 
      string[-1L] <- paste0("\n", string[-1L])
    string
  }
  step.results <- function(models, fit, object) { 
    change <- sapply(models, "[[", "change")
    rdf <- sapply(models, "[[", "df.resid")
    ddf <- c(NA, diff(rdf))
    infcriteria <- sapply(models, "[[", "criteria")
    heading <- c("Stepwise Model Path \nAnalysis of Deviance Table", 
                 "\nInitial Model:", deparse(object$fixed), "\nFinal Model:", 
                 deparse(fit$fixed), "\n")
    aod <- data.frame(Step = I(change), Df = ddf, criteria = infcriteria, 
                         check.names = FALSE)  
    attr(aod, "heading") <- heading
    fit$anova <- aod
    list(Call = fit$call,
    Coefficients = fit$model$coefficients)
    
  }
  Terms <- terms(object$fixed)
  object$call$formula <- object$formula <- Terms
  md <- missing(direction)
 # direction <- match.arg(direction)
  backward <- direction == "both" | direction == "backward"
  forward <- direction == "both" | direction == "forward"
  if (missing(scope)) {
    fdrop <- numeric()
    fadd <- attr(Terms, "factors")
    if (md) 
      forward <- FALSE
  }
  else {
    if (is.list(scope)) {
      fdrop <- if (!is.null(fdrop <- scope$lower)) 
        attr(terms(update.formula(object$fixed, fdrop)), "factors")
      else numeric()
      fadd <- if (!is.null(fadd <- scope$upper)) 
        attr(terms(update.formula(object$fixed, fadd)), "factors")
    }
    else {
      fadd <- if (!is.null(fadd <- scope)) 
        attr(terms(update.formula(object$fixed, scope)), "factors")
      fdrop <- numeric()
    }
  }
  models <- vector("list", steps)
  n <- object$framework$N_dom_smp
  fit <- object 
  bcriteria <- fit$model$model_select[[criteria]]
    edf <- length(attr(terms(object$fixed), "term.labels")) + 1 
   # if (is.na(bcriteria)) 
     # stop(criteria, "is not defined for this model, so 'step' cannot proceed", sep = " ")
    if (bcriteria == -Inf) 
      stop(criteria, "is -infinity for this model, so 'step' cannot proceed", sep = " ")
    nm <- 1
    if (trace == TRUE) {
      cat("Start: ", criteria, " = ",format(round(bcriteria, 2)), "\n", 
          cut.string(deparse(fit$fixed)), "\n\n", sep = "")
      flush.console()
    }
    models[[nm]] <- list(df.resid = n - 
                           edf,change = "", criteria = bcriteria) 
  
  while (steps > 0) {
    steps <- steps - 1
    infcriteria <- bcriteria
    ffac <- attr(Terms, "factors")
    scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
    aod <- NULL
    change <- NULL
    if (backward && length(scope$drop)) {
      aod <- drop1.fh(fit, criteria = criteria, scope = scope$drop,
                      scale = 0)
      rn <- row.names(aod)
      row.names(aod) <- c(rn[1L], paste("-", rn[-1L]))
      if (any(aod$Df == 0, na.rm = TRUE)) {
        zdf <- aod$Df == 0 & !is.na(aod$Df)
        change <- rev(rownames(aod)[zdf])[1L]
      }
    }
    if (is.null(change)) {
      if (forward && length(scope$add)) {
        aodf <- add1.fh(fit, criteria = criteria,scope = scope$add)
        rn <- row.names(aodf)
        row.names(aodf) <- c(rn[1L], paste("+", rn[-1L]))
        aod <- if (is.null(aod)) 
          aodf
        else rbind(aod, aodf[-1, , drop = FALSE])
      }
      attr(aod, "heading") <- NULL
      nzdf <- if (!is.null(aod$Df)) 
        aod$Df != 0 | is.na(aod$Df)
      aod <- aod[nzdf, ]
      if (is.null(aod) || ncol(aod) == 0) 
        break
      nc <- match("criteria", names(aod)) #"Cp", match(c("AIC"), names(aod)) #"Cp",
      nc <- nc[!is.na(nc)][1L]
      o <- order(aod[, nc])
      names(aod) <- c("df", criteria)
      if (trace == TRUE) 
        print(aod[o, ])
      if (o[1L] == 1) 
        break
      change <- rownames(aod)[o[1L]]
    }
    
    fit$call$fixed <- update(fit$fixed, as.formula(paste("~ . ", change)),
                             evaluate = FALSE) 
    fit$call$MSE <- FALSE
    fit$call$formula <- NULL  
    fit <- eval(fit$call)


    nnew <- fit$framework$N_dom_smp 
    if (all(is.finite(c(n, nnew))) && nnew != n) 
      stop("number of rows in use has changed: remove missing values?")
    Terms <- terms(fit$fixed)
    
    bcriteria <- fit$model$model_select[[criteria]]
    edf <- length(attr(terms(fit$fixed), "term.labels")) + 1 
    if (trace == TRUE) {
      cat("\nStep: ", criteria, " = ", format(round(bcriteria, 2)), "\n", 
          cut.string(deparse(fit$fixed)), "\n\n", sep = "")
      flush.console()
    }
    if (bcriteria >= infcriteria + 1e-07) 
      break
    nm <- nm + 1
    models[[nm]] <- list(df.resid = n - edf,change = "", criteria = bcriteria)
  }
  results <- step.results(models = models[seq(nm)], fit, object) 
  class(results) <- "step_fh"
  results
}

#' Prints step function results
#'
#' The elements described in step_fh are printed.
#' @param x an object of type "step_fh".
#' @param ... further arguments passed to or from other methods.
#' @export

print.step_fh <- function(x, ...)
{
  cat("\n")
  cat("Call:\n ")
  print(x$Call)
  cat("\n")
  cat("Coefficients:\n ")
  print(x$Coefficients)
}

step_fh_check <- function(object, scope, criteria, direction, trace, 
                          steps){
  
  if(!inherits(object, "fh")){
    stop('Object needs to be fh object.')
  }
  if (!(object$method$method == "ml")){
    stop('The variance estimation method of the random effect has to be ml. 
Otherwise the comparison of models based on information criteria would not be valid.')
  }
  if (is.null(criteria) || !(criteria == "AIC" 
                             || criteria == "AICc" 
                             || criteria == "AICb1"
                             || criteria == "AICb2"
                             || criteria == "BIC"
                             || criteria == "KIC"
                             || criteria == "KICc"
                             || criteria == "KICb1"
                             || criteria == "KICb2"))  {
    stop("The nine options for criteria are ''AIC', ''AICc'', ''AICb1'', ''AICb2'',
         ''BIC'', ''KIC'', ''KICc'', ''KICb1'', ''KICb2''.")
  }
  if (is.null(object$model$model_select[[criteria]])){
    stop("The fh object does not contain the chosen criterion. Please set the 
         input argument B of the fh function to a positive number to receive 
         results for all of the information criteria. For some model extensions of 
         the fh model the information criteria are not defined. Check the 
         model_select component of the fh object (objectname$model$model_select). 
         If no criteria are provided, it is not possible to apply the 
         stepwise variable selection algorithm.")
  }
  if (is.null(direction) || !(direction == "forward" 
                              || direction == "backward" 
                              || direction == "both")) {
    stop("The three options for direction are ''forward', ''backward'',
         ''both''.")
  }
  if (!is.logical(trace) || length(trace) != 1) {
    stop("trace must be a logical value. Set MSE to TRUE or FALSE. The default is 
          set to TRUE. See also help(step_fh).")
  }
  if (!is.null(scope) && ((!inherits(scope, "formula")) && 
      (!inherits(scope, "list") || (!inherits(scope[[1]], "formula") ||
                                   !inherits(scope[[2]], "formula")) ))){
    stop('Scope must be a formula or a list including two formulas 
         (lower and upper).')
  }
  if (!is.numeric(steps) || !(is.numeric(steps) && length(steps) == 1)) {
    stop("steps must be a single number determining the maximum number of steps.
         See help(step_fh).")
  }
  
}


