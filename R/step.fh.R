#' Step function
#'
#' This function selects a model by different criteria in a stepwise algorithm.
#'
#' @param object fitted FH model.
#' @param scope defines the range of models examined in the stepwise search. 
#' This should be either a single formula, or a list containing components
#' \code{upper} and \code{lower}, both formulae.
#' @param critera a character string describing the model selection criterion. 
#' Criteria that can be chosen are "\code{AIC}" or "\code{BIC}". Defaults to
#' "\code{AIC}".
#' @param direction the mode of stepwise search, can be one of "\code{both}",
#' "\code{backward}", or "\code{forward}", with a default of "\code{both}".
#' If the scope argument is missing the default for direction is "\code{backward}".
#' @param trace if positive, information is printed during the running of \code{step}.
#' Defaults to 1.
#' @param keep a filter function whose input is a fitted model object and the 
#' associated \code{AIC} statistic, and whose output is arbitrary. Typically 
#' \code{keep} will select a subset of the components of the object and return 
#' them. The default is not to keep anything.
#' @param steps the maximum number of steps to be considered. The default is 1000.
#' @return Call and coefficients of the best model due to information criterion.
#' @export


step.fh <- function (object, scope, criteria = "AIC", direction = c("both", "backward", 
                                                          "forward"), trace = 1,
                     keep = NULL, steps = 1000, 
                  ...) 
{
  step.fh_check(object = object, scope = scope, direction = direction,
                keep = keep, steps = steps)
  
  cut.string <- function(string) {
    if (length(string) > 1L) 
      string[-1L] <- paste0("\n", string[-1L])
    string
  }
  re.arrange <- function(keep) {
    namr <- names(k1 <- keep[[1L]])
    namc <- names(keep)
    nc <- length(keep)
    nr <- length(k1)
    array(unlist(keep, recursive = FALSE), c(nr, nc), list(namr, 
                                                           namc))
  }
  step.results <- function(models, fit, object) { 
    change <- sapply(models, "[[", "change")
    rdf <- sapply(models, "[[", "df.resid")
    ddf <- c(NA, diff(rdf))
    AIC <- sapply(models, "[[", "AIC")
    BIC <- sapply(models, "[[", "BIC")
    heading <- c("Stepwise Model Path \nAnalysis of Deviance Table", 
                 "\nInitial Model:", deparse(object$fixed), "\nFinal Model:", 
                 deparse(fit$fixed), "\n")
    if (criteria == "AIC"){
      aod <- data.frame(Step = I(change), Df = ddf, AIC = AIC, 
                        check.names = FALSE)  
    }
    if (criteria == "BIC"){
      aod <- data.frame(Step = I(change), Df = ddf, BIC = BIC, 
                        check.names = FALSE) 
    }
    attr(aod, "heading") <- heading
    fit$anova <- aod
    list(Call = fit$call,
    Coefficients = fit$model$coefficients)
    
  }
  Terms <- terms(object$fixed)
  object$call$formula <- object$formula <- Terms
  md <- missing(direction)
  direction <- match.arg(direction)
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
  if (!is.null(keep)) 
    keep.list <- vector("list", steps)
  n <- object$framework$m
  fit <- object 
  if (criteria == "AIC"){
    bAIC <- fit$model$model_select$AIC
    edf <- length(attr(terms(object$fixed), "term.labels")) + 1 
    if (is.na(bAIC)) 
      stop("AIC is not defined for this model, so 'step' cannot proceed")
    if (bAIC == -Inf) 
      stop("AIC is -infinity for this model, so 'step' cannot proceed")
    nm <- 1
    if (trace) {
      cat("Start:  AIC=", format(round(bAIC, 2)), "\n", 
          cut.string(deparse(fit$fixed)), "\n\n", sep = "")
      flush.console()
    }
    models[[nm]] <- list(df.resid = n - 
                           edf,change = "", AIC = bAIC) 
    if (!is.null(keep)) 
      keep.list[[nm]] <- keep(fit, bAIC)
  }
  else if (criteria == "BIC"){
    bBIC <- fit$model$model_select$BIC
    edf <- length(attr(terms(object$fixed), "term.labels")) + 1
    if (is.na(bBIC)) 
      stop("BIC is not defined for this model, so 'step' cannot proceed")
    if (bBIC == -Inf) 
      stop("BIC is -infinity for this model, so 'step' cannot proceed")
    nm <- 1
    if (trace) {
      cat("Start:  BIC=", format(round(bBIC, 2)), "\n", 
          cut.string(deparse(fit$fixed)), "\n\n", sep = "")
      flush.console()
    }
    models[[nm]] <- list(df.resid = n - 
                           edf,change = "", BIC = bBIC)   
    if (!is.null(keep)) 
      keep.list[[nm]] <- keep(fit, bBIC)
  }

  while (steps > 0) {
    steps <- steps - 1
    if (criteria == "AIC"){
      AIC <- bAIC
    }
    if (criteria == "BIC"){
      BIC <- bBIC
    }
    ffac <- attr(Terms, "factors")
    scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
    aod <- NULL
    change <- NULL
    if (backward && length(scope$drop)) {
      aod <- drop1.fh(fit, criteria = criteria,scope = scope$drop,
                      scale = 0, data = data, interval = interval)
      rn <- row.names(aod)
      row.names(aod) <- c(rn[1L], paste("-", rn[-1L]))
      if (any(aod$Df == 0, na.rm = TRUE)) {
        zdf <- aod$Df == 0 & !is.na(aod$Df)
        change <- rev(rownames(aod)[zdf])[1L]
      }
    }
    if (is.null(change)) {
      if (forward && length(scope$add)) {
        aodf <- add1.fh(fit, criteria = criteria,scope = scope$add, scale = 0)
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
      if (criteria == "AIC"){
        nc <- match("AIC", names(aod)) #"Cp", match(c("AIC"), names(aod)) #"Cp", 
      }
      if (criteria == "BIC"){
        nc <- match("BIC", names(aod)) #"Cp", match(c("AIC"), names(aod)) #"Cp", 
      }
      nc <- nc[!is.na(nc)][1L]
      o <- order(aod[, nc])
      if (trace) 
        print(aod[o, ])
      if (o[1L] == 1) 
        break
      change <- rownames(aod)[o[1L]]
    }
    
    fit$call$fixed <- update(fit$fixed, as.formula(paste("~ . ", change)),
                             evaluate = FALSE) 
    
    fit$call$formula <- NULL  
    fit <- eval(fit$call)

    nnew <- fit$framework$m 
    if (all(is.finite(c(n, nnew))) && nnew != n) 
      stop("number of rows in use has changed: remove missing values?")
    Terms <- terms(fit$fixed)
    if (criteria == "AIC"){
      bAIC <- fit$model$model_select$AIC
      edf <- length(attr(terms(fit$fixed), "term.labels")) + 1 
      if (trace) {
        cat("\nStep:  AIC=", format(round(bAIC, 2)), "\n", 
            cut.string(deparse(fit$fixed)), "\n\n", sep = "")
        flush.console()
      }
      if (bAIC >= AIC + 1e-07) 
        break
      nm <- nm + 1
      models[[nm]] <- list(df.resid = n - edf,change = "", AIC = bAIC)
      if (!is.null(keep)) 
        keep.list[[nm]] <- keep(fit, bAIC)
    }
    else if (criteria == "BIC"){
      bAIC <- fit$model$model_select$BIC
      edf <- length(attr(terms(fit$fixed), "term.labels")) + 1 
      if (trace) {
        cat("\nStep:  BIC=", format(round(bAIC, 2)), "\n", 
            cut.string(deparse(fit$fixed)), "\n\n", sep = "")
        flush.console()
      }
      if (bBIC >= BIC + 1e-07) 
        break
      nm <- nm + 1
      models[[nm]] <- list(df.resid = n - 
                             edf,change = "", BIC = bBIC)
      if (!is.null(keep)) 
        keep.list[[nm]] <- keep(fit, bBIC)
    }
  }
  if (!is.null(keep)) 
    fit$keep <- re.arrange(keep.list[seq(nm)])
  results <- step.results(models = models[seq(nm)], fit, object) 
  class(results) <- "step.fh"
  results
}


print.step.fh <- function(x)
{
  cat("\n")
  cat("Call:\n ")
  print(x$Call)
  cat("\n")
  cat("Coefficients:\n ")
  print(x$Coefficients)
}

step.fh_check <- function(object, scope, direction, trace,keep,
                          steps){
  
  if(!inherits(object, "fh")){
    stop('Object needs to be fh object.')
  }
  if (!(object$method$method == "ml")){
    stop('The variance estimation method of the random effect has to be ml. Otherwise
         the information criteria are not valid.')
  }
  if (is.null(direction) || !(direction == "forward" 
                              || direction == "backward" 
                              || direction == "both")) {
    stop("The three options for direction are ''forward', ''backward'',
         ''both''.")
  }
  
  if (!is.numeric(steps) || !(is.numeric(steps) && length(steps) == 1)) {
    stop("steps must be a single number determining the maximum number of steps.
         See help(step.fh).")
  }
  
}
