#' Step Function
#'
#' This generic function selects a model by different criteria in a stepwise
#' algorithm.
#'
#' @param object an object of type "emdi".
#' @param scope formula or a list including two formulas (\code{lower} and
#' \code{upper}) specifying the models considered in the step function.
#' Defaults to \code{NULL}.
#' @param criteria a character string describing the model selection criterion.
#' @param direction a character string describing the direction of stepwise
#' algorithm. Directions that can be chosen are "\code{both}",
#' "\code{backward}" or "\code{forward}". Defaults to "\code{both}". If no
#' \code{scope} argument is provided, the default is "\code{backward}".
#' @param trace if \code{TRUE}, information about the single steps is
#' provided during the stepwise procedure. Defaults to \code{TRUE}.
#' @param steps a number determining the maximum number of steps. Defaults
#' to 1000.
#' @param ... arguments to be passed to or from other methods.
#' @return The return of \code{step} depends on the class of its argument.
#' Please refer to the documentation of the \code{\link[stats]{step}} function
#' of the stats package for details of the default method.
#' @name step
#' @export
#' @importFrom stats factor.scope

step <- function(object, scope, criteria, direction, trace, steps,
                 ...) {
  UseMethod("step")
}


#' Method \code{step.default} Performs a Variable Selection for lm Models by
#' AIC
#'
#' @param object an object of type "emdi", or a \code{lm} object.
#' @param ... arguments to be passed to or from other methods.
#' @details The default method of the generic function \code{step}
#' applies the \code{\link[stats]{step}} function for \code{lm} models of the
#' stats package. Please refer to the documentation of the \code{step} function
#' of the stats package for details.
#' @seealso \code{\link[stats]{step}}
#' @export
#' @rdname step
#' @method step default
#' @importFrom stats step
step.default <- function(object, ...) stats::step(object, ...)



#' Method \code{step.fh} Selects a Fay-Herriot Model by Different
#' Information Criteria in a Stepwise Algorithm
#'
#' @param object an object of type "fh" that contains the chosen
#' information criterion or of type "lm" for the default method.
#' @param scope formula or a list including two formulas (\code{lower} and
#' \code{upper}) specifying the models considered in the step function.
#' Defaults to \code{NULL}.
#' @param criteria a character string describing the model selection criterion.
#' Criteria that can be chosen are "\code{AIC}", "\code{AICc}", "\code{AICb1}",
#' "\code{AICb2}", "\code{BIC}", "\code{KIC}", "\code{KICc}", "\code{KICb1}",
#' or "\code{KICb2}". Defaults to "\code{AIC}".
#' @param direction a character string describing the direction of stepwise
#' algorithm. Directions that can be chosen are "\code{both}",
#' "\code{backward}" or "\code{forward}". Defaults to "\code{both}". If no
#' \code{scope} argument is provided, the default is "\code{backward}".
#' @param trace if \code{TRUE}, information about the single steps is
#' provided during the stepwise procedure. Defaults to \code{TRUE}.
#' @param steps a number determining the maximum number of steps. Defaults to
#' 1000.
#' @param ... additional arguments that are not used in this method.
#' @return For the fh method information about the resulting "best" model due
#' to the chosen information criterion is provided:
#' \item{\code{call}}{the function call that produced the object.}
#' \item{\code{coefficients}}{data frame containing the estimated regression
#' coefficients, the standard errors and the \code{t}- and \code{p}-values of
#' the explanatory variables.}
#' @details The information criteria "\code{AICc}", "\code{AICb1}",
#' "\code{AICb2}", "\code{KIC}", "\code{KICc}", "\code{KICb1}" and
#' "\code{KICb2}" are especially developed for Fay-Herriot models by
#' \cite{Marhuenda et al. (2014)}. They are based on a bootstrap
#' algorithm. If one of the criteria is chosen, make sure that the
#' bootstrap iterations (\code{B}) of the "fh" object are set to a positive
#' number. For some model extensions of the Fay-Herriot model only the
#' "\code{AIC}" and the "\code{BIC}" information criteria are provided and for
#' some none of the information criteria are defined. Check the model_select
#' component of the "fh" object (objectname$model$model_select). If no
#' criteria are provided, it is not possible to apply the stepwise
#' variable selection algorithm.
#' @references
#' Marhuenda, Y., Morales, D. and Pardo, M.C. (2014). Information criteria for
#' Fay-Herriot model selection. Computational Statistics and Data Analysis 70,
#' 268-280.
#' @seealso \code{\link{emdiObject}}, \code{\link{fh}}
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
#' # Estimate FH model that contains all variables that should be considered
#' fh_std <- fh(
#'   fixed = Mean ~ cash + self_empl + unempl_ben,
#'   vardir = "Var_Mean", combined_data = combined_data,
#'   domains = "Domain", method = "ml", B = c(0, 50)
#' )
#'
#' # Example 1: Use default settings
#' step(fh_std)
#'
#' # Example 2: Choose "KICb2" information criterion
#' step(fh_std, criteria = "KICb2")
#' }
#' @export
#' @rdname step
#' @method step fh
#' @importFrom stats factor.scope
#' @importFrom utils capture.output

step.fh <- function(object, scope = NULL, criteria = "AIC",
                    direction = "both", trace = TRUE,
                    steps = 1000, ...) {
  step_check(
    object = object, scope = scope, criteria = criteria,
    direction = direction, trace = trace, steps = steps
  )

  if ((criteria == "AICc" || criteria == "AICb1" ||
    criteria == "AICb2" || criteria == "KICc" ||
    criteria == "KICb1" || criteria == "KICb2") &&
    (is.null(object$model$seed))) {
    object$call$seed <- 123
    catmessage <- capture.output(object <- eval(object$call))
    message(strwrap(prefix = " ", initial = "",
                    "Seed in fh object not defined, 123 used as default
                    seed."))
  }

  startobject <- object

  cut.string <- function(string) {
    if (length(string) > 1L) {
      string[-1L] <- paste0("\n", string[-1L])
    }
    string
  }

  step.results <- function(models, fit, object, catmessage) {
    change <- vapply(models, "[[", "change", FUN.VALUE = character(1))
    rdf <- vapply(models, "[[", "df.resid", FUN.VALUE = numeric(1))
    ddf <- c(NA, diff(rdf))
    infcriteria <- NULL
    infcriteria <- vapply(models, "[[", "criteria", FUN.VALUE = numeric(1))
    heading <- c(
      "Stepwise Model Path \nAnalysis of Deviance Table",
      "\nInitial Model:", deparse(object$fixed), "\nFinal Model:",
      deparse(fit$fixed), "\n"
    )
    aod <- data.frame(
      Step = I(change), Df = ddf, criteria = infcriteria,
      check.names = FALSE
    )
    attr(aod, "heading") <- heading
    fit$call$MSE <- startobject$call$MSE
    fit$call$formula <- NULL
    invisible(fit <- eval(fit$call))
    if (catmessage == TRUE) {
      message("\n")
      message(strwrap(prefix = " ", initial = "",
                      "Please note that the model selection criteria are only
                      computed based of the in-sample domains."))
    }
    class(fit) <- c("step_fh", "fh", "emdi")
    fit
  }

  Terms <- terms(object$fixed)
  object$call$formula <- object$formula <- Terms
  md <- missing(direction)
  backward <- direction == "both" | direction == "backward"
  forward <- direction == "both" | direction == "forward"
  if (missing(scope)) {
    fdrop <- numeric()
    fadd <- attr(Terms, "factors")
    if (md) {
      forward <- FALSE
    }
  } else {
    if (is.list(scope)) {
      fdrop <- if (!is.null(fdrop <- scope$lower)) {
        attr(terms(update.formula(object$fixed, fdrop)), "factors")
      } else {
        numeric()
      }
      fadd <- if (!is.null(fadd <- scope$upper)) {
        attr(terms(update.formula(object$fixed, fadd)), "factors")
      }
    } else {
      fadd <- if (!is.null(fadd <- scope)) {
        attr(terms(update.formula(object$fixed, scope)), "factors")
      }
      fdrop <- numeric()
    }
  }
  models <- vector("list", steps)
  n <- object$framework$N_dom_smp
  fit <- object
  bcriteria <- fit$model$model_select[[criteria]]
  edf <- length(attr(terms(object$fixed), "term.labels")) + 1
  if (bcriteria == -Inf) {
    stop(criteria, "is -infinity for this model, so 'step' cannot proceed",
      sep = " "
    )
  }
  nm <- 1
  if (trace == TRUE) {
    message("Start: ", criteria, " = ", format(round(bcriteria, 2)), "\n",
      cut.string(deparse(fit$fixed)), "\n\n",
      sep = ""
    )
    flush.console()
  }
  models[[nm]] <- list(df.resid = n -
    edf, change = "", criteria = bcriteria)

  while (steps > 0) {
    steps <- steps - 1
    infcriteria <- bcriteria
    ffac <- attr(Terms, "factors")
    scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
    aod <- NULL
    change <- NULL
    if (backward && length(scope$drop)) {
      aod <- drop1.fh(fit,
        criteria = criteria, scope = scope$drop,
        scale = 0
      )
      rn <- row.names(aod)
      row.names(aod) <- c(rn[1L], paste("-", rn[-1L]))
      if (any(aod$Df == 0, na.rm = TRUE)) {
        zdf <- aod$Df == 0 & !is.na(aod$Df)
        change <- rev(rownames(aod)[zdf])[1L]
      }
    }
    if (is.null(change)) {
      if (forward && length(scope$add)) {
        aodf <- add1.fh(fit, criteria = criteria, scope = scope$add)
        rn <- row.names(aodf)
        row.names(aodf) <- c(rn[1L], paste("+", rn[-1L]))
        aod <- if (is.null(aod)) {
          aodf
        } else {
          rbind(aod, aodf[-1, , drop = FALSE])
        }
      }
      attr(aod, "heading") <- NULL
      nzdf <- if (!is.null(aod$Df)) {
        aod$Df != 0 | is.na(aod$Df)
      }
      aod <- aod[nzdf, ]
      if (is.null(aod) || ncol(aod) == 0) {
        break
      }
      nc <- match("criteria", names(aod))
      nc <- nc[!is.na(nc)][1L]
      o <- order(aod[, nc])
      names(aod) <- c("df", criteria)
      if (trace == TRUE) {
        print(aod[o, ])
      }
      if (o[1L] == 1) {
        break
      }
      change <- rownames(aod)[o[1L]]
    }

    fit$call$fixed <- update(fit$fixed, as.formula(paste("~ . ", change)),
      evaluate = FALSE
    )
    fit$call$MSE <- FALSE
    fit$call$formula <- NULL
    catmessage <- capture.output(fit <- eval(fit$call))

    nnew <- fit$framework$N_dom_smp
    if (all(is.finite(c(n, nnew))) && nnew != n) {
      stop("number of rows in use has changed: remove missing values?")
    }
    Terms <- terms(fit$fixed)

    bcriteria <- fit$model$model_select[[criteria]]
    edf <- length(attr(terms(fit$fixed), "term.labels")) + 1
    if (trace == TRUE) {
      message("\nStep: ", criteria, " = ", format(round(bcriteria, 2)), "\n",
        cut.string(deparse(fit$fixed)), "\n\n",
        sep = ""
      )
      flush.console()
    }
    if (bcriteria >= infcriteria + 1e-07) {
      break
    }
    nm <- nm + 1
    models[[nm]] <- list(df.resid = n - edf, change = "", criteria = bcriteria)
  }
  if (any(grepl(
    pattern = c("Please note that the model selection criteria are
                            only computed based on the in-sample domains."),
    x = ""
  ))) {
    catmessage <- TRUE
  } else {
    catmessage <- FALSE
  }
  step.results(models = models[seq(nm)], fit, object, catmessage)
}



step_check <- function(object, scope, criteria, direction, trace,
                       steps) {
  if (!inherits(object, "fh")) {
    stop("Object needs to be fh object.")
  }
  if (!(object$method$method == "ml")) {
    stop(strwrap(prefix = " ", initial = "",
                 "The variance estimation method of the random effect has to be
                 ml. Otherwise the comparison of models based on information
                 criteria would not be valid."))
  }
  if (is.null(criteria) || !(criteria == "AIC" ||
    criteria == "AICc" ||
    criteria == "AICb1" ||
    criteria == "AICb2" ||
    criteria == "BIC" ||
    criteria == "KIC" ||
    criteria == "KICc" ||
    criteria == "KICb1" ||
    criteria == "KICb2")) {
    stop(strwrap(prefix = " ", initial = "",
                 "The nine options for criteria are ''AIC', ''AICc'',
                 ''AICb1'', ''AICb2'', ''BIC'', ''KIC'', ''KICc'', ''KICb1'',
                 ''KICb2''."))
  }
  if (is.null(object$model$model_select[[criteria]])) {
    stop(strwrap(prefix = " ", initial = "",
                 "The fh object does not contain the chosen criterion. Please
                 set the second element of the input argument B of the fh
                 function to a number greater than 1 to receive results for all
                 of the information criteria. For some model extensions of the
                 fh model the information criteria are not defined. Check the
                 model_select component of the fh object
                 (objectname$model$model_select). If no criteria are provided,
                 it is not possible to apply the stepwise variable selection
                 algorithm."))
  }
  if (is.null(direction) || !(direction == "forward" ||
    direction == "backward" ||
    direction == "both")) {
    stop(strwrap(prefix = " ", initial = "",
                 "The three options for direction are ''forward', ''backward'',
                 ''both''."))
  }
  if (!is.logical(trace) || length(trace) != 1) {
    stop(strwrap(prefix = " ", initial = "",
                 "trace must be a logical value. Set MSE to TRUE or FALSE. The
                 default is set to TRUE. See also help(step)."))
  }
  if (!is.null(scope) && ((!inherits(scope, "formula")) &&
    (!inherits(scope, "list") ||
      (!inherits(scope[[1]], "formula") ||
        !inherits(scope[[2]], "formula"))))) {
    stop(strwrap(prefix = " ", initial = "",
                 "Scope must be a formula or a list including two formulas
                 (lower and upper)."))
  }
  if (!is.numeric(steps) || !(is.numeric(steps) && length(steps) == 1)) {
    stop(strwrap(prefix = " ", initial = "",
                 "steps must be a single number determining the maximum number
                 of steps. See help(step)."))
  }
}


#' @export
print.step_fh <- function(x, ...) {
  cat("\n")
  cat("Call:\n ")
  print(x$call)
  cat("\n")
  cat("Coefficients:\n ")
  printCoefmat(as.matrix(x$model$coefficients), has.Pvalue = TRUE)
}
