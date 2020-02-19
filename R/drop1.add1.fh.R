################################################################################
################################################################################
#### drop1 Method for fh function

drop1.fh <- function(object,criteria, scope,...)
{
  tl <- attr(terms(object$fixed), "term.labels") 
  if(missing(scope)) 
    scope <- drop.scope(object$fixed)
  else {
    if(!is.character(scope))
      scope <- attr(terms(update.formula(object$fixed, scope)), "term.labels")
    if(!all(match(scope, tl, 0L) > 0L))
      stop("scope is not a subset of term labels")
  }
  ns <- length(scope)
  ans <- matrix(nrow = ns + 1L, ncol = 2L,
                dimnames = list(c("<none>", scope), c("df", "criteria")))

  ans[1, ] <- c(length(attr(terms(object$fixed), "term.labels")) + 1, 
                object$model$model_select[[criteria]]) ##### ACHTUNG

  n0 <- object$framework$N_dom_smp

  for(i in seq_len(ns)) {
    tt <- scope[i]
    nfit <- object
    nfit$call$fixed <- update(object$fixed, as.formula(paste("~ . -", tt)),
                             evaluate = FALSE) 
    nfit$call$formula <- NULL
    nfit <- eval(nfit$call)
    
    ans[i+1, ] <- c(length(attr(terms(nfit$fixed), "term.labels")) + 1,
                    nfit$model$model_select[[criteria]])
    nnew <- nfit$framework$N_dom_smp
    if(all(is.finite(c(n0, nnew))) && nnew != n0)
      stop("number of rows in use has changed: remove missing values?")
  }
  dfs <- ans[1L , 1L] - ans[, 1L]
  dfs[1L] <- NA
  aod <- data.frame(Df = dfs, criteria = ans[,2])
  head <- c("Single term deletions", "\nModel:", deparse(formula(object$fixed)))
  class(aod) <- c("anova", "data.frame")
  attr(aod, "heading") <- head
  aod
}


################################################################################
################################################################################
#### add1 Method for fh function

add1.fh <- function(object, criteria, scope,trace = 1)
{
  if(missing(scope) || is.null(scope)) stop("no terms in scope")
  if(!is.character(scope))
    scope <- add.scope(object$fixed, update.formula(object$fixed, scope))
 
  if(!length(scope))
    stop("no terms in scope for adding to object")
  #     newform <- update.formula(object,
  #                               paste(". ~ . +", paste(scope, collapse="+")))
  #     data <- model.frame(update(object, newform)) # remove NAs
  #     object <- update(object, data = data)
  
  ns <- length(scope)
  ans <- matrix(nrow = ns + 1L, ncol = 2L,
                dimnames = list(c("<none>", scope), c("df", criteria)))
  ans[1, ] <- c(length(attr(terms(object$fixed), "term.labels")) + 1, 
                object$model$model_select[[criteria]])
  n0 <- object$framework$N_dom_smp
  for(i in seq_len(ns)) {
    tt <- scope[i]
    if(trace > 1) {
      cat("trying +", tt, "\n", sep = "")
      flush.console()
    }
    nfit <- object
    nfit$call$fixed <- update(object$fixed, as.formula(paste("~ . +", tt)),
                              evaluate = FALSE)
    nfit$call$formula <- NULL
    nfit <- eval(nfit$call)
    ans[i+1L, ] <- nfit$model$model_select[[criteria]]
    nnew <- nfit$framework$N_dom_smp
    if(all(is.finite(c(n0, nnew))) && nnew != n0)
      stop("number of rows in use has changed: remove missing values?")
  }
  
  dfs <- ans[, 1L] - ans[1L, 1L]
  dfs[1L] <- NA
  aod <- data.frame(Df = dfs, criteria = ans[, 2L])
  head <- c("Single term additions", "\nModel:", deparse(formula(object$fixed)))
  class(aod) <- c("anova", "data.frame")
  attr(aod, "heading") <- head
  aod
}

