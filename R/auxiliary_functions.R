# Auxiliary functions

makeXY <- function(formula, data) {
  mf <- model.frame(formula = formula, data = data)
  x <- model.matrix(attr(mf, "terms"), data = mf)
  y <- model.response(mf)

  list(
    y = y,
    x = x
  )
}

throw_class_error <- function(object, subclass) {
  if (!inherits(object, "emdi")) {
    error_string <- paste0(subclass, " object has to be created by the emdi
                           package for emdi methods to work.")
    stop(error_string)
  }
}

logit <- function(p) {
  p[p == 0] <- 0.001
  p[p == 1] <- 1 - 0.001
  log(p / (1 - p))
}

logit_variance <- function(p, v){
  p[p == 0] <- 0.001
  p[p == 1] <- 1 - 0.001
  deriv <- 1 / (p * (1 - p))
  deriv^2 * v
}
