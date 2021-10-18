# Auxiliary functions

makeXY <- function(formula, data) {
  mf <- model.frame(formula = formula, data = data)
  x <- model.matrix(attr(mf, "terms"), data = mf)
  y <- model.response(mf)

  list(y = y,
       x = x)
}

throw_class_error <- function(object, subclass){
  if (!inherits(object, "emdi")) {
    error_string <- paste0(subclass, " object has to be created by the emdi 
                           package for emdi methods to work.")
    stop(error_string)
  }
}