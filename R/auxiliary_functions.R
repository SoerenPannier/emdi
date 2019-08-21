# Auxiliary functions

makeXY <- function(formula, data){
  mf <- model.frame(formula=formula, data=data)
  x <- model.matrix(attr(mf, "terms"), data=mf)
  y <- model.response(mf)

  list(y = y,
       x = x)
}
