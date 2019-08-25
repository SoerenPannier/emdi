#' Spatial autocorrelation tests
#'
#' This function computes two spatial autocorrelation tests: Moran's I and 
#' Geary's C.
#'
#' @param direct a vector containing the direct estimates of the Fay-Herriot model.
#' @param corMatrix proximity matrix or data frame.
#' @return The values of the test statistics their corresponding p values.
#' @import spdep
#' @export


spatialcor.tests <- function(direct, corMatrix){
  
  if (is.matrix(corMatrix) == FALSE){corMatrix <- as.matrix(corMatrix)}
  corMatrix_list <- mat2listw(corMatrix)
  moran <- moran.test(direct, corMatrix_list, randomisation=TRUE, zero.policy=TRUE,
             alternative="greater")
  geary <- geary.test(direct, corMatrix_list, randomisation=TRUE, zero.policy=TRUE,
             alternative="greater")
  out <- data.frame(Statistics = c("Moran's I", "Geary's C"), 
                    Value = c(unname(moran$estimate[1]), unname(geary$estimate[1])),
                    p.value = c(moran$p.value, geary$p.value))
  out
  
  
}



