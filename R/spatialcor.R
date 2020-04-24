#' Spatial autocorrelation tests
#'
#' This function computes two spatial autocorrelation tests: Moran's I and 
#' Geary's C.
#'
#' @param direct a vector containing direct estimates.
#' @param corMatrix proximity matrix or data frame.
#' @return The values of the test statistics and their corresponding p values.
#' @details When creating the proximity matrix \code{corMatrix}, please make 
#' sure that the elements of \code{direct} and \code{corMatrix} are sorted 
#' equally and that \code{direct} and \code{corMatrix} do not contain any 
#' \code{NA}s. For a description of how to create the proximity matrix see the 
#' package vignette. 
#' @references Bivand, R. (2019), spdep: Spatial Dependence: Weighting Schemes, 
#' Statistics. R package.
#' @importFrom spdep geary.test mat2listw moran.test
#' @export


spatialcor.tests <- function(direct, corMatrix){
  
  spatialcor_check(direct = direct, corMatrix = corMatrix)
  
  if (is.matrix(corMatrix) == FALSE){corMatrix <- as.matrix(corMatrix)}
  corMatrix_list <- mat2listw(corMatrix)
  moran <- moran.test(direct, corMatrix_list, randomisation = TRUE, 
                      zero.policy = TRUE, alternative = "greater")
  geary <- geary.test(direct, corMatrix_list, randomisation=TRUE, 
                      zero.policy = TRUE, alternative = "greater")
  out <- data.frame(Statistics = c("Moran's I", "Geary's C"), 
                    Value = c(unname(moran$estimate[1]), unname(geary$estimate[1])),
                    p.value = c(moran$p.value, geary$p.value))
  out
  
}

spatialcor_check <- function(direct, corMatrix){
  
  if (!is.vector(direct)) {
    stop('direct must be a vector.')
  }
  if (any(is.na(direct))) {
    stop('direct must not contain NAs.')
  }
  if (any(is.na(corMatrix))) {
    stop('corMatrix must not contain NAs.')
  }
  if (!(is.matrix(corMatrix) || is.data.frame(corMatrix))) {
    stop('corMatrix must be a matrix or data frame.')
  }
  if (dim(corMatrix)[1] != dim(corMatrix)[2]){
    stop('corMatrix must be a matrix or data frame with same number of rows and 
         columns equal to the number of in-sample domains.')
  }
  if (length(direct) != dim(corMatrix)[1]){
    stop('The vector containing the direct estimates must have the same length 
        as the dimensions of corMatrix. The same in-sample domains must 
         be included.')
  }
  
}
