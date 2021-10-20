#' Spatial autocorrelation tests
#'
#' This function computes two spatial autocorrelation tests: Moran's I and
#' Geary's C.
#'
#' @param direct a vector containing direct estimates. The elements of
#' \code{direct} must be sorted like the elements in \code{corMatrix}.
#' @param corMatrix matrix or data frame with dimensions number of areas times
#' number of areas containing the row-standardized proximities between the
#' domains. Values must lie between \code{0} and \code{1}. The columns and rows
#' must be sorted like the domains in \code{direct}.
#' @return The values of the test statistics and their corresponding p values.
#' @details When creating the proximity matrix \code{corMatrix}, please make
#' sure that the elements of \code{direct} and \code{corMatrix} are sorted
#' equally and that \code{direct} and \code{corMatrix} do not contain any
#' \code{NA}s. For a description of how to create the proximity matrix,
#' see the package vignette "A Framework for Producing Small Area Estimates
#' Based on Area-Level Models in R". If direct estimates do not exist for every
#' area contained in the proximity matrix, the proximity matrix needs to be
#' subsetted to the areas contained in the direct vector.
#' @references Bivand, R. (2019), spdep: Spatial Dependence: Weighting Schemes,
#' Statistics. R package.
#' @examples
#' # Loading data - sample data and proximity matrix
#' data("eusilcA_smpAgg")
#' data("eusilcA_prox")
#'
#' # Compute spatial correlation tests
#' spatialcor.tests(direct = eusilcA_smpAgg$Mean,
#' corMatrix = eusilcA_prox)
#' @importFrom spdep geary.test mat2listw moran.test
#' @export


spatialcor.tests <- function(direct, corMatrix){

  spatialcor_check(direct = direct, corMatrix = corMatrix)

  if (is.matrix(corMatrix) == FALSE) {
    corMatrix <- as.matrix(corMatrix)}
  corMatrix_list <- mat2listw(corMatrix)
  moran <- moran.test(direct, corMatrix_list, randomisation = TRUE,
                      zero.policy = TRUE, alternative = "greater")
  geary <- geary.test(direct, corMatrix_list, randomisation = TRUE,
                      zero.policy = TRUE, alternative = "greater")
  out <- data.frame(Statistics = c("Moran's I", "Geary's C"),
                    Value = c(unname(moran$estimate[1]),
                              unname(geary$estimate[1])),
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
  if (dim(corMatrix)[1] != dim(corMatrix)[2]) {
    stop('corMatrix must be a matrix or data frame with same number of rows and
         columns equal to the length of direct.')
  }
  if (length(direct) != dim(corMatrix)[1]) {
    stop('The vector containing the direct estimates must have the same length
        as the dimensions of corMatrix.')
  }
  if (all(corMatrix == 0)) {
    stop('The elements of corMatrix are all equal to 0. Please provide a
       valid proximity matrix. A description how a proximity matrix
          can be computed can be found in the vignette.')
  }
}




