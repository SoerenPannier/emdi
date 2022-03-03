#' Proximity matrix for spatial area-level models
#'
#' A data set comprising the row-standardized proximities between the
#' domains of the \code{\link{eusilcA_smpAgg}} data set.
#'
#' @format A data set with dimensions number of areas (94) times
#' number of areas (94). Values lie between \code{0} and \code{1}. The
#' respective row sums amount to 1.
#'
#' @details For a description of how to create the proximity matrix,
#' see the package vignette "A Framework for Producing Small Area Estimates
#' Based on Area-Level Models in R".
#'
#' @docType data
"eusilcA_prox"
