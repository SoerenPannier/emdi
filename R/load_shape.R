#' Loading the Shape File for Austrian Districts
#'
#' The function simplifies to load the shape file for Austrian districts.
#'
#' @return A shape file of class \code{"sf", "data.frame"}.
#' @details The shape file contains the borders of Austrian districts. Thus, it
#' can be used for the visualization of estimation results for Austrian
#' districts.
#' @export

load_shapeaustria <- function() {
  load(file  = system.file("shapes/shape_austria_dis.rda",
                           package = "emdi"),
       envir = .GlobalEnv
  )
}

#' Loading the Shape File for Austrian Municipalities
#'
#' The function simplifies to load the shape file for Austrian municipalities.
#'
#' @return A shape file of class \code{"sf", "data.frame"}.
#' @details The shape file contains the borders of Austrian municipalities.
#' Thus, it can be used for the visualization of estimation results for Austrian
#' municipalities.
#' @export
#'
load_shapeaustria_mun <- function() {
  load(file  = system.file("shapes/shape_austria_mun.rda",
                           package = "emdi"),
       envir = .GlobalEnv
  )
}
