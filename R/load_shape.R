#' Loading the shape file for austrian districts
#'
#' @return A shape file of class \code{SpatialPolygonsDataFrame}.
#' @details The shape file contains the borders of Austrian districts. Thus, it
#' can be used for the visualiuzation of estimation results for Austrian
#' districts.
#' @export

load_shapeaustria <- function(){
  load(system.file("shapes/shape_austria_dis.RData", package="emdi"),envir = .GlobalEnv)
}