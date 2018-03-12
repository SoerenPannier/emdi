writeexcel_check <- function(object, file, split){
  if (!inherits(object, "emdi")) {
    stop('First object needs to be of class emdi.')
  }
  if (!(inherits(split, "logical") && length(split) == 1)) {
    stop("split must to be a logical value. Set CV to TRUE or FALSE.")
  }
  if (!inherits(file, "character") || (length(grep(".xlsx", file)) == 0)) {
    stop("file must to be a character string that determines a path and 
         filename. It should end on .xlsx")
  }
}

writeods_check <- function(object, file, split){
  if (!inherits(object, "emdi")) {
    stop('First object needs to be of class emdi.')
  }
  if (!(inherits(split, "logical") && length(split) == 1)) {
    stop("split must to be a logical value. Set CV to TRUE or FALSE.")
  }
  if (!inherits(file, "character") || (length(grep(".ods", file)) == 0)) {
    stop("file must to be a character string that determines a path and 
         filename. It should end on .ods")
  }
}
