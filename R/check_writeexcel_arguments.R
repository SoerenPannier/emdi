writeexcel_check <- function(object, file, split) {
  if (!inherits(object, "emdi")) {
    stop("First object needs to be of class emdi.")
  }
  if (!(inherits(split, "logical") && length(split) == 1)) {
    stop(strwrap(prefix = " ", initial = "",
                 "split must to be a logical value. Set CV to TRUE or FALSE."))
  }
  if (!inherits(file, "character") || (length(grep(".xlsx", file)) == 0)) {
    stop(strwrap(prefix = " ", initial = "",
                 "file must to be a character string that determines a path and
                 filename. It should end on .xlsx"))
  }
}

writeods_check <- function(object, file, split) {
  if (!inherits(object, "emdi")) {
    stop("First object needs to be of class emdi.")
  }
  if (!(inherits(split, "logical") && length(split) == 1)) {
    stop(strwrap(prefix = " ", initial = "",
                 "split must to be a logical value. Set CV to TRUE or FALSE."))
  }
  if (!inherits(file, "character") || (length(grep(".ods", file)) == 0)) {
    stop(strwrap(prefix = " ", initial = "",
                 "file must to be a character string that determines a path and
                 filename. It should end on .ods"))
  }
  testZIP <- 1
  try(testZIP <- shell("zip"), silent = TRUE)
  if (testZIP == 1) {
    stop(strwrap(prefix = " ", initial = "",
                 "No zipping application was found, see details in
                 help(write.ods) for details."))
  }
}
