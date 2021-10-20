mapplot_check <- function(object,
                          map_obj,
                          map_dom_id,
                          map_tab,
                          col,
                          return_data) {
  if (!inherits(object, "emdi")) {
    stop("First object needs to be of class emdi.")
  }

  if (!is.null(map_obj) && !(inherits(map_dom_id, "character")
                             && length(map_dom_id) == 1)) {
      stop("A domain ID needs to be given by argument map_dom_id. This
           argument  must be a vector of lenght 1 and of class character
           specifying the variable (name) in map_obj. Thus, it needs to be
           contained in map_obj. See also help(map_plot).")
  }
  if (!(map_dom_id %in% names(map_obj))) {
    stop(paste0(map_dom_id, " is not contained in map_obj.
        Please provide valid variable name for pop_domains."))
  }

  if (length(col) != 2 || !is.vector(col)) {
      stop("col needs to be a vector of length 2
           defining the lower and upper color of the map plot.")
  }
  if (!is.null(map_tab) && !(inherits(map_tab, "data.frame")
                             && dim(map_tab)[2] == 2)) {
    stop("If the IDs in the data object and shape file differ a mapping
         table needs to be used. This table needs to be a data frame with
         two colums. See also help(map_plot).")
  }
  if (!(inherits(return_data, "logical") && length(return_data) == 1)) {
    stop("return_data needs to be a logical value. Set na.rm to TRUE or FALSE.
         See also help(map_plot).")
  }

  }