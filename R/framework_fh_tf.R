framework_FH_tf <- function(data, fixed, vardir, domains, subdomains,
                            trafo, eff_n, nunits) {

  orig_data <- data
  if(trafo != "arcsin"){
    data <- data[, c(get.vars(fixed), vardir, domains, subdomains, nunits)]
  } else if(trafo == "arcsin") {
    data <- data[, c(get.vars(fixed), vardir, domains, subdomains, nunits, eff_n)]
  }
  # Framework-------------------------------------------------------------------
  # Number of units in each sub-area (in population)
  N_ij <- data[[nunits]]
  # Number of units in each area (in population)
  N_i <- aggregate(data[[nunits]], by = list(data[[domains]]), FUN = sum)$x
  # Total number of units (in population)
  N <- sum(N_ij)
  # Names of area
  DomName <- unique(data[[domains]])

  # data of sampled sub-areas-----------------------------------------------------
  # Observation of sub-area
  data$ObsSub <- ifelse(is.na(data[[fixed[[2]]]]), "no", "yes")

  # data of sampled sub-areas
  smp_data <- data[data$ObsSub == "yes", ]

  X <- model.matrix(object = fixed, data = smp_data)
  rownames(X) <- smp_data[[subdomains]]

  # In or Out of sample domains & subdomains
  N_in_sub <- length(which(data$ObsSub == "yes"))
  N_out_sub <- length(which(data$ObsSub == "no"))
  N_in_dom <- length(unique(data[[domains]][data$ObsSub == "yes"]))
  N_out_dom <- length(unique(data[[domains]][data$ObsSub == "no"]))

  # Dir_y and vare at subarea level-----------------------------------------------
  # With arcsin trafo
  if(trafo == "arcsin"){
    y_ij_orig <- smp_data[[fixed[[2]]]]
    vare_orig <- smp_data[[vardir]]
    y_ij <- asin(sqrt(y_ij_orig))
    vare <- 1 / (4 * smp_data[[eff_n]])
    data$vare <- 1 / (4 * data[[eff_n]])
    smp_data[[fixed[[2]]]] <- y_ij
    smp_data$y_orig <- y_ij_orig
  } else if(trafo == "log") {
    y_ij_orig <- smp_data[[fixed[[2]]]]
    vare_orig <- smp_data[[vardir]]
    vare <- (y_ij_orig)^(-2) * vare_orig
    data$vare <- (1 / data[[fixed[[2]]]])^2 * data[[vardir]] # for bc back-trafo
    y_ij <- log(y_ij_orig)
    smp_data[[fixed[[2]]]] <- y_ij
    smp_data$y_orig <- y_ij_orig
  } else if(trafo == "no") {
    y_ij <- smp_data[[fixed[[2]]]]
    vare <- smp_data[[vardir]]
    data$vare <- data[[vardir]]
  }

  names(vare) <- smp_data[[subdomains]]
  names(y_ij) <- smp_data[[subdomains]]
  # Number of sub-areas in each area
  if(is.factor(smp_data[[domains]])){no <- as.vector(table(as.character(smp_data[[domains]])))}
  else {no <- as.vector(table(smp_data[[domains]]))}
  # Total number of areas in the sample
  m <- length(unique(smp_data[[domains]]))
  # Total number of areas in pop
  M <- length(unique(data[[domains]]))
  # Total number of subareas in the sample
  n <- length(smp_data[[subdomains]])
  # Number of x variables
  p <- ncol(X)

  return(list(orig_data = orig_data,
              smp_data = smp_data,
              data = data,
              y_ij = y_ij,
              no = no,
              X = X,
              m = m,
              p = p,
              vare = vare,
              DomName = DomName,
              M = M,
              N = N,
              N_in_sub = N_in_sub,
              N_out_sub = N_out_sub,
              N_in_dom = N_in_dom,
              N_out_dom = N_out_dom
  ))

}