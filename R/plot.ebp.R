#' @rdname plot.emdi
#' @export
plot.ebp <- function(x,
                     label = "orig",
                     color = c("blue", "lightblue3"),
                     gg_theme = NULL,
                     cooks = TRUE,
                     range = NULL, ...) {
  plot_check(x = x, label = label, color = color, cooks = cooks, range = range)

  # Preparation for plots
  residuals <- residuals(x$model, level = 0, type = "pearson")
  rand.eff <- nlme::ranef(x$model)$"(Intercept)"
  srand.eff <- (rand.eff - mean(rand.eff)) / sd(rand.eff)
  tmp <- as.matrix(random.effects(x$model))[, 1]
  model <- x$model
  model$call$fixed <- x$fixed
  cook_df <- NULL
  indexer <- NULL
  likelihoods <- NULL
  opt_lambda <- FALSE

  if (cooks == TRUE) {
    cooksdist <- NULL
    # Supress warning is used here due to a small bug in the
    # HLMdiag:::.extract.lmeDesign which is underlying the here used
    # cooks distance. The given warning has no relevance in this case.
    try(cooksdist <- as.vector(suppressWarnings(cooks.distance(model))),
      silent = TRUE
    )
    if (is.null(cooksdist)) {
      cooks <- FALSE
      warning(strwrap(prefix = " ", initial = "",
                      paste0("Cook's distance could not be calculated, this is
                             usually due to exceedence of available memory. Try
                             using cooks = FALSE to avoid this message and
                             improve computation time."
                             )))
    } else {
      cook_df <- data.frame(index = seq_along(cooksdist), cooksdist)
      indexer <- cook_df[order(cooksdist, decreasing = TRUE), ][seq_len(3), ]
    }
  }

  if (x$transformation == "box.cox" || x$transformation == "dual" ||
    x$transformation == "log.shift") {
    opt_lambda <- TRUE

    if (is.null(range)) {
      range <- seq(x$transform_param$optimal_lambda -
        (0.5 * x$transform_param$optimal_lambda),
      x$transform_param$optimal_lambda +
        (0.5 * x$transform_param$optimal_lambda),
      length = 50
      )

      # if (x$transformation == 'box.cox') {
      #  range <- seq(-1, 2, length = 50)
      # } else if (x$transformation == 'dual') {
      #  range <- seq(0, 2, length = 50)
      # } else if (x$transformation == 'log.shift') {
      #  vector <- x$framework$smp_data[paste(x$fixed[2])]
      #  span <- range(vector)
      #  if( (span[1]+1) <= 1) {
      #    lower <- abs(span[1])+1
      #  } else {
      #    lower <- 0
      #  }
      #
      #  upper = diff(span) / 2
      #
      #  range <- seq(lower, upper, length = 50)
      # }
    } else {
      range <- range
    }


    likelihoods <- vapply(range,
      function(lam, fixed, smp_data, smp_domains,
               transformation) {
        result <- NULL
        try(result <- -as.numeric(
          generic_opt(
            lam, fixed, smp_data,
            smp_domains, transformation
          )
        ),
        silent = TRUE
        )
        if (is.null(result)) result <- NA
        result
      }, numeric(1),
      fixed = x$fixed,
      smp_data = x$framework$smp_data,
      smp_domains = x$framework$smp_domains,
      transformation = x$transformation
    )

    if (any(is.na(likelihoods))) {
      warning(strwrap(prefix = " ", initial = "",
                      paste0("For some lambda in the chosen range, the
                             likelihood does not converge. For these lambdas,
                             no likelihood is plotted. Choose a different range
                             to avoid this behaviour"
                             )))
    }
  }
  NextMethod("plot",
    cooks = cooks, range = range,
    opt_lambda = opt_lambda, cook_df = cook_df,
    indexer = indexer, likelihoods = likelihoods,
    residuals = residuals,
    srand.eff = srand.eff, tmp = tmp
  )
}
