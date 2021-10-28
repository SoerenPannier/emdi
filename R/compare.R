#' Compare function
#'
#' Function \code{compare} is a generic function used to assess the quality of
#' the model-based estimates by comparing them with the direct estimates.
#'
#' @param object an object of type "emdi".
#' @param ... further arguments passed to or from other methods.
#' @return The return of \code{compare} depends on the class of its argument.
#' The documentation of particular methods gives detailed information about the
#' return of that method.
#' @export

compare <- function(object, ...) UseMethod("compare")


#' Compare function
#'
#' Method \code{compare.fh} assesses the quality of the model-based estimates
#' of the Fay-Herriot model by comparing them with the direct estimates based
#' on a goodness-of-fit test proposed by \cite{Brown et al. (2001)} and by
#' computing the correlation between the regression-synthetic part of the
#' Fay-Herriot model and the direct estimates.
#'
#' @param object an object of type "fh".
#' @param ... further arguments passed to or from other methods.
#' @return For the method for class "fh", the null hypothesis, the value W of
#' the test statistic, the degrees of freedom and the p value of the Brown test;
#' and the correlation coefficient of the synthetic part and the direct
#' estimator \cite{(Chandra et al. 2015)} are returned.
#' @references
#' Brown, G., R. Chambers, P. Heady, and D. Heasman (2001). Evaluation of small
#' area estimation methods: An application to unemployment estimates from the UK
#' LFS. Symposium 2001 - Achieving Data Quality in a Statistical Agency: A
#' Methodological Perspective, Statistics Canada. \cr \cr
#' Chandra, H., Salvati, N. and Chambers, R. (2015), A Spatially
#' Nonstationary Fay-Herriot Model for Small Area Estimation, Journal
#' of the Survey Statistics and Methodology, 3, 109-135.
#' @rdname compare
#' @export
#' @importFrom stats cor pchisq

compare.fh <- function(object, ...) {
  throw_class_error(object, "fh")

  if (is.null(object$MSE$FH)) {
    testresults <- NULL
    message("The fh object does not contain MSE estimates. The Brown test
               statistic cannot be computed.", "\n")
  } else {
    W_BL <- sum((object$ind$Direct[object$ind$Out == 0] -
      object$ind$FH[object$ind$Out == 0])^2 /
      (object$MSE$Direct[object$MSE$Out == 0] +
        object$MSE$FH[object$MSE$Out == 0]))

    # Degress of freedom
    df_BL <- object$framework$N_dom_smp

    # p Value
    p_value_BL <- 1 - pchisq(W_BL, df_BL)

    testresults <- data.frame(
      W.value = W_BL,
      Df = df_BL,
      p.value = p_value_BL
    )
  }

  # Extraction of the regression part
  if (!is.null(object$model$gamma)) {
    xb <- (object$ind$FH[object$ind$Out == 0] -
      object$model$gamma$Gamma[object$ind$Out == 0] *
        object$ind$Direct[object$ind$Out == 0]) /
      (1 - object$model$gamma$Gamma[object$ind$Out == 0])
  }
  if (is.null(object$model$gamma)) {
    xb <- object$ind$FH[object$ind$Out == 0] -
      object$model$random_effects
  }


  # Direct estimator
  direct_insample <- object$ind$Direct[object$ind$Out == 0]
  # Correlation
  syndircor <- cor(xb, direct_insample)

  results <- list(Brown = testresults, syndir = syndircor)

  class(results) <- "compare.fh"

  if (object$framework$N_dom_unobs > 0) {
    message("Please note that the computation of both test statistics is only
           based on in-sample domains.", "\n")
  }
  return(results)
}

#' Prints compare.fh objects
#'
#' compare.fh object is printed.
#'
#' @param x an object of type "compare.fh".
#' @param ... further arguments passed to or from other methods.
#' @noRd
#' @export

print.compare.fh <- function(x, ...) {
  if (!(is.null(x$Brown))) {
    cat("Brown test", "\n")
    cat("\n")
    cat("Null hypothesis: EBLUP estimates do not differ significantly from the
      direct estimates", "\n")
    cat("\n")
    print(data.frame(
      W.value = x[[1]]$W,
      Df = x[[1]]$Df,
      p.value = x[[1]]$p.value,
      row.names = ""
    ))
    if (length(x) == 2) {
      cat("\n")
      cat(
        "Correlation between synthetic part and direct estimator: ",
        round(x[[2]], 2), "\n"
      )
    }
  } else {
    if (length(x) == 2) {
      cat("\n")
      cat(
        "Correlation between synthetic part and direct estimator: ",
        round(x[[2]], 2), "\n"
      )
    }
  }
}

#' Compare predictions of model objects
#'
#' Function \code{compare_pred} is a generic function used to compare
#' predictions of two model objects.
#'
#' @param object1 an object of type "emdi".
#' @param object2 an object of type "emdi".
#' @param MSE if \code{TRUE}, MSE estimates are also returned. Defaults to
#' \code{FALSE}.
#' @param ... further arguments passed to or from other methods.
#' @export
#' @name compare_pred

compare_pred <- function(object1, object2, MSE = FALSE, ...) {
  UseMethod("compare_pred")
}

#' Compare predictions of emdi objects
#'
#' Method \code{compare_pred.emdi} compares predictions of two emdi objects.
#'
#' @param object1 an object of type "emdi".
#' @param object2 an object of type "emdi".
#' @param MSE if \code{TRUE}, MSE estimates are also returned. Defaults to
#' \code{FALSE}.
#' @param ... further arguments passed to or from other methods.
#' @return Data frame containing the point estimates of both emdi objects. If
#' column names are duplicated, the suffixes "_1" and "_2" are added to their
#' names. "_1" and "_2" standing for object1 and object2, respectively. If
#' \code{MSE} is set to \code{TRUE}, the data frame also contains the MSE
#' estimates of the emdi objects.
#' @seealso \code{\link{direct}}, \code{\link{ebp}}, \code{\link{fh}}
#' @examples
#' \donttest{
#' # Example for class ebp
#' emdi_model_1 <- ebp(
#'   fixed = eqIncome ~ gender + eqsize + cash + self_empl +
#'     unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow +
#'     house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#'   pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#'   na.rm = TRUE
#' )
#'
#' emdi_model_2 <- ebp(
#'   fixed = eqIncome ~ gender + eqsize + cash + self_empl +
#'     unempl_ben + age_ben + surv_ben, pop_data = eusilcA_pop,
#'   pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#'   na.rm = TRUE
#' )
#'
#' compare_pred(emdi_model_1, emdi_model_2)
#' }
#' @export
#' @rdname compare_pred
#' @method compare_pred emdi


compare_pred.emdi <- function(object1, object2, MSE = FALSE, ...) {
  if (!inherits(object1, "emdi") || !inherits(object2, "emdi")) {
    stop("Both objects need to be of class emdi.")
  }

  if ((length(object1$ind$Domain) == length(object2$ind$Domain)) &&
    (!all(as.character(object1$ind$Domain) %in%
      as.character(object2$ind$Domain)))) {
    stop("It is only possible to compare emdi objects with the same domains.")
  }

  if ((length(object1$ind$Domain) < length(object2$ind$Domain)) &&
    !all(as.character(object1$ind$Domain) %in%
      as.character(object2$ind$Domain))) {
    stop("The first object contains domains that are
         not contained in the second object. It is only possible to compare
         emdi objects with the same domains.")
  }

  if ((length(object2$ind$Domain) < length(object1$ind$Domain)) &&
    !all(as.character(object2$ind$Domain) %in%
      as.character(object1$ind$Domain))) {
    stop("The second object contains domains that are
         not contained in the first object. It is only possible to compare
         emdi objects with the same domains.")
  }

  if ((MSE == TRUE) && (is.null(object1$MSE) || is.null(object2$MSE))) {
    stop("If MSE is set to TRUE, both emdi objects need to contain MSE
           estimates.")
  }


  if (MSE == FALSE) {
    object1data <- get("ind", object1)
    object2data <- get("ind", object2)
  } else if (MSE == TRUE) {
    object1data <- get("MSE", object1)
    object2data <- get("MSE", object2)
  }

  if (inherits(object1, "fh")) {
    object1data <- object1data[, -4] # remove column Out
  }

  if (inherits(object2, "fh")) {
    object2data <- object2data[, -4] # remove column Out
  }

  order_direct_ebp <- c(
    "Domain", "Mean_1", "Mean_2", "Head_Count_1",
    "Head_Count_2", "Poverty_Gap_1", "Poverty_Gap_2",
    "Gini_1", "Gini_2", "Quintile_Share_1",
    "Quintile_Share_2", "Quantile_10_1", "Quantile_10_2",
    "Quantile_25_1", "Quantile_25_2", "Median_1",
    "Median_2", "Quantile_75_1", "Quantile_75_2",
    "Quantile_90_1", "Quantile_90_2"
  )

  if ((inherits(object1, "ebp") && inherits(object2, "ebp")) ||
    (inherits(object1, "direct") && inherits(object2, "direct")) ||
    (inherits(object1, "direct") && inherits(object2, "ebp")) ||
    (inherits(object1, "ebp") && inherits(object2, "direct"))) {
    if (dim(object1data)[2] > 11) {
      colnames(object1data)[12:dim(object1data)[2]] <-
        paste0(names(object1data[12:dim(object1data)[2]]), "_1")
    }

    if (dim(object2data)[2] > 11) {
      colnames(object2data)[12:dim(object2data)[2]] <-
        paste0(names(object2data[12:dim(object2data)[2]]), "_2")
    }

    data <- merge(object1data, object2data,
      by.x = "Domain", by.y = "Domain",
      suffixes = c("_1", "_2")
    )

    if (dim(data)[2] == 21) {
      data <- data[, order_direct_ebp]
    } else if (dim(data)[2] > 21) {
      custom_indicators <- colnames(data)[(which(!colnames(data) %in%
        order_direct_ebp))]
      data <- data[, c(order_direct_ebp, custom_indicators)]
    }
  } else if (inherits(object1, "fh") && inherits(object2, "fh")) {
    data <- merge(object1data, object2data,
      by.x = "Domain", by.y = "Domain",
      suffixes = c("_1", "_2")
    )
    data <- data[, c("Domain", "Direct_1", "Direct_2", "FH_1", "FH_2")]
  } else if ((inherits(object1, "direct") && inherits(object2, "fh")) ||
    (inherits(object1, "fh") && inherits(object2, "direct"))) {
    data <- merge(object1data, object2data,
      by.x = "Domain", by.y = "Domain",
      all = TRUE, suffixes = c("_1", "_2")
    )
  } else if ((inherits(object1, "fh") && inherits(object2, "ebp")) ||
    (inherits(object1, "ebp") && inherits(object2, "fh"))) {
    data <- merge(object1data, object2data,
      by.x = "Domain", by.y = "Domain",
      all = TRUE, suffixes = c("_1", "_2")
    )
  }
  data
}
