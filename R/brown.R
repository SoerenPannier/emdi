#' Brown function
#'
#' This function assesses the quality of the model-based estimates by comparing 
#' them with the direct estimates based on a goodness-of-fit test proposed by
#' Brown et al. (2001).
#'
#' @param object fitted FH model.
#' @return The value W of the test statistic, the degrees of freedom and the p value.
#' @references 
#' Brown, G., R. Chambers, P. Heady, and D. Heasman (2001). Evaluation of small 
#' area estimation methods: An application to unemployment estimates from the UK
#' LFS. Symposium 2001 - Achieving Data Quality in a Statistical Agency: A 
#' Methodological Perspective, Statistics Canada.
#' @export

brown <- function(object, ...) UseMethod("brown")


#' Brown function
#'
#' This function assesses the quality of the model-based estimates by comparing 
#' them with the direct estimates based on a goodness-of-fit test proposed by
#' Brown et al. (2001).
#'
#' @param object fitted FH model.
#' @return The value W of the test statistic, the degrees of freedom and the p value.
#' @references 
#' Brown, G., R. Chambers, P. Heady, and D. Heasman (2001). Evaluation of small 
#' area estimation methods: An application to unemployment estimates from the UK
#' LFS. Symposium 2001 - Achieving Data Quality in a Statistical Agency: A 
#' Methodological Perspective, Statistics Canada.
#' @export

brown.fh <- function(object){

  W_BL <- sum((object$ind$Direct[object$ind$Out == 0] - 
                 object$ind$FH[object$ind$Out == 0])^2 /
              (object$MSE$Direct[object$MSE$Out == 0] + 
                 object$MSE$FH[object$MSE$Out == 0]))
  
 # Degress of freedom
 df_BL <- object$framework$N_dom_smp
 
 # p Value
 p_value_BL <- 1 - pchisq(W_BL, df_BL)
 
 testresults <- data.frame(W.value = W_BL,
                           Df = df_BL,
                           p.value = p_value_BL)
 class(testresults) <- "brown.fh"
 return(testresults)
}

#' Prints brown.fh objects
#'
#' @param x an object of type "brown.fh".
#' @param ... further arguments passed to or from other methods.
#' @export

print.brown.fh <- function(x)
{
  cat("Brown Test: ","\n")
  print(data.frame(W.value = x$W,
                   Df = x$Df,
                   p.value = x$p.value,
                   row.names = ""))
}

#' Synthetic vs direct 
#'
#' This function computes the correlation between the regression-synthetic part
#' of the Fay-Herriot model and the direct estimates.
#'
#' @param object fitted FH model.
#' @return Correlation coefficient.
#' @export

syndir <- function(object, ...) UseMethod("syndir")


#' Synthetic vs direct 
#'
#' This function computes the correlation between the regression-synthetic part
#' of the Fay-Herriot model and the direct estimates.
#'
#' @param object fitted FH model.
#' @return Correlation coefficient.
#' @export

syndir.fh <- function(object) {

  # Extraction of the regression part
  xb <- (object$ind$FH[object$ind$Out == 0] - 
           object$model$gamma$Gamma *
           object$ind$Direct[object$ind$Out == 0]) /
          (1 - object$model$gamma$Gamma)
  # Direkt estimator
  direct_insample <- object$ind$Direct[object$ind$Out == 0]
  # Correlation
  syndircor <- cor(xb, direct_insample)
  class(syndircor) <- "syndir.fh"
  return(syndircor)
}

#' Prints syndir.fh objects
#'
#' @param x an object of type "syndir.fh".
#' @param ... further arguments passed to or from other methods.
#' @export

print.syndir.fh <- function(x)
{
  cat("Correlation between synthetic part and direct estimator: ",x,"\n")
}



