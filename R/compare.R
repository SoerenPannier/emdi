#' Compare function
#'
<<<<<<< HEAD
#' For all indicators or a selection of indicators two plots are returned. The 
#' first plot is a scatter plot of the direct and model-based point estimates 
#' and the second is a line plot with both point estimates. 
#' @param direct an object of type "emdi","direct", representing point and MSE
#' estimates. 
#' @param model an object of type "emdi","model", representing point and MSE
#' estimates. 
#' @param indicator optional character vector that selects which indicators
#' shall be returned: (i) all calculated indicators ("all");
#' (ii) each indicator name: "Mean", "Quantile_10", "Quantile_25", "Median",
#' "Quantile_75", "Quantile_90", "Head_Count", 
#' "Poverty_Gap", "Gini", "Quintile_Share" or the function name/s of 
#' "custom_indicator/s"; (iii) groups of indicators: "Quantiles", "Poverty", 
#' "Inequality" or "Custom".If two of these groups are selected, only the first
#' one is returned. Defaults to "all". Note, additional custom indicators can be 
#' defined as argument for model-based approaches (see also \code{\link{ebp}}) 
#' and do not appear in groups of indicators even though these might belong to 
#' one of the groups.
#' @param label argument that enables to customize title and axis labels. There 
#' are three options to label the evaluation plots: (i) original labels ("orig"), 
#' (ii) axis labels but no title ("no_title"), (iii) neither axis 
#' labels nor title ("blank").  
#' @param color a vector with two elements. The first color determines
#' the color of the line in the scatter plot and the color for the direct 
#' estimates in the line plot. The second color specifies the color of the line
#' for the model-based estimates.
#' @param shape a numeric vector with two elements. The first shape determines
#' the shape of the points in the line plot for the direct estimates and the 
#' second shape for the model-based estimates. The options are numbered from 
#' 0 to 25. 
#' @param line_type a character vector with two elements. The first line type 
#' determines the type of the line for the direct estimates and the 
#' second type for the model-based estimates. The options are: "twodash", 
#' "solid", "longdash", "dotted", "dotdash", "dashed" and "blank". 
#' @param gg_theme \code{\link[ggplot2]{theme}} list from package \pkg{ggplot2}.
#' For using this argument, package \pkg{ggplot2} must be loaded via 
#' \code{library(ggplot2)}. See also Example 2.
#' @return A scatter plot and a line plot comparing direct and model-based 
#' estimators for each selected indicator obtained by \code{\link[ggplot2]{ggplot}}.
#' @seealso \code{\link{emdiObject}}, \code{\link{direct}}, \code{\link{ebp}} 
#' @examples
#' \dontrun{
#' # Loading data - population and sample data
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'   
#' # Generation of two emdi objects
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + 
#' self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
#' fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#' threshold = function(y){0.6 * median(y)}, L = 50, MSE = TRUE,
#' na.rm = TRUE, cpus = 1)
#' 
#' emdi_direct <- direct(y = "eqIncome", smp_data = eusilcA_smp,
#' smp_domains = "district", weights = "weight", threshold = 11161.44,
#' var = TRUE, boot_type = "naive", B = 50, seed = 123, na.rm = TRUE)
#' 
#' # Example 1: Receive first overview
#' compare_plot(direct = emdi_direct, model = emdi_model)
#' 
#' # Example 2: Change plot theme
#' library(ggplot2)
#' compare_plot(emdi_direct, emdi_model, indicator = "Median",
#' gg_theme = theme(axis.line = element_line(size = 3, colour = "grey80"),
#' plot.background = element_rect(fill = "lightblue3"),
#' legend.position = "none"))
#' }
=======
#' Function \code{compare} is a generic function used to assess the quality of 
#' the model-based estimates by comparing them with the direct estimates.
#'
#' @param model an object of type "emdi","model".
#' @param ... further arguments passed to or from other methods.
#' @return The return of \code{compare} depends on the class of its argument. The
#' documentation of particular methods gives detailed information about the
#' return of that method.
>>>>>>> fh_integration
#' @export

compare <- function(model, ...) UseMethod("compare")


#' Compare function
#'
#' Method \code{compare.fh} assesses the quality of the model-based estimates by 
#' comparing them with the direct estimates based on a goodness-of-fit test 
#' proposed by \cite{Brown et al. (2001)} and by computing the correlation between the 
#' regression-synthetic part of the Fay-Herriot model and the direct estimates.
#'
#' @param model an object of type "model","fh".
#' @param ... further arguments passed to or from other methods.
#' @return The null hypothesis, the value W of the test statistic, the degrees 
#' of freedom and the p value of the Brown test; and the correlation coefficient 
#' of the synthetic part and the direct estimator \cite{(Chandra et al. 2015)}.
#' @references 
#' Brown, G., R. Chambers, P. Heady, and D. Heasman (2001). Evaluation of small 
#' area estimation methods: An application to unemployment estimates from the UK
#' LFS. Symposium 2001 - Achieving Data Quality in a Statistical Agency: A 
#' Methodological Perspective, Statistics Canada. \cr \cr
#' Chandra, H., Salvati, N. and Chambers, R. (2015), A Spatially 
#' Nonstationary Fay-Herriot Model for Small Area Estimation, Journal 
#' of the Survey Statistics and Methodology, 3, 109-135.
#' @export
#' @importFrom stats cor pchisq

compare.fh <- function(model, ...){

  if(!inherits(model, "fh")){
    stop('Object needs to be of class fh.')
  }
  if(is.null(model$MSE$FH)){
     testresults <- NULL
      cat('The fh object does not contain MSE estimates. The Brown test 
          statistic cannot be computed.', "\n")
   } else {
  W_BL <- sum((model$ind$Direct[model$ind$Out == 0] - 
                 model$ind$FH[model$ind$Out == 0])^2 /
              (model$MSE$Direct[model$MSE$Out == 0] + 
                 model$MSE$FH[model$MSE$Out == 0]))
  
 # Degress of freedom
 df_BL <- model$framework$N_dom_smp
 
 # p Value
 p_value_BL <- 1 - pchisq(W_BL, df_BL)
 
 testresults <- data.frame(W.value = W_BL,
                           Df = df_BL,
                           p.value = p_value_BL)
   }
 if (model$method$method == "reblupbc"){
   results <- list(Brown = testresults)
   cat("Please note that for the bias-corrected robust EBLUP ('reblupbc') only 
       the goodness-of-fit test proposed by Brown et al. (2001) is provided and 
       not the correlation coefficient of the synthetic part and the direct 
       estimator. \n")
 } else {
   # Extraction of the regression part
   if (!is.null(model$model$gamma)){
     xb <- (model$ind$FH[model$ind$Out == 0] - 
              model$model$gamma$Gamma *
              model$ind$Direct[model$ind$Out == 0]) /
       (1 - model$model$gamma$Gamma) 
   } 
   if (is.null(model$model$gamma)){
     xb <- model$ind$FH[model$ind$Out == 0] - 
       model$model$random_effects
   }
   
   
   # Direkt estimator
   direct_insample <- model$ind$Direct[model$ind$Out == 0]
   # Correlation
   syndircor <- cor(xb, direct_insample)
   
   results <- list(Brown = testresults, syndir = syndircor)
 }
 
 class(results) <- "compare.fh"
 
 if (model$framework$N_dom_unobs > 0) {
   cat("Please note that the computation of both test statistics is only based 
       on in-sample domains.","\n")
 }
 return(results)
}

#' Prints compare.fh objects
#' 
#' compare.fh object is printed.
#'
#' @param x an object of type "compare.fh".
#' @param ... further arguments passed to or from other methods.
#' @export

print.compare.fh <- function(x, ...)
{
   if(!(is.null(x$Brown))){
      cat("Brown test","\n")
      cat("\n")
      cat("Null hypothesis: EBLUP estimates do not differ significantly from the 
      direct estimates","\n")
      cat("\n")
      print(data.frame(W.value = x[[1]]$W,
                       Df = x[[1]]$Df,
                       p.value = x[[1]]$p.value,
                       row.names = ""))
      if (length(x) == 2){
         cat("\n")
         cat("Correlation between synthetic part and direct estimator: ", 
             round(x[[2]],2),"\n")
      }
   } else {
      if (length(x) == 2){
         cat("\n")
         cat("Correlation between synthetic part and direct estimator: ", 
             round(x[[2]],2),"\n")
      }
   }
  
}




