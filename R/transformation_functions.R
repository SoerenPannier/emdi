# Internal documentation -------------------------------------------------------

# Various transformation functions ---------------------------------------------

# Fuction data_transformation transforms the dependent variable depending on
# the choice of transformation. See below for different transformations.
# The return is a data frame with transformed dependent variable, all others
# equal to sample data and a shift parameter m.


# External documentation -------------------------------------------------------

#' Tranforms dependent variables
#'
#' Function \code{data_transformation} transforms the dependent variable from 
#' the formula object \code{fixed} in the given sample data set. Thus, it 
#' returns the original sample data set with transformed dependent variable. 
#' For the transformation three types can be chosen, particularly no, natural 
#' log and Box-Cox transformation.  
#'
#' @param fixed a two-sided linear formula object describing the
#' fixed-effects part of the nested error linear regression model with the
#' dependent variable on the left of a ~ operator and the explanatory
#' variables on the right, separated by + operators. The argument corresponds
#' to the argument \code{fixed} in function \code{\link[nlme]{lme}}.
#' @param smp_data a data frame that needs to comprise all variables named in
#' \code{fixed}. If transformed data is further used to fit a nested error
#' linear regression model \code{smp_data} also needs to comprise the variable
#' named in \code{smp_domains} (see \code{\link{ebp}}).
#' @param transformation a character string. Three different transformation
#' methods for the dependent variable can be chosen (i) no transformation ("no");
#' (ii) natural log transformation ("log"); (iii) Box-Cox transformation
#' ("box.cox").
#' @param lambda a scalar parameter that determines the Box-Cox transformation. 
#' In case of no and natural log transformation \code{lambda} can be set to NULL.
#' @return a named list with two elements, a data frame containing the data set
#' with transformed dependent variable (\code{transformed_data}) and a shift 
#' parameter \code{shift} if present. In case of no transformation the original 
#' data frame is returned and the shift parameter is NULL.
#' @details For the natural log and Box-Cox transformation the dependent variable 
#' is shifted such that all values are greater than zero since the transformations 
#' are not applicable for values equal to or smaller than zero. The shift is 
#' calculated as follows: 
#'   \deqn{shift = |min(y)| + 1 if min(y) <= 0}
#' Function \code{data_transformation} works as a wrapper function. This means
#' that the function manages the selection of the three different transformation
#' functions \code{no_transform},\code{log_transform} and \code{box_cox}. 
#' @seealso \code{\link[nlme]{lme}}
#' @examples
#' # Loading data - sample data
#' data("eusilcA_smp")
#'
#' # Transform dependent variable in sample data with Box-Cox transformation
#' transform_data <- data_transformation(eqIncome ~ gender + eqsize + cash + 
#' self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
#' fam_allow + house_allow + cap_inv + tax_adj, eusilcA_smp, "box.cox", 0.7)
#' @export
#' @import nlme

data_transformation <- function(fixed,
                                smp_data,
                                transformation,
                                lambda) {

  y_vector <- as.vector(smp_data[paste(fixed[2])])

  transformed <- if (transformation == "no") {
    no_transform(y = y_vector, shift = NULL)
  } else if (transformation == "log") {
    log_transform(y = y_vector, shift = 0)
  } else if (transformation == "box.cox") {
    box_cox(y = y_vector, lambda = lambda, shift = 0)
  }

  smp_data[paste(fixed[2])] <- transformed$y

  return(list(transformed_data = smp_data, shift = transformed$shift))
} # End data_transformation


# Following functions are only internal ----------------------------------------

# Function std_data_transformation only returns a data frame with transformed
# dependent variable.

std_data_transformation <- function(fixed=fixed,
                                    smp_data,
                                    transformation,
                                    lambda) {

  y_vector <- as.matrix(smp_data[paste(fixed[2])])

  std_transformed <- if (transformation == "box.cox"){
    as.data.frame(box_cox_std(y = y_vector, lambda = lambda))
    } else if (transformation == "log") {
    smp_data[paste(fixed[2])]
    } else if (transformation == "no") {
    smp_data[paste(fixed[2])]
    }

  smp_data[paste(fixed[2])] <- std_transformed
  return(transformed_data=smp_data)
} # End std_data_transformation


# Back transformation function -------------------------------------------------

back_transformation <- function(y, transformation, lambda, shift) {
  back_transformed <- if (transformation == "no") {
    no_transform_back(y = y)
  } else if (transformation == "log") {
    log_transform_back(y = y, shift = shift)
  } else if (transformation == "box.cox") {
    box_cox_back(y = y, lambda = lambda, shift = shift)
  }
  return(y = back_transformed)
} # End back_transform


# Transformation types ---------------------------------------------------------

# No transformation ------------------------------------------------------------

# Transformation: no transformation
no_transform <- function(y, shift = NULL) {
  return(list(y = y, shift = NULL))
} # End no-transform


# Back transformation: no transformation
no_transform_back <- function(y) {
  return(y = y)
}

# Log transformation -----------------------------------------------------------

# Transformation: log
log_transform <- function(y, shift = 0) {
  min <- min(y)
  if (min <= 0) {
    shift <- abs(min) + 1
    y <- y + shift
  }
  y <- log(y)
  return(list(y = y, shift = shift))
} # End log_transform


# Back transformation: log
log_transform_back <- function(y, shift = 0) {
  y <- exp(y) - shift
  return(y = y)
} # End log_transfom_back


# Box Cox ----------------------------------------------------------------------

# Transformation: Box Cox
box_cox <- function(y, lambda = lambda, shift = 0) {
  with_shift <- function(y, shift) {
    min <- min(y)
    if (min <= 0) {
      shift <- shift + abs(min(y)) +1
    } else {
      shift <- shift
    }
    return(shift)
  }
  # Shift parameter
  shift <- with_shift(y = y, shift = shift)

  lambda_cases <- function(y, lambda = lambda) {
    lambda_absolute <- abs(lambda)
    if (lambda_absolute <= 1e-12) {  #case lambda=0
      y <- log(y + shift)
    } else {
      y <- ((y + shift)^lambda - 1) / lambda
    }
    return(y)
  }
  y <- lambda_cases(y = y, lambda = lambda)

  return(list(y = y, shift = shift))
} # End box_cox



# Standardized transformation: Box Cox

geometric.mean <- function(x) { #for RMLE in the parameter estimation

  exp(fast_mean(log(x)))
}

box_cox_std <- function(y, lambda) {
  min <- min(y)
  if (min <= 0) {
    y <- y - min + 1
  }

  gm <- geometric.mean(y)
  y <- if(abs(lambda) > 1e-12) {
    y <- (y^lambda - 1) / (lambda * ((gm)^(lambda - 1)))
  } else {
    y <- gm * log(y)
  }
  return(y)
}


# Back transformation: Box Cox
box_cox_back <- function(y, lambda, shift = 0) {

  lambda_cases_back <- function(y, lambda = lambda, shift){
    if (abs(lambda) <= 1e-12) {   #case lambda=0
      y <-  exp(y) - shift
    } else {
      y <- (lambda * y + 1)^(1 / lambda) - shift
    }
    return(y = y)
  }
  y <- lambda_cases_back(y = y, lambda = lambda, shift = shift)

  return(y = y)
} #  End box_cox_back

