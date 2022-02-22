
#' Fixed Effects model
#'
#' @param df dataframe with target variable and fixed variables
#' @param target The name of the target variable column
#' @param fixedv A vector of fixed variables names. If null all the other
#'               columns in the dataframe
#'
#' @return Fixed effects model
#'
#' @export

fixedEffetsModel <- function(df, targetv, fixedv=NULL) {
  # Use all other columns in the dataframe as fixed variables if no fixed
  # variables are provided
  if (is.null(fixedv)){

    allcols <- colnames(df)

    fixedv <- allcols[!allcols %in% targetv]
  }


  # Create a linear Fixed-Effects model
  # lm.model <- yieldest::yieldModel_lm(df = df, targetV = targetv, fixedV = fixedv)

  # summary1 <- summary(lm.model)
  #
  # summary1$coefficients
  # summary1
  # Return a linear Fixed-Effects model
  yieldest::yieldModel_lm(df = df, targetV = targetv, fixedV = fixedv)
}



#' Run fixed effects yield model
#'
#' The function creates and runs a fixed effects yield estimate model.
#'
#' The function will create a yield estimate model by specifying the
#' target variable and fixed variables
#'
#' @param df dataframe
#' @param targetV target variable
#' @param fixedV a character vector of fixed variables. Ignored if
#'               use.rest.as.fixed is TRUE
#' @param use.rest.as.fixedvboolean: All other variables should be fixed variables
#'                         default is TRUE (Overrides fixedv)
#'
#'
#' @return model object
#' @export
#'
yieldModel_lm <- function(df, targetV, fixedV=NULL, use.rest.as.fixedv = FALSE) {

  # Override the fixed variables and used all other columns in the dataframe
  if (use.rest.as.fixedv == TRUE){
    allcols <- base::names(df)

    fixedV <- allcols[!allcols %in% targetV]
  }


  # Fixed variables
  # "aa + bb + cc"
  fixedV.str <- paste(fixedV, collapse = " + ")

  # "y ~ aa + bb + cc "
  formular.str <- paste( targetV, "~", fixedV.str)


  stats::lm(stats::as.formula(formular.str), data = df)


}
