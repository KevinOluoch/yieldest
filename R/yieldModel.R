#' Run mixed effects yield model
#'
#' The function creates and runs a yield estimate model.
#'
#' The function can be used to create a yield estimate model by specifying the
#' model variable as either target variable, random variables or fixed variables
#'
#' @param df dataframe
#' @param targetv target variable
#' @param randomv a character vector of random variables
#' @param fixedv a character vector of fixed variables. Ignored if
#'               use.rest.as.fixed is TRUE
#' @param use.rest.as.fixed boolean: All other variables should be fixed variables
#'                         default is FALSE (Overrides fixedv)
#'
#' @return model object
#'
#' @family Yield estimate functions
#' @export
#'
yieldModel <- function(df, targetv, randomv, fixedv=NULL, use.rest.as.fixedv = FALSE) {

  # Override the fixed variables and used all other columns in the dataframe
  if (use.rest.as.fixedv == TRUE){
    allcols <- base::names(df)
    all.cols.rm.target <- allcols[!allcols %in% targetv]
    all.cols.rm.random <- all.cols.rm.target[!all.cols.rm.target %in% randomv]

    fixedv = all.cols.rm.random
  }

  # Random variables
  # "(1|dd) + (1|ee)"
  randomv.str <- paste(paste0("(1|", randomv, ")"), collapse = " + ")

  # Fixed variables
  # "aa + bb + cc"
  fixedv.str <- paste(fixedv, collapse = " + ")

  # "y ~ aa + bb + cc + (1|dd) + (1|ee)"
  formular.str <- paste( targetv, "~", fixedv.str, "+", randomv.str)


  lme4::lmer(stats::as.formula(formular.str), data = df)

}


#' Run fixed effects yield model
#'
#' The function creates and runs a fixed effects yield estimate model.
#'
#' The function will create a yield estimate model by specifying the
#' target variable and fixed variables
#'
#' @param df dataframe
#' @param targetv target variable
#' @param fixedv a character vector of fixed variables. Ignored if
#'               use.rest.as.fixed is TRUE
#' @param use.rest.as.fixedvboolean: All other variables should be fixed variables
#'                         default is TRUE (Overrides fixedv)
#'
#'
#' @return model object
#' @export
#'
yieldModel_lm <- function(df, targetv, fixedv=NULL, use.rest.as.fixedv = FALSE) {

  # Override the fixed variables and used all other columns in the dataframe
  if (use.rest.as.fixedv == TRUE){
    allcols <- base::names(df)
    all.cols.rm.target <- allcols[!allcols %in% targetv]
    all.cols.rm.random <- all.cols.rm.target[!all.cols.rm.target %in% randomv]

    fixedv = all.cols.rm.random
  }


  # Fixed variables
  # "aa + bb + cc"
  fixedv.str <- paste(fixedv, collapse = " + ")

  # "y ~ aa + bb + cc "
  formular.str <- paste( targetv, "~", fixedv.str)


  stats::lm(stats::as.formula(formular.str), data = df)


}
