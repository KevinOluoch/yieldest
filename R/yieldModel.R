#' Run yield model
#'
#' The function creates and runs a yield estimate model.
#'
#' The function can be used to create a yield estimate model by specifying the
#' model variable as either target variable, random variables or fixed variables
#'
#' @param df dataframe
#' @param targetv target variable
#' @param randomv a character vector of random variables
#' @param fixedv a character vector of fixedd variables
#'
#' @return model object
#'
#' @family Yield estimate functions
#' @export
#'
yieldModel <- function(df, targetv, randomv, fixedv) {

  # Random variables
  # "(1|dd) + (1|ee)"
  randomv.str <- paste(paste0("(1|", randomv, ")"), collapse = " + ")

  # Fixed variables
  # "aa + bb + cc"
  fixedv.str <- paste(fixedv, collapse = " + ")

  # "y ~ aa + bb + cc + (1|dd) + (1|ee)"
  formular.str <- paste( targetv, "~", fixedv.str, "+", randomv.str)


  lme4::glmer(stats::as.formula(formular.str), data = df)


}

#' Run yield model
#'
#' wrapper function for yieldModel.
#'
#' This function takes in the target variable and the fixed variables and
#' considers the remaining variables to be fixed variables
#'
#'
#' @inheritParams yieldModel
#' @param use.rest.as.fixed boolean: All other variables should be fixed variables
#'                         default is TRUE (Overrides fixedv)
#'
#' @return model object
#'
#' @family Yield estimate functions
#' @export
#'
yieldModel2 <- function(df, targetv, randomv, fixedv, use.rest.as.fixed = TRUE) {

  if (use.rest.as.fixed == TRUE){
    allcols <- base::names(df)
    all.cols.rm.target <- allcols[!allcols %in% targetv]
    all.cols.rm.random <- all.cols.rm.target[!all.cols.rm.target %in% randomv]

    fixedv = all.cols.rm.random
  }
  # Call yieldModel
  # print(all.cols.rm.target)
  # print(fixedv)
  yieldModel(df, targetv, randomv, fixedv)

}
