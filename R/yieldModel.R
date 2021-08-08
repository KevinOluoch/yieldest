#' Run mixed effects yield model
#'
#' The function creates and runs a yield estimate model.
#'
#' The function can be used to create a yield estimate model by specifying the
#' model variable as either target variable, random variables or fixed variables
#'
#' @param df dataframe
#' @param targetV target variable
#' @param randomV a character vector of random variables
#' @param fixedV a character vector of fixed variables. Ignored if
#'               use.rest.as.fixed is TRUE
#' @param use.rest.as.fixed boolean: All other variables should be fixed variables
#'                         default is FALSE (Overrides fixedV)
#'
#' @return model object
#'
#' @family Yield estimate functions
#' @export
#'
yieldModel <- function(df, targetV, randomV, fixedV=NULL, use.rest.as.fixedv = FALSE) {

  # Override the fixed variables and used all other columns in the dataframe
  if (use.rest.as.fixedv == TRUE){
    allcols <- base::names(df)
    all.cols.rm.target <- allcols[!allcols %in% targetV]
    all.cols.rm.random <- all.cols.rm.target[!all.cols.rm.target %in% randomV]

    fixedV = all.cols.rm.random
  }



  if(is.null(randomV)){

    output <- yieldModel_lm(df, targetV=targetV, fixedV=fixedV)

  } else{

    # Random variables
    # "(1|dd) + (1|ee)"
    randomV.str <- paste(paste0("(1|", randomV, ")"), collapse = " + ")


    # Fixed variables
    # "aa + bb + cc"
    fixedV.str <- paste(fixedV, collapse = " + ")

    # "y ~ aa + bb + cc + (1|dd) + (1|ee)"
    formular.str <- paste( targetV, "~", fixedV.str, "+", randomV.str)
    # print(formular.str)
    output <- lme4::lmer(stats::as.formula(formular.str), data = df)

  }

  output
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
