#' dropCol
#'
#' Remove dataframe columns
#'
#' @param df dataframe
#' @param cols names of columns to be removed from df
#'
#' @return df with column names in cols removed
#' @export
dropCol <- function(df, cols = "") {
  # All columns
  allcols <- names(df)

  # Return df without columns in cols vector
  remaincols <- allcols[!allcols %in% cols]
  df[, remaincols]
}


#' dropNaCol
#'
#' Remove dataframe columns with Na values
#'
#' @param df dataframe
#' @param names_only logical, whether to return dataframe or names (default is FALSE)
#'
#' @return df with NA values of names of columns with Na values
#' @export

dropNaCol <- function(df, names_only = FALSE) {
  # All columns
  allcols <- names(df)
  # Columns with NA
  nacols <- allcols[base::colSums(base::is.na(df)) > 0]

  # Return Na column names if names_only is true
  if (names_only == TRUE){
    return(nacols )
  }

  # Return df without Na columns if names_only is false
  completecols <- allcols[base::colSums(base::is.na(df)) == 0]
  df[, completecols]
}
