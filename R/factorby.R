#' Factor by reference value
#'
#' Convert column to factors with the specified value as the reference value
#'
#' @param df object of class data.frame
#' @param cols vector of column names to be converted to factors
#' @param refvalue vector of reference values: same length as cols
#'
#' @return data.frame with specified columns factored
#' @export
#'
factorby <- function(df, cols, refvalues) {
  # Number of columns and ref rows should be equal
  if (base::length(cols) != base::length(refvalues)) factors_warn1(refvalues, cols)

  for (i in 1:length(cols)) {
    col_ <- cols[i]
    refvalue <- refvalues[i]

    uniquevalues <- base::unique(df[, col_])
    if (!refvalue %in% uniquevalues) factors_warn2(refvalues, col_)

    # convert to levels
    df[, col_] <- base::as.factor(df[, col_])

    # set the refrence level
    df[, col_] <- stats::relevel(df[, col_], ref = refvalue)

  }
  df
}

#' Factor with mode as reference value
#'
#' Convert column to factors with the mode value as the reference value
#'
#' @param df object of class data.frame
#' @param cols vector of column names to be converted to factors
#'
#' @return data.frame with specified columns factored
#' @export
#'
factorby_occur <- function(df, cols) {
  for (col_ in cols) {

    # convert to levels
    df[, col_] <- base::as.factor(df[, col_])

    # Get refrence level (most occurence)
    refvalue <- utils::tail(base::names(base::sort(base::table(df[, col_]))), 1)
    # set the refrence level
    df[, col_] <- stats::relevel(df[, col_], ref = refvalue)

  }
  df
}
