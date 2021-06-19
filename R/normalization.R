#' normalization
#'
#' Normalize the columns
#'
#' Will normalize the  specified columns in the df via the specified method
#'
#' @param df dataframe
#' @param method Normalization method
#' @param cols vector of df columns to be normalized, must be one of
#'     'standardization', 'centering', 'unit-range', 'squares',
#'     'clipping' or 'logarithmic'
#'
#' @return df with specified columns normalized
#' @export

normalization <- function(df, cols, method = "standardization"){



  # subtract mean, then divide by standard deviation
  if (method == "standardization"){

    for (col_ in cols) {
      # Check if column is numeric
      if (!is.numeric(df[, col_])) {yieldest::normalization_warn(method, col_); next}

      df[, col_] <- ((df[, col_] - base::mean(df[, col_], na.rm = TRUE))/
                       stats::sd(df[, col_], na.rm = TRUE))
    }

    # subtract mean
    } else if(method == "centering"){
      for (col_ in cols) {
        # Check if column is numeric
        if (!is.numeric(df[, col_])) {yieldest::normalization_warn(method, col_); next}

        df[, col_] <- (df[, col_] - base::mean(df[, col_], na.rm = TRUE))
                       }
    # subtract the minimum value and divide by range to have unit range
    } else if(method == "unit-range"){
      for (col_ in cols) {
        # Check if column is numeric
        if (!is.numeric(df[, col_])) {yieldest::normalization_warn(method, col_); next}

        df[, col_] <- ((df[, col_]- base::min(df[, col_], na.rm = TRUE))/
          (base::max(df[, col_] , na.rm = TRUE)- base::min(df[, col_],
                                                           na.rm = TRUE)))
      }

    } else if(method == "squares"){
      for (col_ in cols) {
        # Check if column is numeric
        if (!is.numeric(df[, col_])) {yieldest::normalization_warn(method, col_); next}

        df[, col_] <- df[, col_] ** 2
      }

    } else if(method == "clipping"){
      print("clipping not implemented ")

    } else if(method == "logarithmic"){
      for (col_ in cols) {
        # Check if column is numeric
        if (!is.numeric(df[, col_])) {yieldest::normalization_warn(method, col_); next}
        df[, col_] <- base::log(df[, col_])
      }

    } else{
    print("method should be one of 'centering', unit-range', 'squares',
                 'clipping' or 'logarithmic'")}

  df
}
