

#' Alerts to the user
#'
#' Create warning for normalization function
#'
#' Given the name of a method an a column, this function will create a warmning
#'  message stating that the column has not been normalized since it is non-numeric.
#'
#' @param method Normalization method
#' @param col_ Column with the error
#'
#' @return NULL
normalization_warn <- function(method, col_) {
  # check if column is numeric
   base::warning(paste0(method, " normalization has not been applied to column '",
                        col_, "'. It is non-numeric"))

}

factors_warn1 <- function(refvalue, col_) {
  # check if column is numeric
  base::stop(paste0("Number of columns and ref rows should be equal!",
                   "There are ", base::length(refvalue),
                   " reference values and ", col_, " columns"))

}

factors_warn2 <- function(refvalue, col_) {
  # check if column is numeric
  base::stop(paste0(refvalue, " in not in column ", col_))

}


