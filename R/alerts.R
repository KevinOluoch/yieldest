


#' Alerts to the user
#'
#' Create warning for normalization function
#'
#' Given the name of a method an a column, this function will create a warmning
#'  message stating that the column has not been normalized since it is non-numeric.
#'
#' @param method
#' @param col_
#'
#' @return NULL
normalization_warn <- function(method, col_) {
  # check if column is numeric
   base::warning(paste0(method, " normalization has not been applied to column '",
                        col_, "'. It is non-numeric"))

}
