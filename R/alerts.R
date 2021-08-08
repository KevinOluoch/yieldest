

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

model_selection.error1 <- function(step.direction) {
  # Stop if the step direction is not one of: "forward" or "backward"
  base::stop(paste0('step.direction, must be one of: "forward" or "backward". The current value is :', step.direction))

}


#' Progress report
#'
#' Show progress of a process
#'
#' @param i Current count of items i
#' @param total_i The number of items i
#' @param starttime The time the process started
#' @param Message_ Short message describing the process
#'
#' @return NULL
#' @export
progressReport <- function(i, total_i, starttime, Message_ = "Progress Report:") {

  time.remain <- base::difftime(Sys.time(),
                                starttime,
                                units = "secs") * ((total_i -  i)/i)

  hrs.remain <- base::floor(time.remain/3600)
  min.remain <- base::floor((time.remain - (hrs.remain*3600))/60)
  sec.remain <- base::floor(time.remain  - (hrs.remain*3600) -(min.remain*60))
  # Show progress and remaining time
  cat("\r", paste(Message_, " Progress ",
                  100*base::round(i/total_i, 2), "%",
                  ". Approximate remaining time is",
                  hrs.remain, " Hours  ",
                  min.remain, " minutes  ",
                  sec.remain, " seconds "
  ))

  if (i >= total_i){
    time.remain <- base::difftime(Sys.time(), starttime, units = "secs")

    hrs.remain <- base::floor(time.remain/3600)
    min.remain <- base::floor((time.remain - (hrs.remain*3600))/60)
    sec.remain <- base::floor(time.remain  - (hrs.remain*3600) -(min.remain*60))
    # Show progress and final time
    cat("\r", paste(Message_, " Progress ",
                    100*base::round(i/total_i, 2), "% Complete",
                    ". Total time: ",
                    hrs.remain, " Hours  ",
                    min.remain, " minutes  ",
                    sec.remain, " seconds "
    ))
  }

}
