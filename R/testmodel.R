#' Make predictions
#'
#' Use the moddel from the main function to make predictions
#'
#'
#'
#' @return dataframe with computed and observed values
#' @export
#'
#'
testmodel <- function() {
  mainresult <- main()
  data.frame(computed=stats::predict(mainresult[["model"]],
                                     newdata = mainresult[["modeldata"]]),
             observed=mainresult[["modeldata"]]["maize_pro"])
}
