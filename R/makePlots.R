caterpillarPlot <- function(df) {
  ggmcmc::ggs_caterpillar(ggs(s))

}

#' daily values to average daily values
#'
#' Calculate the average daily values for the selected years. For each day of
#' the year, get the mean value over the years given.
#'
#' @param list.of.df A list of dataframes. each data frame containdns the daily value
#'
#' @return a dataframe over average daily values for every day of the year
#' @export
DailytoYearyAve <- function(list.of.df ) {

  matrixsum <- feb29thsum_1 <- feb29thsum <- NULL
  j <- leapyrcount <- 0
  for (i in list.of.df){

    j <- j + 1
    df <- as.data.frame(i)
    matrix1 <- data.matrix(df)

    matrix1.colnames <- colnames(matrix1)
    matrix2 <- matrix1[, matrix1.colnames[!grepl("02.29", matrix1.colnames)]]
    LeapFeb29 <- matrix1[, matrix1.colnames[grepl("02.29", matrix1.colnames)]]

    # If Leap year, sum feb 29th separately

    if (length(LeapFeb29 ) > 0){

      if(is.null(feb29thsum)){
        feb29thsum <-  LeapFeb29
        feb29thsum_name <- matrix1.colnames[grepl("02.29", matrix1.colnames)]
      } else{
        feb29thsum <-  feb29thsum + LeapFeb29
      }

      leapyrcount <- leapyrcount + 1

    }


    # If this is the first loop assign
    if (is.null(matrixsum)) {
      matrixsum <-  matrix2
      next
    }

    # If this is a subsequent loop add to existing values
    matrixsum <- matrixsum + matrix2

  }


  # Add the february 29th data
  matrixsum <- matrixsum/length(list.of.df)
  if(length(feb29thsum_1) == 1){
    feb29thsum_1 <- feb29thsum/leapyrcount
    output <- c(feb29thsum_1, matrixsum)
  } else if(length(feb29thsum_1) > 1){
    feb29thsum_1 <- feb29thsum/leapyrcount
    output <- cbind(feb29thsum_1, matrixsum)
  } else {
    output <- matrixsum
  }


  # colnames(output) <- c(feb29thsum_name, colnames(matrixsum))


  output

}


Daily_List2df <- function(list.of.df ) {

  dfall <- NULL
  j  <- 0
  for (i in list.of.df){
    j <- j + 1
    # df <- as.data.frame(i)
    df1 <- data.frame(i)
    df1.colnames <- colnames(df1)

    df2 <- df1
    # If it is not leap year, add feb29th as NAS
    if(all(!grepl("02.29", df1.colnames))){
      X1981.02.29 <- rep(NA, dim(df1)[1])
      df2 <- data.frame(df1[1:59], X1981.02.29, df1[60:dim(df1)[2]])

      print("j")
    }

    print(dim(df2))
    # add the year column
    year_value <- stringr::str_extract(names(list.of.df)[j], "[[:digit:]]{4}" )
    # print(rep(year_value, dim(df2)[1]))
    df3 <- cbind(Year = rep(year_value, dim(df2)[1]),
                 df2)


    print(names(list.of.df)[j])

    # If this is the first loop assign
    if (is.null(dfall)) {
      dfall <-  df3
      next
    }

    # If this is a subsequent loop add to existing values
    # print(colnames(dfall))
    # print(colnames(df3))
    colnames(df3) <- colnames(dfall)
    dfall <- rbind(dfall, df3)
    # print(dfall[1])

  }

  # data.matrix()
  rownames(dfall) <- dfall[,1]
  dfall[,2:length(colnames(dfall))]

}



violinPlot <- function(df, allmean = 0, plot.title = "Violin Plots",
                       x_label = "X", y_label = "Y", label = "0") {
  if (is.list(df)) {

    print("list")
    print(rownames(df))
    df1 <- cbind(admin.units=rownames(df), as.data.frame(df))
    df2 <- reshape2::melt(df1, id = "admin.units")

  } else{
    df2 <- data.frame(cbind(admin.units= factor(rep(label, length(df)),
                                                levels = label), value=df))
    print("vector")

  }


  # print(df)
  p <-ggplot2::ggplot(df2,
                      ggplot2::aes(x=admin.units, y=value)) +
    ggplot2::geom_violin()+
    ggplot2::geom_hline(yintercept=allmean, linetype="dashed", color = "red") +
    ggplot2::labs(title=plot.title,
                   x = x_label, y = y_label)
  return(p)
}


violinPlot2 <- function(p, df, label = "0", id_ = "admin.units") {
  if (is.vector(df)) {
    df2 <- data.frame(cbind(admin.units= factor(rep(label, length(df)), levels = label),
                            value=df))
    # print(df2)

  } else{

    df1 <- cbind(admin.units=rownames(df), as.data.frame(df))

    df2 <- reshape2::melt(df1, id = id_)
  }

  p <- p + ggplot2::geom_violin(data = df2, ggplot2::aes(x=admin.units, y=value)) +

    return(p)
}
