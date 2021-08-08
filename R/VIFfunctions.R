#' VIF functions
#'
#' A function that calculates the Variance Inflation Factor (VIF) values
#' using the function fmsb::VIF()and performs operations relating to VIF
#'
#' @param df dataframe
#' @param cols2ignore a vector of column names: colnames not included when
#'            calculating VIF values.
#' @param ... other arguments passed to fmsb::VIF()
#'
#' @return
#' @export
calculateVIF <- function(df, targetV = NULL, randomV = NULL, ...){

  if(class(df) != 'data.frame') df <- data.frame(df)

  #get initial vif value for all comparisons of variables
  vif.values <- NULL
  df.colnames <- base::names(df)
  df.colnames <- df.colnames[!df.colnames %in% c(targetV, randomV )]
  for(col_ in df.colnames){

    regressors <- df.colnames[!df.colnames %in% col_]
    formula_0 <- paste(regressors, collapse = '+')
    formula_ <- stats::formula(paste(col_, '~', formula_0))
    vif0 <- fmsb::VIF(stats::lm(formula_, data = df, ...))

    vif.values <-rbind(vif.values, c(col_, as.numeric(vif0)))
    }

  colnames(vif.values) <- c("colnames", "VIF")

  vif.values
  }


#' Back select Independent variables
#'
#' Backwards selection of explanatory variables, stops when all VIF values are
#' below the 'threshold'
#'
#'
#' @param df Dataframe with the Independent variables
#' @param threshold VIF value selection threshold
#' @param trace Boolean: wheter the details of each selection step should be
#'              sent to standard output
#' @param ...
#'
#' @return
#' @export
#'
backselect <- function(df, targetV, randomV = NULL, untouched=NULL,
                       threshold = 10, trace = T, ...) {
  df1 <- df

  # Leave out "untouched" variables
  if (!is.null(untouched)) {
    df1 <- df1[,names(df1)[!names(df1) %in% untouched]]
  }

  repeat {

    # Calculate VIF values
    vif.values <- yieldest::calculateVIF(df1, targetV, randomV)

    # Get the highest VIF value
    # vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
    vif_max <- base::max(base::as.numeric(vif.values[, "VIF"]), na.rm = TRUE)

    # Print the maximun VIF value (if Trace is TRUE) and stop the backward
    # selection (exit the while loop)
    if(vif_max < threshold){
      if(trace==T){ #print output of each iteration
        prmatrix(vif.values) #, collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
        cat('\n')
        cat(paste('All variables have VIF < ', threshold,
                  ', max VIF ',round(vif_max,2), sep=''),
            '\n\n')
      }

      vif_remain.col <- vif.values[, "colnames"][!vif.values[, "VIF"] %in% vif_max]
      break

    }

    # Get the name of the column with the max value.
    vif_max.col <- vif.values[, "colnames"][vif.values[, "VIF"] %in% vif_max]

    if(trace==T){ #print output of each iteration
      # prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      prmatrix(vif.values)
      cat('\n')
      cat(paste('All variables have VIF > ', threshold,
                ', max VIF "', vif_max.col, '" : ', round(vif_max,2), sep=''),
          '\n\n')
    }


    vif_remain.col <- vif.values[, "colnames"][!vif.values[, "VIF"] %in% vif_max]

    df1 <- df1[, vif_remain.col]


  }

  # print(paste0('vif_remain.col: ', vif_remain.col))
  list(input.df = df, output.df = df[, c(targetV, randomV, vif_remain.col)], selected.columns = base::names(df1),
       unselected.columns = base::names(df)[!names(df) %in% names(df1)])
}


#' Forward select Independent variables
#'
#' Forward selection of Independent variables, starts with variables specified
#' by the user and incrementally adds other columns to the list while checking
#' the VIF scores at each step. The process stops when addition of any of the
#' remaining variables will yield a Vif score greater than the threshold.
#'
#' @param df Dataframe with the Indipendent variables
#' @param curr.model.cols Variables that must be in the model
#' @param cols.to.add Variables to be forward selected
#' @param threshold The forward selection threshold
#' @param trace Boolean: wheter the details of each selection step should be
#'              sent to standard output
#'
#' @return a list
#' @export
#'
forwardselect <- function(df, targetV, randomV = NULL, curr.model.cols, cols.to.add, threshold=5, trace=FALSE) {
  df1 <- df

  repeat {
    # Exit the loop if there are no columns to add
    ncolumns <- length(cols.to.add)
    if (ncolumns == 0) break

    # Find the column that has the lowest VIF value when added
    cols.to.add.VIF <-
      sapply(cols.to.add, function(col.to.add){
        # subset the dataframe with current model columns plus
        # one cols.to.add column
        tmp.df1 <- df1[, c(curr.model.cols, col.to.add[i])]

        # Calculate VIF values
        vif.values <- yieldest::calculateVIF(tmp.df1, targetV, randomV)
        # Return VIF value of the added column
        vif.values[vif.values[, "colnames"] %in% col.to.add[i], ]
      }
      )

    # Get the column with the least VIF value
    vif_min <- base::min(cols.to.add.VIF, na.rm = TRUE)

    # If all VIF values are above the threshold, break
    if (vif_min <= threshold){
      # Add the name of the minimum column to the model columns list
      curr.model.cols <- c(curr.model.cols,
                           cols.to.add[cols.to.add.VIF %in% vif_min])

      # Remove the  minimum column from the columns to add vector
      cols.to.add <- cols.to.add[!cols.to.add.VIF %in% vif_min]
    } else  break



  }
  list(input.df = df, output.df = df1, selected.columns = curr.model.cols)
}

