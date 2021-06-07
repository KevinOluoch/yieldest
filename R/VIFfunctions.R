

#' VIF functions
#'
#' A function that calculates the Variance Inflation Factor (VIF) values
#' using the function fmsb::VIF()and performs operations relating to VIF
#'
#' @param df dataframe
#' @param ... other arguments passed to fmsb::VIF()
#'
#' @return
#' @export
calculateVIF <- function(df, ...){

  if(class(df) != 'data.frame') df <- data.frame(df)

  #get initial vif value for all comparisons of variables
  vif.values <- NULL
  df.colnames <- names(df)
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


#' Title
#'
#' Backwards selection of explanatory variables, stops when all VIF values are
#' below the 'threshold'
#'
#'
#' @param df dataframe
#' @param threshold VIF threshold
#' @param trace trace
#' @param ...
#'
#' @return
#' @export
#'
backselect <- function(df, threshold = 10, trace = T, ...) {

  while (TRUE) {

  # Calculate VIF values
  vif.values <- calculateVIF(df)

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

  df <- df[, vif_remain.col]


  }


  list(selected.columns = base::names(df), select.df = df)
}

