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
  df.colnames <- names(df)
  df.colnames <- df.colnames[!df.colnames %in% c(targetV, randomV )]

  df.colnames.factor <-names(Filter(is.factor, df))
  df.colnames.factor <- df.colnames.factor[!df.colnames.factor %in% c(targetV, randomV )]

  df1 <- df

  for(col_ in df.colnames.factor){
    df1[,col_] <- as.integer(df[,col_])
  }



  for(col_ in df.colnames){

    regressors <- df.colnames[!df.colnames %in% col_]
    formula_0 <- paste(regressors, collapse = '+')
    formula_ <- stats::formula(paste(col_, '~', formula_0))
    vif0 <- fmsb::VIF(stats::lm(formula_, data = df1, ...))

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
backselectVIF <- function(df, targetV, randomV = NULL, untouched=NULL,
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

      vif_remain.col <- vif.values[, "colnames"]

      break()
    }
    # Get the name of the column with the max value.
    vif_max.col <- vif.values[, "colnames"][vif.values[, "VIF"] %in% vif_max]

    if(trace==T){ #print output of each iteration
      # prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)

      cat('\n')
      cat(paste('All variables have VIF > ', threshold,
                ', max VIF "', vif_max.col, '" : ', round(vif_max,2), sep=''),
          '\n\n')
    }


    vif_remain.col <- vif.values[, "colnames"][!vif.values[, "VIF"] %in% vif_max]

    df1 <- df1[, vif_remain.col]


  }


  # print(paste0('vif_remain.col: ', vif_remain.col))
  list(input.df = df,
       output.df = df[, c(targetV, randomV, vif_remain.col)],
       selected.columns = base::names(df1),
       unselected.columns = base::names(df)[!names(df) %in% names(df1)])
}


