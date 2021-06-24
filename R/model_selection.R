
#' P-Values
#'
#' @param df dataframe with target variable and fixed variables
#' @param target The name of the target variable column
#' @param fixedv A vector of fixed variables names. If null all the other
#'               columns in the dataframe
#'
#' @return Summary linear model with p_values
#'
#' @examples
pvalues <- function(df, targetv, fixedv=NULL) {
  # Use all other columns in the dataframe as fixed variables if no fixed
  # variables are provided
  if (base::is.null(fixedv)){

    allcols <- base::names(df)

    fixedv <- allcols[!allcols %in% targetv]
  }


  # Create a linear model
  lm.model <- yieldest::yieldModel_lm(df = df,
                                   targetv = targetv,
                                   fixedv = fixedv)

  summary(lm.model)
}



#' Select an optimal model
#'
#' Function to select a model by interactively adding or removing Independent
#' variables to/from model, while calculating the effectsize of each model. The
#' function returns the model with the smallest effectsize.
#'
#' @param step.direction The direction of model iteration. Must be either
#'     'forward' or 'backward'
#' @param df a dataframe with columns of the target variable, random variables
#'     and fixed (fixedv_1, fixedv_2  )variables
#' @param targetV The target variable
#' @param randomV A vector of names of random variables columns in df
#' @param fixedv_1 A vector of names of Independent variables columns in df, which
#'     must be in the final model. If a name appears in both fixedv_1 and fixedv_2,
#'     the fixedv_1 entry is ignored.
#' @param fixedv_2  A vector of names of Independent variables columns in df, which
#'     can be in the final model based on the model's accuracy. If a name appears
#'     in both fixedv_1 and fixedv_2, the fixedv_1 entry is ignored.
#' @param trace_ Boolean: wheter the details of each selection step should be
#'              sent to standard output.
#'
#' @return A object of type list containing the final model
#' @export
model_selection <- function(step.direction, df, targetV, randomV,
                            fixedv_1 = NULL, fixedv_2 , trace_=FALSE  ) {



  # set the reference Independent variables depending on the step.direction
  if(!step.direction %in% c("forward", "backward")) {
    yieldest::model_selection.error1(step.direction)
    }

  # Forward steps
  if(step.direction == "forward") Num.of.conbinations <- 1:length(fixedv_2)

  # Backward steps
    if(step.direction == "backward") Num.of.conbinations <- length(fixedv_2):1

    yieldest:: model_selection_1(df, targetV, randomV,
                                                 fixedv_1, fixedv_2 ,
                                                 trace_ = trace_,
                                                 Num.of.conbinations)

  }


#' Forward Select or Back select an optimal model
#'
#' Function to select a model by interactively adding or removing the number of
#' Independent variables to the model, while calculating the effectsize of
#' each model. The function returns the model with the smallest effectsize.
#'
#' @inheritParams yieldModel
model_selection_1 <- function(df, targetV, randomV, fixedv_1, fixedv_2,
                              Num.of.conbinations, trace_=FALSE  ) {
  selectedAIC <- Inf
  r2 <- 0
  AICofIV <- NULL


  # set the reference Independent variables depending on the step.direction
  selectedIV <- fixedv_1[!fixedv_1 %in% fixedv_2]

  unselectedIV <- fixedv_2

  # Estimate the time it takes
  no.of.steps <- base::sum(sapply(Num.of.conbinations,
                                function(x){base::dim(utils::combn(fixedv_2 , x))[2]}))
  starttime <- base::Sys.time()
  k <- 0

  # Repeat Forward or backward steps
  for (i in Num.of.conbinations) {
    fixedv_2.combinations <- utils::combn(fixedv_2 , i)
    print(i)

    for (j in 1:base::dim(fixedv_2.combinations)[2]) {
      k <- k + 1

      #c(selectedIV, addonIV)
      currentIV <- c(fixedv_1, fixedv_2.combinations[,j] )
      if (trace_ == TRUE) print("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")

      current.model <- yieldest::yieldModel(df, targetV, randomV, currentIV)


      current.model.r2 <- rsq::rsq(current.model)[["model"]]
      current.model.AIC <- stats::AIC(current.model)
      current.model.p_value <- stats::coef(current.model)



      AICofIV <- base::rbind(AICofIV, c(current.model.AIC,
                                        current.model.r2,
                                        length(currentIV),
                                        paste(currentIV,collapse=", ")))

      base::colnames(AICofIV) <- c("AIC", "r2", "no.of.fixed.IV", "parameters")


      if (trace_ == TRUE) {
        print("Fixed variables")
        print(paste(currentIV, collapse=", "))
        print(paste0("R2: ", current.model.r2))
        }

      if (current.model.AIC < selectedAIC) {
        # Save Current Independent variables from selected
        selectedIV <- currentIV
        # Save Current r2
        r2 <- current.model.r2
        # Save Current AIC
        selectedAIC <- current.model.AIC
        # Save Current model
        selected.model <- current.model

        if (trace_ == TRUE) print(paste0("Current best model has fixed variables : ",
                                         paste(currentIV,collapse=" ")))
        }

      # Update the user on progress
      # Get remaining time
      time.remain <- base::difftime(Sys.time(),
                                    starttime,
                                    units = "secs") * ((no.of.steps - k)/k)
      hrs.remain <- base::floor(time.remain/3600)
      min.remain <- base::floor((time.remain - (hrs.remain*3600))/60)
      sec.remain <- base::floor(time.remain  - (hrs.remain*3600) -(min.remain*60))
      # Show progress and remaining time
      cat("\r", paste("Selecting the optimal model. Progress ",
                      100*base::round(k/no.of.steps, 4), "%",
                      ". Approximate remaining time is",
                      hrs.remain, " Hours  ",
                      min.remain, " minutes  ",
                      sec.remain, " seconds "
      ))

    }

  }

  allVariables <- names(df)
  list(selected.model= selected.model,
       r2 = r2,
       selectedAIC = selectedAIC,
       AICofIV = AICofIV,
       selectedIV = selectedIV,
       unselectedIV = allVariables[!allVariables %in% c(targetV, randomV, selectedIV)])
}

