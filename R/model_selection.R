
#' P-Values
#'
#' @param df dataframe with target variable and fixed variables
#' @param target The name of the target variable column
#' @param fixedv A vector of fixed variables names. If null all the other
#'               columns in the dataframe
#'
#' @return Summary linear model with p_values
#'

pvalues <- function(df, targetv, fixedv=NULL) {
  # Use all other columns in the dataframe as fixed variables if no fixed
  # variables are provided
  if (is.null(fixedv)){

    allcols <- colnames(df)

    fixedv <- allcols[!allcols %in% targetv]
  }


  # Create a linear model
  lm.model <- yieldest::yieldModel_lm(df = df,
                                   targetV = targetv,
                                   fixedV = fixedv)

  summary1 <- summary(lm.model)

  summary1$coefficients
  summary1
}


#' Select a model based on p-values
#'
#' random values are not used
#'
#' @inheritParams model_selection
#'
#' @return Model selected based on the p-value of fixed variables
#' @export
model_selection_pvalue <- function(df, targetV, randomV, fixedv=NULL, threshold_ = 0.05){

  selectedfixed <- fixedv
  if (is.null(fixedv)){
    allcols <- names(df)
    selectedfixed <- allcols[!allcols %in% c(targetV, randomV)]

  }


  selectedfixed_num <- length(selectedfixed)
  while(selectedfixed_num > 0){
    # calculate the coefficients
    modelcoefs <- pvalues(df, targetV, selectedfixed)
    # Check if they are all below the threshold(break if true)
    if(all(modelcoefs[,"Pr(>|t|)"] < threshold_)) break

    # Update the list of selected fixed variables by removing the fixed variable
    # with highest p-value
      # Remove intercept row
    modelcoefs1 <- modelcoefs[!rownames(modelcoefs) %in% "(Intercept)",]
    # print(modelcoefs)
      # Update the list of selected fixed variables
    selectedfixed <-
      rownames(modelcoefs1[!modelcoefs1[,"Pr(>|t|)"] %in% max(modelcoefs1[,"Pr(>|t|)"]),])

    print(selectedfixed)
    # return(NULL)
    # Update the number of remaining variables
    selectedfixed_num <- length(selectedfixed)
  }

  list( df = df, fixed.variables = selectedfixed )
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

  #both
  # seqrep = sequential replacement, combination of forward and backward selections).

  # set the reference Independent variables depending on the step.direction
  if(!step.direction %in% c("forward", "backward")) {
    yieldest::model_selection.error1(step.direction)
    }

  # Forward steps
  if(step.direction == "forward") Num.of.conbinations <- 1:length(fixedv_2)

  # Backward steps
    if(step.direction == "backward") Num.of.conbinations <- length(fixedv_2):1

    # return(Num.of.conbinations)


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
#' @param Num.of.conbinations a vector of order of combinations
model_selection_1 <- function(df, targetV, randomV, fixedv_1, fixedv_2,
                              Num.of.conbinations, trace_=FALSE  ) {
  selectedAIC <- Inf
  r2 <- 0
  AICofIV <- NULL


  # set the reference Independent variables depending on the step.direction
  selectedIV <- fixedv_1[!fixedv_1 %in% fixedv_2]

  unselectedIV <- fixedv_2

  # Estimate the time it takes
  no.of.steps <- sum(sapply(Num.of.conbinations,
                                function(x){dim(utils::combn(fixedv_2 , x))[2]}))
  starttime <- Sys.time()
  k <- 0

  # Repeat Forward or backward steps
  for (i in Num.of.conbinations) {
    fixedv_2.combinations <- utils::combn(fixedv_2 , i)
    print(i)

    for (j in 1:dim(fixedv_2.combinations)[2]) {
      k <- k + 1

      #c(selectedIV, addonIV)
      currentIV <- c(fixedv_1, fixedv_2.combinations[,j] )
      if (trace_ == TRUE) print("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")

      current.model <- yieldest::yieldModel(df, targetV, randomV, currentIV)
      # aa <- current.model
      # usethis::use_data(aa, overwrite = TRUE)
      # print(current.model)

      current.model.r2 <- ifelse(is.null(randomV),
                                 rsq::rsq(current.model),
                                 rsq::rsq(current.model)[["model"]])
      R2MnC <- MuMIn::r.squaredGLMM(current.model)

      current.model.R2m <- R2MnC[,"R2m"]
      current.model.R2c <- R2MnC[,"R2c"]

      current.model.AIC <- stats::AIC(current.model)
      current.model.p_value <- stats::coef(current.model)



      AICofIV <- rbind(AICofIV, c(current.model.AIC,
                                  current.model.r2,
                                  current.model.R2m,
                                  current.model.R2c,
                                  length(currentIV),
                                  paste(currentIV,collapse=", ")))

      colnames(AICofIV) <- c("AIC", "r2", "R2m", "R2c", "no.of.fixed.IV", "parameters")


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
     if((k %% 100) == 0 | k == no.of.steps){
       yieldest::progressReport(i = k,
                                total_i = no.of.steps,
                                starttime = starttime,
                                Message_ = "Selecting the optimal model: "
       )
     }

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

