testscenarios <- function(list.of.df, targetV = "maize_qua_kh",
                          randomV = c("Admin_1", "year", "aez")) {

  dfnames <- names(list.of.df)
  j <- 0
  data6 <- list()
  for (df in list.of.df) {
    j <- j + 1
    # Back select the explanatory variables -----------------------------------

    # targetV <- "maize_qua_kh"
    # randomV <- c("Admin_1", "year", "aez")
    # Selection based on VIF
    cat("\r", paste0("Calculate VIF 1 ...'testscenarios' ", dfnames[j]))
    data50 <- yieldest::backselect(df, targetV, randomV,
                                   threshold = 5, trace = F) #data501)
    # return(data50)
    data5 <- data50[["output.df"]]




    # Select a model ----------------------------------------------------------

    cat("\r", paste0("Find the most optimal model ...'testscenarios' ", dfnames[j]))

    fixedv_1 <- NULL #c("hybrid", "sex", "mar_stat", "age", "fam_exp", "educ_hoh")
    fixedv_2 <- names(data5)[!names(data5) %in% c(targetV, randomV, fixedv_1)]


    # print(str(factorby(df = data4, cols = nomCols.binary, rep("0", length(nomCols.binary)))))
    optimalmodel <- yieldest::model_selection(step.direction = 'forward',
                                              df = data5,
                                              targetV = targetV,
                                              randomV  = randomV,
                                              fixedv_1 = fixedv_1,
                                              fixedv_2 = fixedv_2,
                                              trace_=FALSE )


    data6[[dfnames[j]]] <- list(optimalmodel, data5)

  }

  data6
}
