#' The main function
#'
#' Run Analysis
#'
#' @export
main <- function( skipmodelselection=TRUE, topXmodels = 3){ #skipextractnames=TRUE,


# 1.0 Load data  ----------------------------------------------------------

  # 1.1 Option 1: Load data without admin column

  # if (!skipextractnames){
  #
  # data.path0 <- yieldest::system.file("inst/extdata/MaizeYieldHouseHolddata.csv",
  #                                    package = "yieldest")
  # data001 <- utils::read.csv(data.path0, stringsAsFactors = TRUE)
  # data002 <- data001[!data001[, "educ_yrs"] %in% 71,]
  #
  # cat("\r", "Adding Admin name ...")
  # print("data002")
  # print(data002)
  # data0 <- yieldest::addAdminName(data002) #Inefficient
  #
  # # remove outlier in educ_yrs
  #
  # nomCols.outlier <- c("educ_yrs")
  # # data405 <- yieldest::normalization(data404, cols = nomCols.outlier)
  # # print(head(data404))
  # # print(str(factorby(df = data404, cols = nomCols.outlier, rep("0", length(nomCols.outlier)))))
  # # return(unique(data404$educ_yrs))
  # # data405 <- data404
  #
  #
  #
  # data1 <- data0
  #
  # }


  # 1.2 Option 2: Load data with admin column

  # if (skipextractnames){
  # SKIPPED: data already has Admin data
  data.path <- yieldest::system.file("inst/extdata/MaizeYieldHHdata.csv",
                                     package = "yieldest")
  data1 <- utils::read.csv(data.path, stringsAsFactors = TRUE)
  # }


# 2.0 Remove unused columns -----------------------------------------------

  cat("\r", "Removing unused columns ...")

  # 2.1 target associated columns and identification columns
  ta_variables <- c("overall_match", "farm_income", "total_income", "school", #"fam_exp",
                    "maize_area_ha_sw","overall_match", "identification",
                    "match_pr_anh", "keep_hh", "ad_equ_oecd_mod",
                    "fert_qua_sw", "fert_qua_sw_kh", "maize_pro_sw",
                    "maize_qua_sw_kh", "maize_pro", "mysample", "fert_qua",
                    "fert_qua_kh", "HFIA", "HFIAS", "Qair_f_inst_mean_year")


  id.variables <- c( "X", "hhid", "hhid_2010",  "lat", "lon" )
  data201 <- yieldest::dropCol( data1, c(id.variables, ta_variables) )


  # 2.2 Remove Columns with NA Values
  data202 <- yieldest::dropNaCol(data201)
  # print(names(data201)[!names(data201) %in% names(data202)])
  data2 <- data202

# 3.0 Normalize data  -----------------------------------------------------

  cat("\r","Normalizing data ...")

  # 3.1 Random variables: Make the random variables factors (Dummy variables)
  random.variables <- c("Admin_1", "aez", "year")
  data301 <- factorby(df = data2, cols = random.variables,
                      c("Mombasa", "Coastal Lowland", 2010))


  # 3.2 Binary data: Make the Binary variables factors (Dummy variables) with
  #     zero as the reerence value
  nomCols.binary <- c("hybrid", "sex", "credit_acc", "acc_ext",
                      "fert_use", "acc_ext_past")

  data302 <- factorby(df = data301, cols = nomCols.binary, rep("0", length(nomCols.binary)))


  # 3.3 Nominal variables: Make the Nominal variables factors (Dummy variables)
  # with the mode value as the reference value
  nomCols.nominal <- c("mar_stat")
  data303 <- factorby_occur(df = data302, cols = nomCols.nominal)



  # 3.4 Numeric variables(Normally distributed): Standardize continous variables by substracting the
  #     mean and dividing by the standard deviation
  data3040 <- data303
  nomCols.numeric.normal <- c("age","fam_exp", "educ_yrs", "hh_size",
                              "max_2m_air_temp_yr",
                              "min_2m_air_temp_yr",
                              "precipitation_yr")

  # graphics::hist(data2[, "maize_qua_kh"])


  # Square the weather variables before nomali ations
  nomCols.numeric.normal.weather <- c("max_2m_air_temp_yr",
                                      "min_2m_air_temp_yr",
                                      "precipitation_yr")

  # Add squared the weather variables
  nomCols.numeric.normal.weather_sqrd <- paste0(nomCols.numeric.normal.weather,
                                                "_sqrd")
  data3040[,nomCols.numeric.normal.weather_sqrd] <- data3040[, nomCols.numeric.normal.weather]

  # Square terms
  data3041<- yieldest::normalization(data3040,
                                      cols = nomCols.numeric.normal.weather_sqrd,
                                      method = "squares")

  # print(str(data3041))
  # Normalize squared and linear terms
  data304 <- yieldest::normalization(data3041,
                                     cols = c(nomCols.numeric.normal,
                                              nomCols.numeric.normal.weather_sqrd))


  # 3.5 Numeric variables(Heavily skewed distributed): Standardize continuous
  # variables by substracting the mean and dividing by the standard deviation
  nomCols.numeric.skew <- c("maize_area_ha", "dis_market_min",
                            "ext_km")
  # Hmisc::hist.data.frame(data2[, nomCols.numeric.skew])
  data305 <- data304

  data305[, "dis_market_min"] <- base::sapply(data304[, "dis_market_min"],
                                             function(x){base::ifelse(x < 30, 0, 1)})
  data305 <- factorby(df = data305, cols = "dis_market_min", "0")
  # data305[, "dismarket_min"] <- cut(data304$dismarket_min,
  #                              breaks = c(seq(0, 30, by=5),
  #                                         base::max(data304$dismarket_min)),
  #                              labels = seq(5, 35, by=5),
  #                              include.lowest = TRUE)

  data305 <- yieldest::normalization(data304, "ext_km")
  # data305[, "ext_km"] <- cut(data304[, "ext_km"],
  #                            breaks = c(0:10,
  #                                       max(data304[, "ext_km"])),
  #                            labels = 1:11,
  #                            include.lowest = TRUE)

  data305[, "maize_area_ha"] <- base::sapply(data304[, "maize_area_ha"],
                                             function(x){base::ifelse(x < 1, 0, 1)})
  data305 <- factorby(df = data305, cols = "maize_area_ha", "0")

  # graphics::hist(as.numeric(data305[, "ext_km"]))
  # graphics::hist(as.numeric(data305[, "dismarket_min"]))
  # graphics::hist(as.numeric(data305[, "maize_area_ha"]))



  # 3.6 Normalize the target variable
  targetV <- "maize_qua_kh"
  data306 <- yieldest::normalization(data305, targetV)
  # graphics::hist(data305[, "maize_qua_kh"])

  # return(data305)

  data3 <-  data306 # data305 #


# Back select the explanatory variables -----------------------------------

  # Selection based on VIF
  cat("\r", "Calculate VIF 1 ...")

  targetV <- "maize_qua_kh"
  randomV <- c("Admin_1", "year", "aez")
  data50 <- yieldest::backselect(data3, targetV, randomV,
                                 threshold = 5, trace = T) #data501)

  print(names(data50[["output.df"]]))
  data5 <- data50[["output.df"]]




# Select a model ----------------------------------------------------------

  # cat("\r", "Find the most optimal model by AIC...")
  #
  # fixedv_1 <- NULL #c("hybrid", "sex", "mar_stat", "age", "fam_exp", "educ_yrs")
  # fixedv_2 <- names(data5)[!names(data5) %in% c(targetV, randomV, fixedv_1)]
  #
  #
  # # print(str(factorby(df = data4, cols = nomCols.binary, rep("0", length(nomCols.binary)))))
  # if(!skipmodelselection) {
  #   optimalmodel <- model_selection(step.direction = 'forward',
  #                                   df = data5,
  #                                   targetV = targetV,
  #                                   randomV  = randomV,
  #                                   fixedv_1 = fixedv_1,
  #                                   fixedv_2 = fixedv_2,
  #                                   trace_=FALSE )
  #
  #
  #   optimalModelData5 <- optimalmodel
  #   usethis::use_data(optimalModelData5, overwrite = TRUE)
  #   optimalModelData <- optimalmodel
  #   usethis::use_data(optimalModelData, internal = TRUE, overwrite = TRUE)
  #   }
  #
  #
  # if(skipmodelselection) optimalmodel <- optimalModelData
  #
  # # return(optimalmodel)
  #
  # # print(colnames(data5))
  # # print(c(optimalmodel$selectedIV, targetV))
  #
  # data6 <- data5[,c(optimalmodel$selectedIV, targetV)]

# Calculate p-values ------------------------------------------------------

  cat("\r", "Calculate p-values ...")

  pvalueList <-
    list('pvalueall' = yieldest::pvalues(df=data5[,
                                    colnames(data5)[!colnames(data5) %in% c( "Admin_1",
                                                                             "year",
                                                                             "aez")]],
                           targetv = targetV)) #,
         # 'pvalue2' = yieldest::pvalues(df=data6, targetv = targetV))



  # print(yieldest::pvalues(df=data5[, colnames(data5)[!colnames(data5) %in% c( "Admin_1",
  #                                                                             "year",
  #                                                                             "aez")]],
  #                         targetv = targetV))



# Calculate yield Estimate ------------------------------------------------
  cat("\r", "Calculate yield Estimate ...")
#
#   AICofIV0 <- optimalModelData[["AICofIV"]]
#   AICofIV <- AICofIV0[order(AICofIV0[,"AIC"]), ][, "parameters"]
#   models <- list()
#   for (params_i in 1:topXmodels) {
#     # AICofIV[, "parameters"]
#     # print(strsplit(AICofIV[params_i], ", "))
#     # print(AICofIV[params_i])
#     # # print(colnames(data6))
#     models[[paste0("models_", params_i)]] <-
#       yieldest::yieldModel(data5,
#                            targetV = "maize_qua_kh",
#                            randomV = c("Admin_1", "year", "aez"),
#                            fixedV = strsplit(AICofIV[params_i], ", ")[[1]]
#                            # c("hybrid", "educ_yrs", "maize_area_ha",
#                            #   "fert_use", "acc_ext", "dismarket_min")#
#                            #optimalmodel$selectedIV
#       )
#
#
#   }
#


  colnamesfixed <- colnames(data5)[!colnames(data5) %in% c("maize_qua_kh","Admin_1",
                                                        "year", "aez")]
  models0 <-
    yieldest::yieldModel(data5,
                         targetV = "maize_qua_kh",
                         randomV = c("Admin_1", "year", "aez"),
                         fixedV = colnamesfixed
    )
                         # c("hybrid", "educ_yrs", "maize_area_ha",
                         #   "fert_use", "acc_ext", "dismarket_min")#
                         #optimalmodel$selectedIV


# Return Model ------------------------------------------------------------
  cat("\r", "Return Model and the data used", "\n\n")
  list( #"AICofIV0" = AICofIV0,
        "modeldata" = data5,
        "otherStepData" = list("data1" = data1, "data2" = data2, "data3" = data3),
        # "models" = models,
        "models0" = models0,
        "pvalueList" = pvalueList)

}


