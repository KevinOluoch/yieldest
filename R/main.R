#' The main function
#'
#' Run Analysis
#'
#' @export
main <- function(skipextractnames=TRUE, skipmodelselection=TRUE, topXmodels = 3){


# 1.0 Load data  ----------------------------------------------------------

  # 1.1 Option 1: Load data without admin column

  if (!skipextractnames){

  data.path0 <- yieldest::system.file("inst/extdata/MaizeYieldHouseHolddata.csv",
                                     package = "yieldest")
  data001 <- utils::read.csv(data.path0, stringsAsFactors = TRUE)
  data002 <- data001[!data001[, "educ_hoh"] %in% 71,]

  cat("\r", "Adding Admin name ...")
  print("data002")
  print(data002)
  data0 <- yieldest::addAdminName(data002) #Inefficient

  # remove outlier in educ_hoh

  nomCols.outlier <- c("educ_hoh")
  # data405 <- yieldest::normalization(data404, cols = nomCols.outlier)
  # print(head(data404))
  # print(str(factorby(df = data404, cols = nomCols.outlier, rep("0", length(nomCols.outlier)))))
  # return(unique(data404$educ_hoh))
  # data405 <- data404



  data1 <- data0

  }


  # 1.2 Option 2: Load data with admin column

  if (skipextractnames){
    # SKIPPED: data already has Admin data
    data.path <- yieldest::system.file("inst/extdata/MaizeYieldHHdata.csv",
                                       package = "yieldest")
    data1 <- utils::read.csv(data.path, stringsAsFactors = TRUE)
  }


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

  data2 <- data202


# 3.0 Normalize data  -----------------------------------------------------

  cat("\r","Normalizing data ...")

  # 3.1 Random variables: Make the random variables factors (Dummy variables)
  random.variables <- c("Admin_1", "aez", "year")
  data301 <- factorby(df = data2, cols = random.variables,
                      c("Mombasa", "Coastal Lowland", 2010))


  # 3.2 Binary data: Make the Binary variables factors (Dummy variables) with
  #     zero as the reerence value
  nomCols.binary <- c("hybrid", "sex", "credit_acc_hh_sc", "acc_ext",
                      "fert_use", "acc_ext_past")
  data302 <- factorby(df = data301, cols = nomCols.binary, rep("0", length(nomCols.binary)))


  # 3.3 Nominal variables: Make the Nominal variables factors (Dummy variables)
  # with the mode value as the reference value
  nomCols.nominal <- c("mar_stat")
  data303 <- factorby_occur(df = data302, cols = nomCols.nominal)



  # 3.4 Numeric variables(Normally distributed): Standardize continous variables by substracting the
  #     mean and dividing by the standard deviation
  nomCols.numeric.normal <- c("age","fam_exp", "educ_hoh", "hh_size",
                              "maximum_2m_air_temperature_year",
                              "minimum_2m_air_temperature_year",
                              "total_precipitation_year")

  # graphics::hist(data2[, "maize_qua_kh"])
  # print(base::table(data2[, "maize_qua_kh"]))
  # print(str(factorby(df = data4, cols = nomCols.binary, rep("0", length(nomCols.binary)))))
  # data3041 <- yieldest::normalization(data303, cols = nomCols.numeric.normal, method = "squares")
  # Square the weather variables before nomalizations
  nomCols.numeric.normal.weather <- c("maximum_2m_air_temperature_year",
                                      "minimum_2m_air_temperature_year",
                                      "total_precipitation_year")
  data3041 <- yieldest::normalization(data303,
                                      cols = nomCols.numeric.normal.weather,
                                      method = "squares")
  data304 <- yieldest::normalization(data3041, cols = nomCols.numeric.normal)



  # 3.5 Numeric variables(Heavily skewed distributed): Standardize continous variables by substracting the
  #     mean and dividing by the standard deviation
  nomCols.numeric.skew <- c("maize_area_ha", "dismarket_min",
                            "ext_km")
  # Hmisc::hist.data.frame(data2[, nomCols.numeric.skew])
  data305 <- data304

  data305[, "dismarket_min"] <- base::sapply(data304[, "dismarket_min"],
                                             function(x){base::ifelse(x < 30, 0, 1)})
  data305 <- factorby(df = data305, cols = "dismarket_min", "0")
  # data305[, "dismarket_min"] <- cut(data304$dismarket_min,
  #                              breaks = c(seq(0, 30, by=5),
  #                                         base::max(data304$dismarket_min)),
  #                              labels = seq(5, 35, by=5),
  #                              include.lowest = TRUE)

  data305[, "ext_km"] <- cut(data304[, "ext_km"],
                             breaks = c(0:10,
                                        max(data304[, "ext_km"])),
                             labels = 1:11,
                             include.lowest = TRUE)

  data305[, "maize_area_ha"] <- base::sapply(data304[, "maize_area_ha"],
                                             function(x){base::ifelse(x < 1, 0, 1)})
  data305 <- factorby(df = data305, cols = "maize_area_ha", "0")

  graphics::hist(as.numeric(data305[, "ext_km"]))
  # graphics::hist(as.numeric(data305[, "dismarket_min"]))
  # graphics::hist(as.numeric(data305[, "maize_area_ha"]))



  # 3.6 Normalize the target variable
  targetV <- "maize_qua_kh"
  data306 <- yieldest::normalization(data305, targetV)
  # graphics::hist(data305[, "maize_qua_kh"])


  data3 <- data306


# Type diffrent data normalization options --------------------------------
  # tryOutNormalizations() # Makes use of saved computed data
  # All normalizations
  # dataoption1a <- data3
  # # All normalizations except target variable
  # dataoption1b <- data303
  #
  # dataoption1_ts <- testscenarios(list(dataoption1a = dataoption1a,
  #                                  dataoption1b = dataoption1b))
  # # save the record
  # usethis::use_data(dataoption1_ts, overwrite = TRUE)
  #
  #
  #
  # # All normalizations but just square nomCols.numeric.skew
  # dataoption2a <- yieldest::normalization(data303, cols = nomCols.numeric.skew,
  #                                         method = "squares")
  # # All normalizations but standardize nomCols.numeric.skew
  # dataoption2b <- yieldest::normalization(data303, cols = nomCols.numeric.skew)
  #
  # dataoption2_ts <- testscenarios(list(dataoption2a  = dataoption2a,
  #                                      dataoption2b = dataoption2b))
  # # save the record
  # usethis::use_data(dataoption2_ts, overwrite = TRUE)


#   dataoption3a <- data303[,colnames(data303)[!colnames(data303) %in% random.variables] ]
#   dataoption3b <- data303[,colnames(data303)[!colnames(data303) %in% c("Admin_1", "year")] ]
# # return(dataoption3b)
#   dataoption3_ts <- testscenarios(list(dataoption3a = dataoption3a,
#                                        dataoption3b = dataoption3b),
#                                   targetV = "maize_qua_kh", randomV = NULL)
#   # save the record
#   usethis::use_data(dataoption3_ts, overwrite = TRUE)

# Back select the explanatory variables -----------------------------------

  # Selection based on VIF
  cat("\r", "Calculate VIF 1 ...")

  targetV <- "maize_qua_kh"
  randomV <- c("Admin_1", "year", "aez")

  data50 <- yieldest::backselect(data3, targetV, randomV,
                                 threshold = 5, trace = F) #data501)
  # return(data50)
  data5 <- data50[["output.df"]]




# Select a model ----------------------------------------------------------

  cat("\r", "Find the most optimal model ...")

  fixedv_1 <- NULL #c("hybrid", "sex", "mar_stat", "age", "fam_exp", "educ_hoh")
  fixedv_2 <- names(data5)[!names(data5) %in% c(targetV, randomV, fixedv_1)]


  # print(str(factorby(df = data4, cols = nomCols.binary, rep("0", length(nomCols.binary)))))
  if(!skipmodelselection) {
    optimalmodel <- model_selection(step.direction = 'forward',
                                    df = data5,
                                    targetV = targetV,
                                    randomV  = randomV,
                                    fixedv_1 = fixedv_1,
                                    fixedv_2 = fixedv_2,
                                    trace_=FALSE )


    optimalModelData5 <- optimalmodel
    usethis::use_data(optimalModelData5, overwrite = TRUE)
    optimalModelData <- optimalmodel
    usethis::use_data(optimalModelData, internal = TRUE, overwrite = TRUE)
    }


  if(skipmodelselection) optimalmodel <- optimalModelData

  # return(optimalmodel)

  # print(colnames(data5))
  # print(c(optimalmodel$selectedIV, targetV))
  data6 <- data5[,c(optimalmodel$selectedIV, targetV)]

# Calculate p-values ------------------------------------------------------

  cat("\r", "Calculate p-values ...")

  pvalueList <-
    list('pvalueall' = yieldest::pvalues(df=data5[,
                                    colnames(data5)[!colnames(data5) %in% c( "Admin_1",
                                                                             "year",
                                                                             "aez")]],
                           targetv = targetV),
         'pvalue2' = yieldest::pvalues(df=data6, targetv = targetV))



  # print(yieldest::pvalues(df=data5[, colnames(data5)[!colnames(data5) %in% c( "Admin_1",
  #                                                                             "year",
  #                                                                             "aez")]],
  #                         targetv = targetV))



# Calculate yield Estimate ------------------------------------------------
  cat("\r", "Calculate yield Estimate ...")

  AICofIV0 <- optimalModelData[["AICofIV"]]
  AICofIV <- AICofIV0[order(AICofIV0[,"AIC"]), ][, "parameters"]
  models <- list()
  for (params_i in 1:topXmodels) {
    # AICofIV[, "parameters"]
    # print(strsplit(AICofIV[params_i], ", "))
    # print(AICofIV[params_i])
    # # print(colnames(data6))
    models[[paste0("models_", params_i)]] <-
      yieldest::yieldModel(data5,
                           targetV = "maize_qua_kh",
                           randomV = c("Admin_1", "year", "aez"),
                           fixedV = strsplit(AICofIV[params_i], ", ")[[1]]
                           # c("hybrid", "educ_hoh", "maize_area_ha",
                           #   "fert_use", "acc_ext", "dismarket_min")#
                           #optimalmodel$selectedIV
      )


  }


  colnamesfixed <- colnames(data5)[!colnames(data5) %in% c("maize_qua_kh","Admin_1",
                                                        "year", "aez")]
  models0 <-
    yieldest::yieldModel(data5,
                         targetV = "maize_qua_kh",
                         randomV = c("Admin_1", "year", "aez"),
                         fixedV = colnamesfixed
    )
                         # c("hybrid", "educ_hoh", "maize_area_ha",
                         #   "fert_use", "acc_ext", "dismarket_min")#
                         #optimalmodel$selectedIV

# Return Model ------------------------------------------------------------
  cat("\r", "Return Model and the data used", "\n\n")
  list( "AICofIV0" = AICofIV0,
        "modeldata" = data5,
        "inputdata" = data1,
        "inputdata2" = data2,
        "inputdata3" = data3,
        "models" = models,
        "models0" = models0,
        "pvalueList" = pvalueList)

}
mainPlots<- function() {

# Temperature -------------------------------------------------------------

  ken0.Tave.mean <- NULL
  for (name_i in names(ken0.Tave)) {
    # print(name_i)
    # print(year_ <- stringr::str_extract(name_i, "[[:digit:]]{4}" ))
    # print(mean(ken0.Tave[[name_i]]))
    ken0.Tave.mean <- rbind(ken0.Tave.mean,
                            c(stringr::str_extract(name_i, "[[:digit:]]{4}" ),
                              mean(ken0.Tave[[name_i]], na.rm = TRUE))
    )
  }

  colnames(ken0.Tave.mean) <- c("Year", "ken0.Tave.mean")
  numValues <- as.numeric(ken0.Tave.mean[, 2])

  ken0.Tave.mean <- data.frame(Year = as.factor(ken0.Tave.mean[, 1]),
                               ken0.Tave.mean = as.numeric(round(numValues - mean(numValues), 3)))
  ggplot2::ggplot(data = ken0.Tave.mean,
                  ggplot2::aes(x = Year, y = ken0.Tave.mean, fill = ken0.Tave.mean > 0)) +
    ggplot2::geom_col( position = "dodge", width = 0.3)  +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  # ken0.Tave.mean


# Precipitation -----------------------------------------------------------

  ken0.precipitation.total <- NULL
  for (name_i in names(ken0.precipitation)) {
    # print(name_i)
    # print(year_ <- stringr::str_extract(name_i, "[[:digit:]]{4}" ))
    # print(mean(ken0.precipitation[[name_i]]))
    ken0.precipitation.total <- rbind(ken0.precipitation.total,
                                     c(stringr::str_extract(name_i,
                                                            "[[:digit:]]{4}" ),
                                       sum(ken0.precipitation[[name_i]], na.rm = TRUE))
    )
  }

  colnames(ken0.precipitation.total) <- c("Year", "ken0.precipitation.total")
  numValues <- as.numeric(ken0.precipitation.total[, 2])

  ken0.precipitation.total <- data.frame(Year = as.factor(ken0.precipitation.total[, 1]),
                                         ken0.precipitation.total = as.numeric(round(numValues - mean(numValues), 3)))
  ggplot2::ggplot(data = ken0.precipitation.total,
                  ggplot2::aes(x = Year,
                               y = ken0.precipitation.total,
                               fill = ken0.precipitation.total > 0)) +
    ggplot2::geom_col( position = "dodge", width = 0.3)  +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  # ken0.precipitation.total




}
maintest <- function() {

  prep1 <- "../data/weather/Precipitation/precipitation1971_present"
  Tmin1 <- "../data/weather/Temperature/Tmin1979_present"
  Tmax1 <- "../data/weather/Temperature/Tmax1979_present"

  # data888 <- extractYearlyMean(prep1, years=2007, extracted.data = ken0.Tmax)
  # data999 <- DailytoYearyAve(data888[["extracted.data"]])
  # p0 <- violinPlot(data999, label = 2007)
  # p1 <- p0 + ggplot2::geom_hline(yintercept=data888[["allmean"]], linetype="dashed",
  #                      color = "red")


  # data888 <- extractYearlyMean(prep1, years=2008:2015, extracted.data = ken0.Tave)
  # data999 <- DailytoYearyAve(data888[["extracted.data"]])
  #
  # data777 <- Daily_List2df(data888[["extracted.data"]])
  # print(data777[,c(1:3, 58:62)])
  # violinPlot(data777, allmean =  data888[["allmean"]],
  #            plot.title = "Average Temperature",
  #            x_label = "Years",
  #            y_label = "Average Daily Temperature")


  data888 <- extractYearlyMean(prep1, years=2008:2015, extracted.data = ken0.precipitation)
  data999 <- DailytoYearyAve(data888[["extracted.data"]])

  data777 <- Daily_List2df(data888[["extracted.data"]])
  print(data777[,c(1:3, 58:62)])
  violinPlot(data777, allmean =  data888[["allmean"]],
             plot.title = "Precipitation",
             x_label = "Years",
             y_label = "Total Daily Precipitation")


  # typeof(violinPlot(ken1.Tmax))
  # print(data999)
  # print(is.list(data999))
  # print(is.data.frame(data999))
  #
  #
  # violinPlot(data999)
}

