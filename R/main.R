#' The main function
#'
#' Run Analysis
#'
#' @export
main <- function( skipmodelselection=TRUE, topXmodels = 3, df.main = NULL){


  # 1.0 Load data  ----------------------------------------------------------

  data.path <- yieldest::system.file("inst/extdata/FarmersHHdata.csv",
                                     package = "yieldest")
  data1 <- utils::read.csv(data.path, stringsAsFactors = TRUE)

  # Identify the target variable
  targetV <- "maize_qua_kh"
  randomV <- c("Admin_1", "aez", "year")

# return(list("a" = data1, "b" = data12[colnames(data12) %in% c(colnames(data1),
#                                                               c("Admin_1", "year", "aez"))]))


  # 2.0 Normalize data  -----------------------------------------------------

  cat("\r","Normalizing data ...")
  data200 <- data1

  # 3.2 Binary data: Make the Binary variables factors (Dummy variables) with
  #     zero as the reerence value
  nomCols.binary <- c("hybrid", "sex", "credit_acc", "acc_ext",
                      "fert_use", "acc_ext_past")

  data202 <- yieldest::factorby(df = data200, cols = nomCols.binary, rep("0", length(nomCols.binary)))


  # 3.3 Nominal variables: Make the Nominal variables factors (Dummy variables)
  # with the mode value as the reference value
  nomCols.nominal <- c("mar_stat")
  data203 <- yieldest::factorby_occur(df = data202, cols = nomCols.nominal)



  # 3.4 Numeric variables(Normally distributed): Standardize continous variables by substracting the
  #     mean and dividing by the standard deviation
  data2040 <- data203
  nomCols.numeric.normal <- c("age","fam_exp", "educ_yrs", "hh_size",
                              "max_2m_air_temp_yr",
                              #"min_2m_air_temp_yr",
                              "precipitation_yr")

  # graphics::hist(data2[, "maize_qua_kh"])


  # Square the weather variables before nomali ations
  nomCols.numeric.normal.weather <- c("max_2m_air_temp_yr",
                                      #"min_2m_air_temp_yr",
                                      "precipitation_yr")

  # Add squared the weather variables
  nomCols.numeric.normal.weather_sqrd <- paste0(nomCols.numeric.normal.weather,
                                                "_sqrd")
  data2040[,nomCols.numeric.normal.weather_sqrd] <- data2040[, nomCols.numeric.normal.weather]

  # Square terms
  data2041<- yieldest::normalization(data2040,
                                     cols = nomCols.numeric.normal.weather_sqrd,
                                     method = "squares")

  # print(str(data3041))
  # Normalize squared and linear terms
  data204 <- yieldest::normalization(data2041, group = TRUE, groupby = "aez",
                                     cols = c(nomCols.numeric.normal,
                                              nomCols.numeric.normal.weather_sqrd))


  # 3.5 Numeric variables(Heavily skewed distributed): Standardize continuous
  # variables by subtracting the mean and dividing by the standard deviation
  nomCols.numeric.skew <- c("maize_area_ha", "dis_market_min",
                            "ext_km")

  data205 <- data204

  data205 <- yieldest::normalization(data204, nomCols.numeric.skew)


  # 3.6 Normalize the target variable
  targetV <- "maize_qua_kh"
  data206 <- yieldest::normalization(data205, targetV)

  data2 <-  data206 # data305 #



# 3.0 Rename variables ----------------------------------

  cat("\r", "Rename variables ...")


# 4.0 Use VIF to check Collinearity ---------------------------------------


  # Selection based on VIF
  cat("\r", "Calculate VIF 1 ...")
  data40 <- data2

  #Organize the columns
  list41 <- organizedf(data40)
  data42 <- list41[["outputdf"]]

  # Leave out the squared terms when calculating VIF
  sqrd_terms <- c("max_2m_air_temp_yr"="max_2m_air_temp_yr_sqrd",
                  #"mnT" = "mnT2",
                  "precipitation_yr" = "precipitation_yr_sqrd")

  data43 <- data42[,!names(data42) %in% sqrd_terms]


  list44 <- yieldest::backselectVIF(data43, targetV, randomV,
                                 threshold = 5, trace = F)

  # if (!is.null(df.main)){
  #
  #   list64 <- yieldest::backselect(data43, targetV, randomV,
  #                                  threshold = 10, trace = F)
  # }
  data45 <- list44[["output.df"]]
  data46 <- data45

  # Return squared terms if the linear terms are still there
  for (linear_name in names(sqrd_terms)){
    if (linear_name %in% names(data45)){
      data46[sqrd_terms[linear_name]] <- data40[sqrd_terms[linear_name]]
    }
  }

  data4 <- data46



# 5.0 Fixed- Effects Model Calculate Yield Estimate -----------------------

  cat("\r", "Calculate yield Estimate ...")
  data50 <- data4
  list51 <- yieldest::organizedf(data50)
  data52 <- list51[["outputdf"]]

  fxdEffMdl <-
    yieldest::fixedEffetsModel(df=data52[,colnames(data52)[!colnames(data52) %in% randomV]],
                                          targetv = targetV)




# 6.0 Return model --------------------------------------------------------
  cat("\r", "Complete: Returned Model, ")

  list("Model" = fxdEffMdl, "summary" = summary(fxdEffMdl))

}



