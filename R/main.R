#' The main function
#'
#' Run Analysis
#'
#' @export
main <- function(){
  # Load data
  data.path <- yieldest::system.file("inst/extdata/MaizeYieldHHdata.csv",
                                     package = "yieldest")
  data0 <- utils::read.csv(data.path, stringsAsFactors = TRUE)

  #Add admin level names
  # SKIPPED: data already has Admin data
  # cat("\r", "Adding Admin name ...")
  # data10 <- yieldest::addAdminName(data0) #Inefficient
  data1 <- data0

  # Remove Label columns
  cat("\r", "Removing Label columns ...")
  sw_variables <- c("farm_income", "total_income", "maize_area_ha_sw",
                    "fert_qua_sw", "fert_qua_sw_kh", "maize_pro_sw",
                    "maize_qua_sw_kh", "maize_qua_kh", "mysample" )

  label.variables <- c("X", "hhid", "hhid_2010",  "lat", "lon", sw_variables  )
  data2 <- yieldest::dropCol(data1, label.variables)

  # Remove Columns with NA Values
  cat("\r","Remove Columns with NA Values ...")
  data3 <- yieldest::dropNaCol(data2)

  # Check for colinearity
  # Hmisc::rcorr(base::as.matrix(data4))
  data4 <- data3

  # Normalize data
  cat("\r","Normalizing data ...")
  random.variables <- c("Admin_1", "aez", "year")
  nomCols.numeric <- c("age", "fam_exp", "hh_size", "HFIAS", "overall_match",
                       "maximum_2m_air_temperature_year",
                       "minimum_2m_air_temperature_year", "total_precipitation_year",
                       "Qair_f_inst_mean_year")
  nomCols.nominal <- c("mar_stat", "HFIA", "match_pr_anh")
  nomCols.binary <- c("hybrid", "sex", "fert_use", "credit_acc_hh_sc", "acc_ext",
                      "acc_ext_past", "identification", "keep_hh", "school")

  nomCols.outlier <- c("educ_hoh")

  logarithimiccols <- c("maize_area_ha", "maize_pro", "fert_qua",
                        "fert_qua_kh", "dismarket_min", "ext_km")
  # print("")
  # print(table(data4$Qair_f_inst_mean_year))
  # return(summary(data4))

  data5 <- yieldest::normalization(data4,
                                   cols = names(data4))

  # Calculate yield Estimate
  cat("\r", "Calculating Label columns ...")
  # yieldest::yieldModel2(data5, targetv = "maize_pro", randomv = c("year", "aez"))
  model0 <- yieldest::yieldModel(data5 ,
                       targetv = "maize_pro",
                       fixedv = names(data5)[c(4:13, 15:30)], #c("maize_area_ha", "fert_use"),
                       randomv = c("Admin_1", "aez", "year"))
                       # randomv = c("aez", "year"))


  # Calculate VIF
  vif.data <- car::vif(model0)



  list("vif"=vif.data, "model" = model0)
  # summary(data5)

}


