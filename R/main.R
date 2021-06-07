#' The main function
#'
#' Run Analysis
#'
#' @export
main <- function(skipextractnames=TRUE){

  # Load data without admin column ------------------------------------------

  if (!skipextractnames){

  data.path0 <- yieldest::system.file("inst/extdata/MaizeYieldHouseHolddata.csv",
                                     package = "yieldest")
  data0 <- utils::read.csv(data.path0, stringsAsFactors = TRUE)

  cat("\r", "Adding Admin name ...")
  data0 <- yieldest::addAdminName(data0) #Inefficient
  data1 <- data0

  }


# Load data with admin column ---------------------------------------------
  if (skipextractnames){
    # SKIPPED: data already has Admin data
    data.path <- yieldest::system.file("inst/extdata/MaizeYieldHHdata.csv",
                                       package = "yieldest")
    data1 <- utils::read.csv(data.path, stringsAsFactors = TRUE)
  }

# Remove Label columns ----------------------------------------------------

  cat("\r", "Removing Label columns ...")
  sw_variables <- c("farm_income", "total_income", "maize_area_ha_sw",
                    "fert_qua_sw", "fert_qua_sw_kh", "maize_pro_sw",
                    "maize_qua_sw_kh", "maize_qua_kh", "mysample" )

  label.variables <- c("X", "hhid", "hhid_2010",  "lat", "lon", sw_variables  )
  data2 <- yieldest::dropCol(data1, label.variables)


# Remove Columns with NA Values -------------------------------------------

  cat("\r","Remove Columns with NA Values ...")
  data3 <- yieldest::dropNaCol(data2)


# Normalize data ----------------------------------------------------------

  cat("\r","Normalizing data ...")
  random.variables <- c("Admin_1", "aez", "year")
  data401 <- data3

  nomCols.numeric <- c("age", "fam_exp", "hh_size", "HFIAS", "overall_match",
                       "maximum_2m_air_temperature_year",
                       "minimum_2m_air_temperature_year", "total_precipitation_year",
                       "Qair_f_inst_mean_year")
  data402 <- yieldest::normalization(data401, cols = nomCols.numeric)

  nomCols.nominal <- c("mar_stat", "HFIA", "match_pr_anh")
  data403 <- data402

  nomCols.binary <- c("hybrid", "sex", "fert_use", "credit_acc_hh_sc", "acc_ext",
                      "acc_ext_past", "identification", "keep_hh", "school")
  data404 <- data403

  nomCols.outlier <- c("educ_hoh")
  # data405 <- yieldest::normalization(data404, cols = nomCols.outlier)
  data405 <- data404

  logarithimiccols <- c("maize_area_ha", "maize_pro", "fert_qua",
                        "fert_qua_kh", "dismarket_min", "ext_km")
  data406 <- yieldest::normalization(data405, cols = logarithimiccols, method = "logarithmic")
  data407 <- yieldest::normalization(data406, cols = logarithimiccols)
  return(list('data405'=data405, 'data406'=data406, 'data407'=data407))
  data407 <- data405

  # data407 <- yieldest::normalization(data406,
                                   # cols = names(data3))

  data4 <- data407



# Back select the explanatory variables -----------------------------------

  # Check for colinearity
  cat("\r", "Calculate VIF 1 ...")

  # Remove Columns with NA Values
  cat("\r","Remove Columns with NA Values ...")
  data501 <- yieldest::dropNaCol(data4)
  data50 <- yieldest::backselect(data4, threshold = 10, trace = F) #data501)
  data5 <- data50[["select.df"]]


# Calculate yield Estimate ------------------------------------------------

  cat("\r", "Calculate yield Estimate ...")
  model0 <- yieldest::yieldModel2(data5,
                        targetv = "maize_pro",
                        randomv = c("Admin_1", "year", "aez"))
  # model0 <- yieldest::yieldModel(data5 ,
  #                      targetv = "maize_pro",
  #                      fixedv = names(data5)[c(4:13, 15:30)], #c("maize_area_ha", "fert_use"),
  #                      randomv = c("Admin_1", "aez", "year"))
  #                      # randomv = c("aez", "year"))




# Return Model ------------------------------------------------------------
  cat("\r", "Return Model and the data used ...", "\n\n")
  list("vif"=data50[["selected.columns"]], "model" = model0, "admindata" = data0)
  model0

}


