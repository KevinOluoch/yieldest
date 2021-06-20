#' The main function
#'
#' Run Analysis
#'
#' @export
main <- function(skipextractnames=TRUE){


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
  ta_variables <- c("overall_match", "farm_income", "total_income", #"fam_exp",
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
                      "fert_use", "acc_ext_past", "school")
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
  data304 <- yieldest::normalization(data303, cols = nomCols.numeric.normal)

  # 3.5 Numeric variables(Heavily skewed distributed): Standardize continous variables by substracting the
  #     mean and dividing by the standard deviation
  nomCols.numeric.skew <- c("maize_area_ha", "dismarket_min",
                            "ext_km")
  # Hmisc::hist.data.frame(data2[, nomCols.numeric.skew])
  data305 <- data304
  data305[, "dismarket_min"] <- cut(data304$dismarket_min,
                               breaks = c(seq(0, 30, by=5),
                                          base::max(data304$dismarket_min)),
                               labels = seq(5, 35, by=5),
                               include.lowest = TRUE)

  data305[, "ext_km"] <- cut(data304[, "ext_km"],
                             breaks = c(0:10,
                                        max(data304[, "ext_km"])),
                             labels = 1:11,
                             include.lowest = TRUE)

  data305[, "maize_area_ha"] <- base::sapply(data304[, "maize_area_ha"],
                                             function(x){base::ifelse(x > 1, 0, 1)})
  data305 <- factorby(df = data305, cols = "maize_area_ha", "0")

  # graphics::hist(as.numeric(data305[, "ext_km"]))
  # graphics::hist(as.numeric(data305[, "dismarket_min"]))
  # graphics::hist(as.numeric(data305[, "maize_area_ha"]))


  data3 <- data305


# Back select the explanatory variables -----------------------------------

  # Check for colinearity
  cat("\r", "Calculate VIF 1 ...")
  data50 <- yieldest::backselect(data3, threshold = 5, trace = F) #data501)
  # return(data50)
  data5 <- data50[["output.df"]]


# Select a model ----------------------------------------------------------

  cat("\r", "Find the most optimal model ...")

  targetV <- "maize_qua_kh"
  randomV <- c("Admin_1", "year", "aez")
  fixedv_1 <- NULL #c("hybrid", "sex", "mar_stat", "age", "fam_exp", "educ_hoh")
  fixedv_2 <- names(data5)[!names(data5) %in% c(targetV, randomV, fixedv_1)]


  # print(str(factorby(df = data4, cols = nomCols.binary, rep("0", length(nomCols.binary)))))
  optimalmodel <- model_selection(step.direction = 'forward',
                              df = data5,
                              targetV = targetV,
                              randomV  = randomV,
                              fixedv_1 = fixedv_1,
                              fixedv_2 = fixedv_2,
                              trace_=FALSE )

  return(optimalmodel)
  dataZ <- data50
  # print(str(dataZ))
  # print(head(data2))
  print(names(dataZ[["output.df"]]))
  print(names(dataZ[["input.df"]]))

  return(head(dataZ[["output.df"]]))

# Calculate yield Estimate ------------------------------------------------

  cat("\r", "Calculate yield Estimate ...")
  model0 <- yieldest::yieldModel(data5,
                        targetv = "maize_qua_kh",
                        randomv = c("Admin_1", "year", "aez"),
                        use.rest.as.fixedv = TRUE)
  # model0 <- yieldest::yieldModel(data5 ,
  #                      targetv = "maize_pro",
  #                      fixedv = names(data5)[c(4:13, 15:30)], #c("maize_area_ha", "fert_use"),
  #                      randomv = c("Admin_1", "aez", "year"))
  #                      # randomv = c("aez", "year"))




# Return Model ------------------------------------------------------------
  cat("\r", "Return Model and the data used", "\n\n")
  list("modeldata" = data5, "vif"=data50[["VIF"]], "model" = model0)



}


