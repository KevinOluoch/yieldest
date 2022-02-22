#' Organised dataframe columns by given order
#'
#' @param df dataframe
#' @param col_ordr column order
#'
#' @return list of input and output dataframe
#' @export
#'
organizedf <- function(df, col_ordr = NULL) {

  rndm <- c("Admin_1", "cty", "aez", "yr", "year")

  trgt <- c("maize_qua_kh", "mzY")

  # crds <- c("lat", "lon")

  demg <- c( "age", "ed", "educ_yrs", "fE", "fam_exp", "hh", "hh_size",
             "mSt", "mar_stat", "mSt1", "mar_stat1", "mSt2", "mar_stat2",
             "mSt3", "mar_stat3", "sex", "sex1")

  scec <- c( "credit_acc", "cdt","credit_acc1", "cdt1", "hybrid", "cS",
             "hybrid1", "cS1", "acc_ext","ex", "acc_ext1", "ex1",
             "acc_ext_past", "exP", "acc_ext_past1", "exP1", "fert_use", "fU",
             "fert_use1", "fU1",  "maize_area_ha", "pS")

  bphy <- c( "ext_km", "dis_market_min", "mkt", "max_2m_air_temp_yr", "mxT",
             #"min_2m_air_temp_yr", "mnT",
             "precipitation_yr", "ppt", "max_2m_air_temp_yr_sqrd", "mxT2",
             # "min_2m_air_temp_yr_sqrd", "mnT2",
             "precipitation_yr_sqrd", "ppt2")


  all_names <- c(trgt, rndm, demg, scec, bphy) #crds,

  select_names <- all_names[all_names %in% names(df)]

  df1 <- df[,select_names]

  list(inputdf = df, outputdf = df1)
}

