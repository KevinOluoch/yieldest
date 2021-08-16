table_0 <- function() {
  # Get the names of study and non study counties
  modeldata0 <- yieldest::main(T, T)
  modeldata <- modeldata0[['modeldata']]
  studycounties <- as.character(unique(modeldata$Admin_1))

  ken1.sf <- admin_sf(country = "KEN", level = 1)
  ken1.sf_stdyCnties <- ken1.sf[data.frame(ken1.sf)[, "NAME_1"] %in% studycounties,]

  output <- list()
  r.paths <- c("inst/extdata/spam2010V2r0_global_P_MAIZ_A.tif",
               "inst/extdata/spam2010V2r0_global_H_MAIZ_R.tif")
  for (r.path in r.paths){

    # Get 2010 yield raster
    r <- terra::rast(r.path)

    ken.r <- terra::mask(terra::crop(r, ken1.sf), terra::vect(ken1.sf))
    ken.r_stdyCnties <- terra::mask(terra::crop(r, ken1.sf_stdyCnties),
                                    terra::vect(ken1.sf_stdyCnties))
    sum.stdyCnty <- sum(terra::values(ken.r_stdyCnties), na.rm = TRUE)
    sum.allCnty <- sum(terra::values(ken.r), na.rm = TRUE)
    output[basename(r.path)] <- c(sum.stdyCnty/sum.allCnty)

    # terra::values(ken.r_stdyCnties)
    #
    # terra::plot(ken.r_stdyCnties)
  }

  # # Get 2013 yield data
  # yield2013 <- read.csv("inst/extdata/2013_Yield_per_County.csv")
  # for (col_ in colnames(yield2013)[2:4]) {
  #   yield2013[, col_] <- as.numeric(gsub(",", "", yield2013[,col_]))
  # }
  # # Get the names of study and non study counties
  # modeldata0 <- yieldest::main(T, T)
  # modeldata <- modeldata0[['modeldata']]
  # studycounties <- as.character(unique(modeldata$Admin_1))
  # # Get study and non study data
  # yield2013.studycty <- colSums(yield2013[yield2013$COUNTY %in% studycounties,][2:4])
  #
  # yield2013.all <- colSums(yield2013[2:4])
  # yield2013.studycty/yield2013.all

  output
}
table_1 <- function(df =NULL) {
  if(is.null(df)){
    modeldata0 <- yieldest::main(T, T)
    df <- modeldata0[["modeldata"]]
    df <- df[, !names(df) %in% c("maize_qua_kh", "Admin_1", "year", "aez")]

  }
  df.colnames.factor <-names(Filter(is.factor, df))

  for(col_ in df.colnames.factor){
    df[,col_] <- as.integer(df[,col_])
  }
  df1 <- rbind(round(apply(df, 2, min), 2),
               round(apply(df, 2, max), 2),
               round(apply(df, 2, mean), 2),
               round(apply(df, 2, median), 2),
               round(apply(df, 2, sd), 2)
  )

rownames(df1) <- c("minimum", "maximum", "mean", "median", "sd")


df1
}

#' Create caterpillar plot
#'
#' Create caterpillar plot that shows the model variable plus the standard coefficients range
#' @param model
#'
#' @return
#' @export
#'
table_2 <- function(model=NULL){

  if(is.null(model)) {
    modeldata <- yieldest::main(T, T)
    model <- modeldata[['models0']]
  }


# Add max min values and pvalue range -------------------------------------

  model_summ <- summary(model)
  model_coefs <- model_summ[['coefficients']]

  model_coefs2 <- cbind(variables = rownames(model_coefs),
                        model_coefs,
                        min = model_coefs[, 'Estimate'] - model_coefs[, 'Std. Error'],
                        max = model_coefs[, 'Estimate'] + model_coefs[, 'Std. Error'],
                        pmin = model_coefs[, 'Estimate'] - 0.05,
                        pmax = model_coefs[, 'Estimate'] + 0.05)

  model_coefs_outp <- model_coefs2


# Order rows by 'Std. Error' or 'Estimate' --------------------------------

  model_coefs2_levels <- model_coefs2[, 'variables'][order(abs(as.numeric(model_coefs2[, 'Estimate'])))]
  # print(data.frame(model_coefs2))
  # model_coefs2_levels <- model_coefs2[, 'variables'][order(model_coefs2[, 'Std. Error'],
  #                                                          decreasing = TRUE)]

# Ensure the dat format is correct ----------------------------------------

  model_coefs2 <- data.frame(model_coefs2[ !rownames(model_coefs) %in% '(Intercept)',])

  model_coefs2[, 'variables'] <- factor(model_coefs2[, 'variables'],
                                        levels = model_coefs2_levels)
  model_coefs2[, 'Estimate'] <- as.numeric(model_coefs2[, 'Estimate'])
  model_coefs2[, 'min'] <- as.numeric(model_coefs2[, 'min'])
  model_coefs2[, 'max'] <- as.numeric(model_coefs2[, 'max'])
  model_coefs2[, 'pmin'] <- as.numeric(model_coefs2[, 'pmin'])
  model_coefs2[, 'pmax'] <- as.numeric(model_coefs2[, 'pmax'])




# Separate data into Demographics, socio-economic and environmental -------

  demo <- c( "sex1", "mar_stat1", "mar_stat2", "mar_stat3", "age",
             "fam_exp", "educ_yrs", "hh_size" )
  demo.data <- data.frame(model_coefs2)[model_coefs2[, 'variables'] %in% demo,]

  soci <- c("hybrid1", "maize_area_ha1", "fert_use1", "credit_acc1",
            "acc_ext1", "acc_ext_past1")
  soci.data <- data.frame(model_coefs2)[model_coefs2[, 'variables'] %in% soci,]

  envi <- c("min_2m_air_temp_yr", "min_2m_air_temp_yr_sqrd",
            "max_2m_air_temp_yr", "max_2m_air_temp_yr_sqrd",
            "precipitation_yr", "precipitation_yr_sqrd",
            "dis_market_min", "ext_km")
  envi.data <- data.frame(model_coefs2)[model_coefs2[, 'variables'] %in% envi,]

  model_coefs2[, 'variables'] <- factor(model_coefs2[, 'variables'],
                                        levels = c(demo, soci, envi))

  # Create Caterpillar plot -------------------------------------------------

  p <- ggplot2::ggplot(data.frame(model_coefs2)) +
    # ggplot2::geom_segment(ggplot2::aes(x = pmin, xend = pmax,
    #                                    y = variables, yend = variables),
    #                       size = 2, colour = 'brown') +
    ggplot2::geom_segment(data = demo.data,
                          mapping = ggplot2::aes(x = pmin, xend = pmax, #x = min, xend = max,
                                       y = variables, yend = variables),
                          size = 1, colour = 'blue') +
    ggplot2::geom_segment(data = soci.data,
                          mapping = ggplot2::aes(x = pmin, xend = pmax, #x = min, xend = max,
                                       y = variables, yend = variables),
                          size = 1, colour = 'pink') +
    ggplot2::geom_segment(data = envi.data,
                          mapping = ggplot2::aes(x = pmin, xend = pmax, #x = min, xend = max,
                                       y = variables, yend = variables),
                          size = 1, colour = 'brown') +
    ggplot2::geom_point(ggplot2::aes(x = Estimate, y = variables)) +
    ggplot2::geom_vline(xintercept = 0, linetype="dotted", color = "blue", size=1) +
    ggplot2::geom_label(ggplot2::aes(x=max, y=variables,  label = variables),
                        size= 3, nudge_x = 0.05, nudge_y = 0.001,
                        label.size = NA, label.padding = ggplot2::unit(0, "lines"),
                        inherit.aes = FALSE, fill = "transparent") +
    ggplot2::scale_x_continuous(name="Standardized Regression Coefficients",
                                limits=c(-0.3, 0.5),
                                breaks = round(seq(-0.3, 0.5, by=0.1), digits = 2)) +
    ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA,
                                                        size = 0.5,
                                                        linetype = 'solid',
                                                        colour = "black"),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   # axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank())

  model_coefs_outp[, 'Estimate'] <- round(as.numeric(model_coefs_outp[, 'Estimate']), digits = 4)
  model_coefs_outp[, 'Std. Error'] <- round(as.numeric(model_coefs_outp[, 'Std. Error']), digits = 4)
  model_coefs_outp[, 't value'] <- round(as.numeric(model_coefs_outp[, 't value']), digits = 4)
  model_coefs_outp[, 'min'] <- round(as.numeric(model_coefs_outp[, 'min']), digits = 4)
  model_coefs_outp[, 'max'] <- round(as.numeric(model_coefs_outp[, 'max']), digits = 4)
  model_coefs_outp[, 'pmin'] <- round(as.numeric(model_coefs_outp[, 'pmin']), digits = 4)
  model_coefs_outp[, 'pmax'] <- round(as.numeric(model_coefs_outp[, 'pmax']), digits = 4)

  # model_coefs_outp[, 'range']
  range0 <- paste0("(",
                   model_coefs_outp[, 'min'],
                   " - " ,
                   model_coefs_outp[, 'max'],
                   ")")

  list(plot = p, data=model_coefs_outp, data2=cbind(model_coefs_outp, range0))
}


#' Create caterpillar plot
#'
#' Create caterpillar plot that shows the model variable plus the standard coefficients range
#' @param model
#'
#' @return
#' @export
#'
table_3 <- function(modeldata=NULL){

  if(is.null(modeldata)) {
    modeldata0 <- yieldest::main(T, T)
    modeldata <- summary(modeldata0[['models0']])[["coefficients"]]
    modeldata_lm <- modeldata0[['pvalueList']][['pvalueall']]
  }

  list("model_ME" = round(modeldata, 4), "model_LM" = modeldata_lm)

  # model_coefs2 <- cbind(variables = rownames(model_coefs),
  #                       model_coefs,
  #                       min = model_coefs[, 'Estimate'] - model_coefs[, 'Std. Error'],
  #                       max = model_coefs[, 'Estimate'] + model_coefs[, 'Std. Error'],
  #                       pmin = model_coefs[, 'Estimate'] - 0.05,
  #                       pmax = model_coefs[, 'Estimate'] + 0.05)


  # Add max min values and pvalue range
  # model_summ <- summary(model)

  # model_coefs <- model[['coefficients']]
  # model_coefs2 <- cbind(variables = rownames(model_coefs),
  #                       model_coefs,
  #                       min = model_coefs[, 'Estimate'] - model_coefs[, 'Std. Error'],
  #                       max = model_coefs[, 'Estimate'] + model_coefs[, 'Std. Error'],
  #                       pmin = model_coefs[, 'Estimate'] - 0.05,
  #                       pmax = model_coefs[, 'Estimate'] + 0.05)
  #
  # # Order rows by 'Std. Error' or 'Estimate'
  # # return(model_coefs2)
  # # model_coefs2_levels <- model_coefs2[, 'variables'][order(as.character(model_coefs2[, 'variables']), decreasing = T)]
  # model_coefs2_levels <- model_coefs2[, 'variables'][order(as.numeric(model_coefs2[, "Pr...t.."]), decreasing = T)]
  # # print(data.frame(model_coefs2))
  # # model_coefs2_levels <- model_coefs2[, 'variables'][order(model_coefs2[, 'Std. Error'],
  # #                                                          decreasing = TRUE)]
  # # EEnsure the dat format is correct
  # model_coefs2 <- data.frame(model_coefs2[ !rownames(model_coefs) %in% '(Intercept)',])
  #
  # #Select significant variables
  # # return(model_coefs2)
  # model_coefs2 <- data.frame(model_coefs2[ as.numeric(model_coefs2[,"Pr...t.."]) < 0.05,])
  #
  # model_coefs2[, 'variables'] <- factor(model_coefs2[, 'variables'],
  #                                       levels = model_coefs2_levels)
  # model_coefs2[, 'Estimate'] <- as.numeric(model_coefs2[, 'Estimate'])
  # model_coefs2[, 'min'] <- as.numeric(model_coefs2[, 'min'])
  # model_coefs2[, 'max'] <- as.numeric(model_coefs2[, 'max'])
  # model_coefs2[, 'pmin'] <- as.numeric(model_coefs2[, 'pmin'])
  # model_coefs2[, 'pmax'] <- as.numeric(model_coefs2[, 'pmax'])
  #
  # # Create Caterpillar plot
  # p <- ggplot2::ggplot(data.frame(model_coefs2)) +
  #   # ggplot2::geom_segment(ggplot2::aes(x = pmin, xend = pmax,
  #   #                                    y = variables, yend = variables),
  #   #                       size = 2, colour = 'brown') +
  #   ggplot2::geom_segment(ggplot2::aes(x = min, xend = max,
  #                                      y = variables, yend = variables),
  #                         size = 1, colour = 'pink') +
  #   ggplot2::geom_point(ggplot2::aes(x = Estimate, y = variables)) +
  #   ggplot2::geom_vline(xintercept = 0, linetype="dotted", color = "blue", size=1) +
  #   ggplot2::geom_label(ggplot2::aes(x=max, y=variables,  label = variables),
  #                       size= 3, nudge_x = 0.05, nudge_y = 0.001,
  #                       label.size = NA, label.padding = ggplot2::unit(0, "lines"),
  #                       inherit.aes = FALSE, fill = "transparent") +
  #   ggplot2::scale_x_continuous(name="Standardized Regression Coefficients",
  #                               limits=c(-0.3, 0.5),
  #                               breaks = round(seq(-0.3, 0.5, by=0.1), digits = 2)) +
  #   ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA,
  #                                                       size = 0.5,
  #                                                       linetype = 'solid',
  #                                                       colour = "black"),
  #                  panel.grid.major = ggplot2::element_blank(),
  #                  panel.grid.minor = ggplot2::element_blank(),
  #                  panel.background = ggplot2::element_blank(),
  #                  # axis.text.x = ggplot2::element_blank(),
  #                  axis.text.y = ggplot2::element_blank(),
  #                  axis.ticks.y = ggplot2::element_blank(),
  #                  axis.title.y = ggplot2::element_blank(),
  #                  axis.text.x.top = TRUE)

  # list(plot = p, data=model_coefs2)
}
