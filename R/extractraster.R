
#' Extract mean value per area
#'
#' Extract mean values for the selected admin level
#'
#' This function extracts data from ncd4(.nc) files in the given folder, and
#' returns the mean for every administration unit in the given administration
#' level. Not all files in the folder are used(extracted from). The function
#' selects file with the given years in their file name eg for 2010 you can
#' have precip.2008.nc, 2008.nc, Tmin008.nc etc
#'
#' @param folderpath Path to folder with '*.nc' files
#' @param country abbreviation for country name (see https://gadm.org)
#' @param level Administration level
#' @param years A vector of years to select
#'
#' @return Returns a list of dataframes for each year selected
#' @export
extractadmin <- function(folderpath, country="KEN", level=1, years=1981:2010) {

  # Get admin level data
  admin.sf <- sf::st_as_sf(raster::getData('GADM',
                                           country = country,
                                           level = level,
                                           path = "inst/extdata")
  )
  # Get the full path of rasters in ncd4 format (.nc extension)
  r_paths <- list.files(folderpath, full.names = TRUE, pattern = "*.nc")

  # Get file path of selected years
  r_paths.years <- NULL
  for (year_ in years) {
    r_paths.years <- c(r_paths.years, r_paths[base::grep(year_, base::basename(r_paths))])

  }

  # get admin level names to use as as rowname
  adminlevelname <- paste0("NAME_", level)
  admin.df <- base::as.data.frame(admin.sf)
  adminlevelnameS <- admin.df[, adminlevelname]

  # Load and extract the rasters of the selected years
  extracted.data <- a <- list()

  starttime <- base::Sys.time()
  i = 0

  for (r_path in r_paths.years) {
    i = i + 1
    file_name <- base::basename(r_path)
    r <- raster::stack(r_path)
    extract0 <- raster::extract(r,
                                admin.sf,
                                method='simple',
                                fun=function(x, ...)base::mean(x[!x %in% NA])
                                )
    base::rownames(extract0) <- adminlevelnameS
    extract0 <- base::round(extract0)
#
#     if (file_name %in% c("precip.1981.nc", "precip.1982.nc", "precip.1983.nc")){
#       a[[file_name]] <- extract0
#       usethis::use_data(a, overwrite = TRUE)
#     }

    yieldest::progressReport(i = i,
                             total_i = base::length(r_paths.years),
                             starttime = starttime,
                             Message_ = paste0("Extracting rasters: Extracted ",
                                               file_name))
    extracted.data[[file_name]] <- extract0
  }



  extracted.data
}


#' Admin level mean per year
#'
#' Get the mean value in the admin level in the selected years
#'
#' @inheritParams extractadmin
#' @param extractnewdata
#'
#' @return
#' @export
extractYearlyMean <- function(folderpath, country="KEN", level=1, years=1981:2010,
                              extractnewdata = FALSE, extracted.data = NULL){

  if (extractnewdata){
    extracted.data <- extractadmin(folderpath, country="KEN", level=1, years=1981:2010)
  } else{
    # Get file path of selected years
    r_paths.years <- NULL
    if (is.null(extracted.data)){
      print("provide extracted.data")
      return(NULL)
    }

    extracted.data.names <- names(extracted.data)
    selectedYears <- NULL
    for (year_ in years) {
      selectedYears <- c(selectedYears,
                         extracted.data.names[base::grep(year_,
                                                         base::basename(extracted.data.names))])
    }
    averageYears <- NULL
    for (year_ in 1981:2010) {
      averageYears <- c(averageYears,
                         extracted.data.names[base::grep(year_,
                                                         base::basename(extracted.data.names))])
    }

    extracted.data.ave <- extracted.data[averageYears]
    extracted.data <- extracted.data[selectedYears]
  }


  # Get the daily mean for each year and add the vectors to a single dataframe
  extractYrMean <- NULL
  for (i in extracted.data){
    extractYrMean <-  base::cbind(extractYrMean, base::rowMeans(i, na.rm = TRUE))
    # break
  }
  base::colnames(extractYrMean ) <- names(extracted.data)
  # Get the daily mean between 1981 and 2010 and get the overall mean
  extractYrMean1 <- NULL
  for (i in extracted.data.ave){
    extractYrMean1 <- base::cbind(extractYrMean1, base::rowMeans(i, na.rm = TRUE))
      # break
  }



  list(allmean = base::rowMeans(extractYrMean1, na.rm = TRUE),
       extractYrMean = extractYrMean,
       extracted.data = extracted.data
       )

}

