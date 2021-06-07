#' getCountyName
#'
#' A function to down load data for specified administrative level and return
#' the dataframe with added columns
#'
#' @param df Dataframe with longitude and latitude columns
#' @param longitude Name of longitude column in df
#' @param latitude Name of latitude column in df
#'
#' @return dataframe with added columns
#' @export

addAdminName <- function(df, longitude = "lon", latitude = "lat",
                         country = 'KEN', level = 1, admin.name = "NAME_1"){

  # Load/Downdload Kenya counties shapefile
  admin.sf <- sf::st_as_sf(raster::getData('GADM',
                                           country = country,
                                           level = level,
                                           path = "inst/extdata")
                           )

  # Remove columns with NA in coordinates
  df <- df[complete.cases(df[,c(longitude, latitude)]), ]

  # Ensure latitude and longitude values are numeric
  df[, longitude] <- base::as.numeric(df[, longitude])
  df[, latitude] <- base::as.numeric(df[, latitude])


  # Ensure sf uses s2 instead of GEOS
  # See https://cran.r-project.org/web/packages/sf/vignettes/sf7.html
  if (!sf::sf_use_s2()) sf::sf_use_s2(TRUE)

  # Extrating Admin names
  print("Extrating Admin names")
  starttime <- base::Sys.time()
  adNames <- sapply(1:dim(df)[1], function(x) {
    # Create a geometry object
    df.geometry <- sf::st_point(base::as.matrix(df[x, c(longitude, latitude)]))

    # Update the user on progress
      # Get remaining time
    time.remain <- base::difftime(Sys.time(), starttime, units = "secs") * ((dim(df)[1] -  x)/x)
    min.remain <- base::floor(time.remain/60)
    sec.remain <- base::floor(time.remain -(min.remain*60))
      # Show progress and remaining time
    cat("\r", paste("Extrating Admin names. Progress ",
                    100*base::round(x/base::dim(df)[1], 2), "%",
                    ". Approximate remaining time is", min.remain, " minutes  ",
                    sec.remain, " seconds "
                    ))

    # Return admin name
    as.data.frame(admin.sf[df.geometry, ])[, admin.name]

  })

  # Ensure column names are ok
  df[paste0("Admin_", level)] <- adNames

  # Save the data
  cat(paste0("Saving file with admin data to: ",
             "inst/extdata/MaizeYieldHHdata",
             round(as.numeric(Sys.time())),
             ".csv"))

  write.csv(df, paste0("inst/extdata/MaizeYieldHHdata",
                       round(as.numeric(Sys.time())),
                       ".csv"), row.names = FALSE)

  cat("\r",
      paste0("Saved file in: ",
             "inst/extdata/MaizeYieldHHdata",
             round(as.numeric(Sys.time())),
             ".csv", "\n\n"))

  # Return the data
  df
}
