createdata <- function(){

prep1 <- "../data/weather/Precipitation/precipitation1971_present"
ken0.precipitation <- extractadmin(prep1, level = 0, years=1979:2021)
usethis::use_data(ken0.precipitation, overwrite = TRUE)

ken1.precipitation <- extractadmin(prep1, level = 1, years=1979:2021)
usethis::use_data(ken1.precipitation, overwrite = TRUE)


Tmin1 <- "../data/weather/Temperature/Tmin1979_present"
ken0.Tmin <- extractadmin(Tmin1, level = 0, years=1979:2021)
usethis::use_data(ken0.Tmin, overwrite = TRUE)

ken1.Tmin <- extractadmin(Tmin1, level = 1, years=1979:2021)
usethis::use_data(ken1.Tmin, overwrite = TRUE)


Tmax1 <- "../data/weather/Temperature/Tmax1979_present"
ken0.Tmax <- extractadmin(Tmax1, level = 0, years=1979:2021)
usethis::use_data(ken0.Tmax, overwrite = TRUE)

ken1.Tmax <- extractadmin(Tmax1, level = 1, years=1979:2021)
usethis::use_data(ken1.Tmax, overwrite = TRUE)


ken0.Tave <- list()
file.names <- names(ken0.Tmin)
for (Tmin.name in file.names) {
  year_ <- stringr::str_extract(Tmin.name, "[[:digit:]]{4}" )
  Tmax.name <- names(ken0.Tmax)[grepl(year_, names(ken0.Tmax))]
  ken0.Tave[[sub("min", "ave", Tmax.name)]] <- (ken0.Tmin[[Tmin.name]] + ken0.Tmax[[Tmax.name]])/2
}
usethis::use_data(ken0.Tave, overwrite = TRUE)

ken1.Tave <- list()
file.names <- names(ken1.Tmin)
for (Tmin.name in file.names) {
  year_ <- stringr::str_extract(Tmin.name, "[[:digit:]]{4}" )
  Tmax.name <- names(ken1.Tmax)[grepl(year_, names(ken1.Tmax))]
  ken1.Tave[[sub("min", "ave", Tmax.name)]] <- ken1.Tmin[[Tmin.name]] + ken1.Tmax[[Tmax.name]]
}
usethis::use_data(ken1.Tave, overwrite = TRUE)
}

