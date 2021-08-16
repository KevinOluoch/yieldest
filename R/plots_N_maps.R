
#' # Load/Download  shapefiles from gadm.org
#'
#' Downloads in sp format then converts to sf
#'
#' @param country Country code
#' @param level Admin level
#'
#' @return Shapefile in sf format
#' @export
#'
admin_sf <- function(country, level = 0) {
  sf::st_as_sf(raster::getData('GADM',
                               country = country,
                               level = level,
                               path = "inst/extdata")
  )
}


weatherPlots<- function() {

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



mainviolinPlot <- function() {

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

plots_N_maps <- function(studyCounties = NULL) {

  # SKIPPED: data already has Admin data
  data.path <- yieldest::system.file("inst/extdata/MaizeYieldHHdata.csv",
                                     package = "yieldest")
  data1 <- utils::read.csv(data.path, stringsAsFactors = TRUE)
  tza0.sf <- yieldest::admin_sf(country="TZA", level = 0)
  uga0.sf <- yieldest::admin_sf(country="UGA", level = 0)
  ssd0.sf <- yieldest::admin_sf(country="SSD", level = 0)
  eth0.sf <- yieldest::admin_sf(country="ETH", level = 0)
  som0.sf <- yieldest::admin_sf(country="SOM", level = 0)

  ken0.sf <- yieldest::admin_sf(country="KEN", level = 0)
  ken1.sf <- yieldest::admin_sf(country="KEN", level = 1)
  ken1.sf <- ken1.sf[order(as.numeric(ken1.sf$CC_1)),]
  studyCounties <- unique(data1[, "Admin_1"])
  ken.studyCounties <- ken1.sf[as.data.frame(ken1.sf)[,"NAME_1"]  %in% studyCounties, ]

  # Color labels for counties
  county_id <- ken1.sf$CC_1
  stdycounty_id <- ken.studyCounties$CC_1
  color.label <- rep("black", 47)
  color.label[county_id %in% stdycounty_id] <- rep("brown", length(stdycounty_id))


  # Legend table
  ken1.df <- data.frame(ken1.sf)
  rownames(ken1.df) <- NULL
  ken1.legend.data <- ken1.df[, c("CC_1", "NAME_1")]
  names(ken1.legend.data) <- c("ID", "County")
  # Color labels for counties
  county_id <- ken1.df$CC_1
  stdycounty_id <- ken.studyCounties$CC_1
  color.label <- rep("black", 47)
  color.label[county_id %in% stdycounty_id] <- rep("brown", length(stdycounty_id))

  county_legend.theme1 <-
    gridExtra::ttheme_default(base_size = 8,
                              base_colour = "black",
                              base_family = "",
                              parse = FALSE,
                              padding = grid::unit(c(2, 2), "mm"),
                              core = list(fg_params=list(col=color.label[1:24])))
  county_legend1 <- gridExtra::tableGrob(ken1.legend.data[1:24,], rows = NULL,
                                        theme = county_legend.theme1)
  county_legend.theme2 <-
    gridExtra::ttheme_default(base_size = 8,
                              base_colour = "black",
                              base_family = "",
                              parse = FALSE,
                              padding = grid::unit(c(2, 2), "mm"),
                              core = list(fg_params=list(col=color.label[25:47])))
  county_legend2 <- gridExtra::tableGrob(ken1.legend.data[25:47,], rows = NULL,
                                        theme = county_legend.theme2)
     # Legend object
  county_legend <- gridExtra::gtable_combine(county_legend1,
                                             county_legend2,
                                             along=1)

  # Load africa shapefiles
  africaWater <-
    sf::st_as_sf(raster::shapefile("inst/extdata/africawaterbody/Africa_waterbody.shp"))
  africa <-
    sf::st_as_sf(raster::shapefile("inst/extdata/africa/WB_countries_Admin0_10m.shp"))
  africa <- africa[as.data.frame(africa)[,"CONTINENT"]  %in% "Africa", ]


  # Get the map limits
  area.lim <- sf::st_bbox(ken1.sf)
  # print(area.lim)
  min.x <- area.lim["xmin"]
  max.x <- area.lim["xmax"]
  min.y <- area.lim["ymin"]
  max.y <- area.lim["ymax"]

  map.inset <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = africa, fill= NA, colour="black", size = 0.4, linetype = "solid") +
    ggplot2::geom_sf(data = ken1.sf, colour= NA, fill= "ivory1") +
    ggplot2::geom_rect(mapping = ggplot2::aes(xmin = min.x, xmax = max.x,
                                              ymin = min.y, ymax = max.y,),
                       colour = "red", fill = NA, size = 1) +
    ggplot2::theme(panel.border = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "transparent",
                                                             colour = NA),
                   plot.background = ggplot2::element_rect(fill = "transparent",
                                                           colour = NA),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank())

  map.main <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = africa, colour= "black", fill= "khaki1",
                     size = 0.4, linetype = "solid") +
    ggplot2::geom_sf(data = tza0.sf, colour="black", fill= "khaki1", size = 0.8) +
    ggplot2::geom_sf(data = uga0.sf, colour="black", fill= "khaki1", size = 0.8) +
    ggplot2::geom_sf(data = ssd0.sf, colour="black", fill= "khaki1", size = 0.8) +
    ggplot2::geom_sf(data = eth0.sf, colour="black", fill= "khaki1", size = 0.8) +
    ggplot2::geom_sf(data = som0.sf, colour="black", fill= "khaki1", size = 0.8) +
    # ggplot2::geom_sf(data = africa,
    #                  colour="black", fill= NA,  size = 0.4, linetype = "solid") +
    ggplot2::geom_sf(data = ken1.sf, colour= NA, fill= "ivory1") +
    ggplot2::geom_sf(data = ken.studyCounties, colour= "NA", fill= "ivory2",) +
    ggplot2::geom_sf(data = africaWater, colour=NA, fill= "lightblue") +

    ggplot2::geom_sf(data = ken1.sf,
                 colour=" light grey", fill= NA,  size = 0.4, linetype = "dashed") +
    ggplot2::geom_sf(data = ken.studyCounties,
                 colour=" dark grey", fill=NA, size = 0.4, linetype = "solid") +
    ggplot2::geom_sf(data = ken0.sf, colour="black", fill=NA, size = 0.8) +
    ggplot2::geom_sf(data = tza0.sf, colour="black", fill=NA, size = 0.8) +
    ggplot2::geom_sf(data = uga0.sf, colour="black", fill=NA, size = 0.8) +
    ggplot2::geom_sf(data = ssd0.sf, colour="black", fill=NA, size = 0.8) +
    ggplot2::geom_sf(data = eth0.sf, colour="black", fill=NA, size = 0.8) +
    ggplot2::geom_sf(data = som0.sf, colour="black", fill=NA, size = 0.8) +
    ggplot2::geom_label(data = ken1.sf,
                        mapping = ggplot2::aes(label = CC_1, col = NAME_1,
                                               geometry = geometry),
                        stat = "sf_coordinates", size= 2, colour = color.label,
                        #nudge_x = 0.02, nudge_y = -0.01,
                        label.size = NA, label.padding = ggplot2::unit(0, "lines"),
                        label.r = ggplot2::unit(0, "lines"), inherit.aes = FALSE,
                        fill = "transparent") +
    # ggrepel::geom_label_repel(data = ken.studyCounties,
    #                           mapping = ggplot2::aes(label = NAME_1,
    #                                                  geometry = geometry),
    #                           stat = "sf_coordinates", size= 2,
    #                           #nudge_x = 0.02, nudge_y = -0.01,
    #                           label.size = NA, label.padding = ggplot2::unit(0, "lines"),
    #                           label.r = ggplot2::unit(0, "lines"), inherit.aes = FALSE,
    #                           fill = "transparent") +

    # ggplot2::geom_sf(data = africa,
    #                  colour="black", fill= NA,  size = 0.4, linetype = "solid") +



  ggplot2::annotation_custom(grob = county_legend, #xmin=42, ymin=-4,
                             xmax=max.x+12, ymax=max.y) +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(fill = NA,
                                                        size = 0.5,
                                                        linetype = 'solid',
                                                        colour = "black"),
                   panel.background = ggplot2::element_rect(fill = "lightblue",
                                                            colour = "lightblue",
                                                            size = 0.5,
                                                            linetype = "solid"))+
    # ggplot2::coord_equal()+
    ggplot2::coord_sf(xlim=c(min.x, max.x), ylim=c(min.y, max.y)) +
    ggplot2::labs(x="Longitude",y="Latitude") +
    ggsn::north(x.min = min.x, x.max = max.x, y.min = min.y, y.max = max.y,
                scale = .1, symbol = 1) +
    ggsn::scalebar(x.min = min.x, x.max = max.x,
                     y.min = min.y, y.max = max.y,
                     dist = 100, dist_unit = "km", #ceiling((max.x - min.x)*20) + 1
                     transform = TRUE, location = "bottomright",
                     st.size = 3, height = 0.01)

  # map.main
  # # map.inset
  map.all <- cowplot::ggdraw()+
    cowplot::draw_plot(map.main) +
    cowplot::draw_plot(map.inset, x = 0.63, y = 0.1, width = 0.2, height = 0.2)

  # ggplot2::ggsave(filename = "../inst/extdata/studyCounties.png", plot = map.all,
  #                 width = 1300, height = 1000, units = "px")
  map.all
}
