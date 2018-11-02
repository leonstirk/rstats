#!/usr/bin/env Rscript

## library(plyr)
## library(ggmap)
## library(maptools)
## library(proj4)

## library(RColorBrewer)
## colors <- brewer.pal(9, "BuGn")

## # Auckland
## # mapImage <- get_map(location = c(lon=174.7633, lat=-36.8485), zoom = 9)
## # proj4string <- "+proj=nzmg +lat_0=-41.0 +lon_0=173.0 +x_0=2510000.0 +y_0=6023150.0 +ellps=intl +units=m"
## # area <- readShapePoly("/home/ubuntu/rstats/shapefiles/AU_RC_Auckland_RC/AU_RC_Auckland_RC")

## # Dunedin
## mapImage <- get_map(location = c(lon=170.5028, lat=-45.8788), zoom = 12)
## proj4string <- "+proj=nzmg +lat_0=-41.0 +lon_0=173.0 +x_0=2510000.0 +y_0=6023150.0 +ellps=intl +units=m"
## area <- readShapePoly("/home/ubuntu/rstats/shapefiles/AU_TA_Dunedin_CC/AU_TA_Dunedin_CC")

## area <- area[area@data$AU_NO %in% levels(as.factor(das$area_unit_id)),]

## area.points <- fortify(area)

## pj <- project(area.points[,c("long", "lat")], proj4string, inverse = TRUE)

## latlon <- data.frame(lat=pj$y, long=pj$x)

## area.points <- rename(area.points, c("long" = "NZMGlong", "lat" = "NZMGlat"))

## area.points$lat <- latlon$lat
## area.points$long <- latlon$lon

## meshblockMap <- ggmap(mapImage) +
##   geom_polygon(aes(x = long,
##       y = lat,
##       group = group),
##     data = area.points,
##     color = colors[9],
##     fill = colors[6],
##     alpha = 0.5) +
## labs(x = "Longitude",
##   y = "Latitude")








require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")

## utah = readOGR(dsn="./shapefiles/utah_eg/", layer="eco_l3_ut")
## utah@data$id = rownames(utah@data)
## utah.points = fortify(utah, region="id")
## utah.df = join(utah.points, utah@data, by="id")

## utah <- ggplot(utah.df) +
##     aes(long,lat,group=group,fill=LEVEL3_NAM) +
##     geom_polygon() +
##     geom_path(color="white") +
##     coord_equal() +
##     scale_fill_brewer("Utah Ecoregion")

dud = readOGR(dsn="./shapefiles/AU_TA_Dunedin_CC/", layer="AU_TA_Dunedin_CC")
dud@data$id = rownames(dud@data)
dud.points = fortify(dud, region="id")
dud.df = join(dud.points, dud@data, by="id")

dud_plot <- ggplot(dud.df) +
    aes(long,lat,group=group,fill=AU_NAME) +
    geom_polygon() +
    geom_path(color="white") +
    coord_equal() +
    scale_fill_brewer("dud aus")
