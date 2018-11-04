source('scripts/flooding_data_processing.R')

require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")


library(proj4)
library(maps)
library(mapproj)

proj4string <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"

dud = readOGR(dsn="./shapefiles/AU_TA_Dunedin_CC/", layer="AU_TA_Dunedin_CC")
dud@data$id = rownames(dud@data)
dud.points = fortify(dud, region="id")
dud.df = join(dud.points, dud@data, by="id")
dud.df <- dud.df[which(dud.df$AU_NAME %in% das$area_unit_name),]
p <- as.data.frame(proj4::project(dud.df[c('long','lat')], proj=proj4string, inverse=TRUE))

dud.df$long <- (p$x)
dud.df$lat <- p$y


dud_plot <- ggplot(dud.df) +
    aes(long,lat,group=group,fill=AU_NAME) +
    geom_polygon() +
    geom_path(color="white") +
    coord_equal()
## + geom_point(data=das, aes(lon_gd2000_x, lat_gd2000_y))
