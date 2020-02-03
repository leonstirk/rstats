source('scripts/flooding_data_processing.R')

require(rgdal)
require(sp)

coordinates(das) <- ~ lon_gd2000_x + lat_gd2000_y
dir <- file.path(getwd(), "shapefiles/das")
writeOGR(das, dir, "das", driver = "ESRI Shapefile")
