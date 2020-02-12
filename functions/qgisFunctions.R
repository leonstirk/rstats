source('scripts/flooding_data_processing.R')

require(rgdal)
require(sp)

coordinates(das) <- ~ lon_gd2000_x + lat_gd2000_y
dir <- file.path(getwd(), "shapefiles/das")
writeOGR(das, dir, "das", driver = "ESRI Shapefile")

##############################################################################


library(rgdal)
library(rgeos)

## download w/o wasting bandwidth
## URL <- "ftp://dnrftp.dnr.ne.gov/pub/data/state/Legislative2010UTM.zip"
## fil <- basename(URL)
## if (!file.exists(fil)) download.file(URL, fil)

## unzip & get list of files
fils <- unzip(fil)

## find the shapefile in it
shp <- grep("shp$", fils, value=TRUE)

## get the first layer from it
lay <- ogrListLayers(shp)[1]

## read in the shapefile
leg <- readOGR(shp, lay)

## get the centroids and then convert them to a SpatialPointsDataFrame
leg_centers <- SpatialPointsDataFrame(gCentroid(leg, byid=TRUE), leg@data, match.ID=FALSE)
