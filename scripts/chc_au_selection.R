source('scripts/das_data_preprocessing.R')

library(geosphere)

chc_au_size <- read.csv("datasets/chc_au_hs_size.csv")
chc_geo_key <- read.csv("datasets/chc_geographic_key.csv")

nz_mb_true_centroid <- read.csv("datasets/meshblock-2018-centroid-true.csv")
# nz_mb_inside_centroid <- read.csv("datasets/meshblock-2018-centroid-inside.csv")

##########################################################################################################
## Get rid of meshblocks outside Christchurch City Territorial Authority (code 60) and get vector of AUs #
##########################################################################################################
chc_city_aus <- chc_geo_key[which(chc_geo_key$Territorial.authority.code..2013.areas.==60),]
au_vec <- levels(as.factor(chc_city_aus$Area.unit.code..2013.areas.))

##########################################################################################################################################
## Within CHC TA, get rid of area units with confidential counts or low counts of fully detatched single-household residential dwellings #
##########################################################################################################################################
og_size <- nrow(chc_au_size)

chc_au_size <- chc_au_size[chc_au_size$Area.Unit.Code..2013.Areas. %in% au_vec,]

chc_au_size[chc_au_size == "..C"] <- NA
chc_au_size$Separate.House.2001 <- as.numeric(as.character(chc_au_size$Separate.House.2001))
chc_au_size$Separate.House.2006 <- as.numeric(as.character(chc_au_size$Separate.House.2006))
chc_au_size$Separate.House.2013 <- as.numeric(as.character(chc_au_size$Separate.House.2013))

chc_au_size <- subset(chc_au_size, !( Separate.House.2001=='NA' | Separate.House.2006=='NA' | Separate.House.2013=='NA'))
chc_au_size$mean_n <- (chc_au_size$Separate.House.2001 + chc_au_size$Separate.House.2006 + chc_au_size$Separate.House.2013)/3

filter_thresh <- 199
chc_au_size_filter <- chc_au_size[which(chc_au_size$mean_n > filter_thresh),]

n_filtered_units <- og_size - nrow(chc_au_size_filter)
print(n_filtered_units)
print(nrow(chc_au_size_filter))


######################################
## Integrate meshblock centroid data #
######################################

au_vec <- levels(as.factor(chc_au_size_filter$Area.Unit.Code..2013.Areas.))
chc_mb_true_centroid <- nz_mb_true_centroid[nz_mb_true_centroid$MB2018_V1_00 %in% au_vec,]
# chc_mb_inside_centroid <- nz_mb_inside_centroid[nz_mb_inside_centroid$MB2018_V1_00 %in% au_vec,]

names(chc_au_size_filter) <- c("au_id","au_name","n_house_2001","n_house_2006","n_house_2013","mean_n")
names(chc_mb_true_centroid) <- c("WKT","au_id","landwater_code","landwater_name","land_area_sqkm","total_area_sqkm","lat","lon","EASTING","NORTHING","SHAPE_X","SHAPE_Y")
# names(chc_mb_inside_centroid) <- c("WKT","au_id","landwater_code","landwater_name","land_area_sqkm","total_area_sqkm","lat","lon","EASTING","NORTHING","SHAPE_X","SHAPE_Y")

chc_au <- merge(chc_au_size_filter,chc_mb_true_centroid,by="au_id")

##############################################################################
## Define centre of Christchurch as the Christchurch Cathedral using Lat Lon #
##############################################################################

chc_cathedral <- c(172.637108, -43.530810)
chc_au$distance_from_centre <- distm(chc_cathedral, c(chc_au$lon, chc_au$lat), fun = distHaversine)

