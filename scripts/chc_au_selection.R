chc_au_size <- read.csv("datasets/chc_au_hs_size.csv")
chc_geo_key <- read.csv("datasets/chc_geographic_key.csv")

nz_meshblock_true_centroid <- read.csv("datasets/meshblock-2018-centroid-true.csv")
nz_meshblock_inside_centroid <- read.csv("datasets/meshblock-2018-centroid-inside.csv")

##########################################################################################################
## Get rid of meshblocks outside Christchurch City Territorial Authority (code 60) and get vector of AUs #
##########################################################################################################
chc_city_aus <- chc_geo_key[which(chc_geo_key$Territorial.authority.code..2013.areas.==60),]
au_vec <- 



###########################################################################################################################
## Get rid of area units with confidential counts or low counts of fully detatched single-household residential dwellings #
###########################################################################################################################
chc_au_size[chc_au_size == "..C"] <- NA
chc_au_size$Separate.House.2001 <- as.numeric(as.character(chc_au_size$Separate.House.2001))
chc_au_size$Separate.House.2006 <- as.numeric(as.character(chc_au_size$Separate.House.2006))
chc_au_size$Separate.House.2013 <- as.numeric(as.character(chc_au_size$Separate.House.2013))

chc_au_size <- subset(chc_au_size, !( Separate.House.2001=='NA' | Separate.House.2006=='NA' | Separate.House.2013=='NA'))
chc_au_size$mean_n <- (chc_au_size$Separate.House.2001 + chc_au_size$Separate.House.2006 + chc_au_size$Separate.House.2013)/3

filter_thresh <- 199
chc_au_size_filter <- chc_au_size[which(chc_au_size$mean_n > filter_thresh),]

n_filtered_units <- nrow(chc_au_size) - nrow(chc_au_size_filter)
print(n_filtered_units)

######################################
## Integrate meshblock centroid data #
######################################

