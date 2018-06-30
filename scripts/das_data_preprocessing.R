rm(list=ls(all=TRUE))

library(MatchIt)
library(dplyr)
library(ggplot2)
library(McSpatial)
library(reshape2)

setwd("/home/ubuntu/rstats")

load("datasets/dud_allsales_2000to2015.Rda")

das<-dud_allsales
rm(dud_allsales)

# Date variable conversion
das$sale_date <- as.Date(das$sale_date)

# Create new variables
das$age_at_time_of_sale <- das$sale_year - das$year_built
das$ln_building_floor_area <- log(das$building_floor_area)
das$land_area <- das$land_area_100sqm*100
das$ln_land_area <- log(das$land_area)

# Rename variables
names(das)[21] <- "view_scope"

# Drop variables
v <- names(das) %in% c("sale_year.1")
das <- das[!v]
rm(v)

# Factor variable conversions
das$property_ownership_type <- as.factor(das$property_ownership_type)
das$wall_construction_material <- as.factor(das$wall_construction_material)
das$view_type <- as.factor(das$view_type)
das$period_built <- as.factor(das$period_built)
das$contour <- as.factor(das$contour)
das$view_scope <- as.factor(das$view_scope)
das$view_type <- as.factor(das$view_type)

das$offstreet_parking <- as.factor(das$offstreet_parking)
das$deck <- as.factor(das$deck)
das$ex_state_house <- as.factor(das$ex_state_house)


## Relevel factors if needed

das <- within(das, view_type <- relevel(view_type, ref = 3))




## Set vector of variables

das_vars <- c("ln_sale_price","bedrooms","bathrooms","carparks","offstreet_parking","deck","ex_state_house","contour","period_built","view_scope","view_type","ln_building_floor_area","ln_land_area")

## das_vars <- c("ln_sale_price","bedrooms","bathrooms","carparks","offstreet_parking","deck","ex_state_house","ln_building_floor_area","ln_land_area")




## Generate dummy matrices

genDummy <- function(v, lab) {
  f <- factor(v)
  labs <- paste(lab, levels(f)[c(-1)], sep = "")
  dummies <- model.matrix(~f)
  dummies <-  data.frame(dummies)[c(-1)]
  names(dummies) <- labs
  return(dummies)
}

# genDummy(das$sale_year)

# dummies <- data.frame(cbind(genDummy(das$view_type, ""),genDummy(das$view_scope, "view_scope"),genDummy(das$contour, "contour"),genDummy(das$period_built, "period_built")))

# View(dummies)

## Generate descriptives on the full sample

# fs_means <- sapply(das[das_vars], mean, na.rm=TRUE)
# fs_stdev <- sapply(das[das_vars], sd, na.rm=TRUE)

# fs_ds <- data.frame(fs_means, fs_stdev)
# fs_ds <- format.data.frame(fs_ds, scientific=FALSE)

# rm(fs_means, fs_stdev)


## Sample descriptives on area unit

au_summ <- c('','','')

getAUNameFromID <- function(data, id) {
 return(data[which(data$area_unit_id == id),]$area_unit_name[1])
}

countByVar <- function(data, key, value) {
 return(nrow(data[which(data[,key] == value),]))
}

au_ids <- levels(as.factor(das$area_unit_id))
for (id in au_ids) {
 row <- cbind(id,getAUNameFromID(das,id),countByVar(das,"area_unit_id",id))
 au_summ <- rbind(au_summ, row)
}

rm(id,row)

au_summ <- tail(au_summ, -1)
au_summ <- data.frame(au_summ)
names(au_summ) <- c('id','area_unit_name','count')
au_summ$count <- as.numeric(as.character(au_summ$count))

au_names <- as.vector(au_summ$area_unit_name)

## Subset on area unit

subsetByVar <- function(data, key, value) {
 return(data[which(data[,key] == value),])
}

das_concord <- das[which(das$area_unit_id == '605920'),] 	 # 726

# das_brockville <- das[which(das$area_unit_id == '603930'),]	 # 1040
# das_musselburgh <- das[which(das$area_unit_id == '604611'),]	 # 1134
# das_wakari <- das[which(das$area_unit_id == '603910'),]	 # 1287
# das_vauxhall <- das[which(das$area_unit_id == '604620'),]	 # 1420
# das_stclair <- das[which(das$area_unit_id == '604500'),]	 # 1503
# das_mornington <- das[which(das$area_unit_id == '604110'),]	 # 1615
# das_nev <- das[which(das$area_unit_id == '603300'),]		 # 1626
das_caversham <- das[which(das$area_unit_id == '604210'),]	 # 2214

model_lhs_vars <- paste(tail(das_vars,-1), collapse = " + ")