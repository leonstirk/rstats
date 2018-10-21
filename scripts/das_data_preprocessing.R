rm(list=ls(all=TRUE))

library(data.table)
library(MatchIt)
library(dplyr)
library(ggplot2)
library(McSpatial)
library(reshape2)
library(zeligverse)

setwd("/home/ubuntu/rstats")

#####################
## Define functions #
#####################

subsetByVar <- function(data, key, value) {
 return(data[which(data[,key] == value),])
}

getArithmeticMeanIndexSeries <- function(data_subset, variable, period) {
  meanSeries <- aggregate(data_subset[, variable], list(data_subset[,period]), mean)
  meanSeries <-  data.frame(meanSeries)
  names(meanSeries) <- c(period,variable)
  index <- (meanSeries[,variable] - meanSeries[1,variable])+1
  return(index)
}

getSampleSize <- function(data_subset, period) {
  samplesize <- aggregate(data_subset[,1], list(data_subset[,period]), length)
  samplesize <-  data.frame(samplesize)
  names(samplesize) <- c(period, "n")
  series <- (samplesize[,"n"])
  return(series)
}

checkFactorLength <- function(data, v) {
 return(length(levels(as.factor(as.vector(data[,v])))))
}

eliminateSingleLevelFactors <- function(df) {
 factor_vars <- names(which(sapply(df, class) == "factor"))

 factor_lengths <- sapply(factor_vars, function(x) {checkFactorLength(df, x)})
 null_factors <- as.vector(names(which(factor_lengths < 2)))

 l <- nrow(df)
 z <- c(rep(0, times = l))
 df[null_factors] <- z
 return(df)
}

genDummy <- function(v, lab) {
  f <- factor(v)
  labs <- paste(lab, levels(f)[c(-1)], sep = "")
  dummies <- model.matrix(~f)
  dummies <-  data.frame(dummies)[c(-1)]
  names(dummies) <- labs
  return(dummies)
}

############################################################################################################
############################################################################################################

load("datasets/dud_allsales_2000to2018.Rda")

#####################################################################################################
## Remove a few observations that had no data we can find this data using the missing_data.R script #
#####################################################################################################
das <- na.omit(data.table(das), cols=c(1:47,51:53))
das <- data.frame(das)

## Factor variable conversions #
f <- c("sale_quarter","meshblock_id","area_unit_id","area_unit_name","legal_description","ct_no","period_built","contour","property_ownership_type","view_type","wall_construction_material","view_scope")
das[f] <- lapply(das[f],as.factor)
rm(f)

## Character conversions #
c <- c("qpid","sale_id")
das[c] <- lapply(das[c],as.character)
rm(c)

# ## Dummy variables
# das$ex_state_house <- as.factor(das$ex_state_house)
# das$offstreet_parking <- as.factor(das$offstreet_parking)
# das$deck <- as.factor(das$deck)

# ## Relevel factors if needed
# das <- within(das, view_type <- relevel(view_type, ref = 3))

# ## Dummy variable assignment for view
# ## There are 7 dummies 'none', 'poor land', 'ok land', 'good land', 'poor water', 'ok water', 'good water'
# das$poor_land  <- ifelse(das$view_scope == 2 & das$view_type == "Focal Point Of view - Other", 1, 0)
# das$ok_land    <- ifelse(das$view_scope == 3 & das$view_type == "Focal Point Of view - Other", 1, 0)
# das$good_land  <- ifelse(das$view_scope == 4 & das$view_type == "Focal Point Of view - Other", 1, 0)
# das$poor_water <- ifelse(das$view_scope == 2 & das$view_type == "Focal Point Of view - Water", 1, 0)
# das$ok_water   <- ifelse(das$view_scope == 3 & das$view_type == "Focal Point Of view - Water", 1, 0)
# das$good_water <- ifelse(das$view_scope == 4 & das$view_type == "Focal Point Of view - Water", 1, 0)

# ## THIS MIGHT BE BETTER AS JUST "VIEW SCOPE" PLUS A "WATER" DUMMY ####

# Drop variables
v <- names(das) %in% c("sale_year.1","hnzc_rate","legal_description","ct_no")
das <- das[!v]
rm(v)


############################
## Set vector of variables #
############################

names(das)[names(das) == "ln_real_net_sale_price"] <- "ln_sale_price" # OR set "ln_real_net_sale_price"

# das_vars <- c("ln_sale_price","bedrooms","bathrooms","carparks","offstreet_parking","deck","ex_state_house","contour","period_built","poor_land","ok_land","good_land","poor_water","ok_water","good_water","ln_building_floor_area","ln_land_area")

das_vars <- c("ln_sale_price", "bedrooms", "bathrooms", "ln_building_floor_area", "ln_land_area", "median_income", "homeowner_rate", "age_at_time_of_sale")

vars <- c("qpid","area_unit_id","area_unit_name","sale_year","ln_sale_price","bedrooms","bathrooms","carparks","offstreet_parking","deck","ex_state_house","contour","period_built","poor_land","ok_land","good_land","poor_water","ok_water","good_water","ln_building_floor_area","ln_land_area")

## Generate dummy matrices


# dummies <- data.frame(cbind(
#  genDummy(das$sale_year, "sale_year"),
#  genDummy(das$contour, "contour"),
#  genDummy(das$period_built, "period_built"),
#  genDummy(das$property_ownership_type, "ownership_type"),
#  genDummy(das$wall_construction_material, "wall_material")
# ))

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
au_years <- as.vector(levels(as.factor(das$sale_year)))
au_quarters <- as.vector(levels(as.factor(das$sale_quarter)))

## Subset on area unit

das_concord <- das[which(das$area_unit_id == '605920'),] 	 # 726

# das_brockville <- das[which(das$area_unit_id == '603930'),]	 # 1040
# das_musselburgh <- das[which(das$area_unit_id == '604611'),]	 # 1134
# das_wakari <- das[which(das$area_unit_id == '603910'),]	 # 1287
# das_vauxhall <- das[which(das$area_unit_id == '604620'),]	 # 1420
das_stclair <- das[which(das$area_unit_id == '604500'),]	 # 1503
das_mornington <- das[which(das$area_unit_id == '604110'),]	 # 1615
# das_nev <- das[which(das$area_unit_id == '603300'),]		 # 1626
das_caversham <- das[which(das$area_unit_id == '604210'),]	 # 2214

das_opoho <- das[which(das$area_unit_id == '603210'),]		 # 452
das_roslynsouth <- das[which(das$area_unit_id == '604020'),]	 # 936
das_maorihill <- das[which(das$area_unit_id == '603710'),]	 # 709

model_lhs_vars <- paste(tail(das_vars,-1), collapse = " + ")

nplot <- ggplot(data=das, aes(x=reorder(area_unit_name,area_unit_id,length))) + geom_bar() + theme(axis.text.x=element_text(angle = -90, hjust = 0))
