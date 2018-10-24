rm(list=ls(all=TRUE))

library(data.table)
library(MatchIt)
library(plyr)
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
  f <- as.factor(v)
  labs <- paste(lab, levels(f)[c(-1)], sep = "_")
  dummies <- model.matrix(~f)
  dummies <-  data.frame(dummies)[c(-1)]
  names(dummies) <- labs
  return(dummies)
}

#######################################################################################################################################
#######################################################################################################################################

load("datasets/dud_allsales_2000to2018.Rda")

#####################################################################################################
## Remove a few observations that had no data we can find this data using the missing_data.R script #
#####################################################################################################
das <- na.omit(data.table(das), cols=c(1:47,51:53))
das <- data.frame(das)

################################
## Factor variable conversions #
################################
f <- c("sale_quarter","meshblock_id","area_unit_id","area_unit_name","legal_description","ct_no","period_built","contour","property_ownership_type","view_type","wall_construction_material","view_scope", "full_roa")
das[f] <- lapply(das[f],as.factor)
rm(f)

##########################
## Character conversions #
##########################
c <- c("qpid","sale_id")
das[c] <- lapply(das[c],as.character)
rm(c)

####################
## Relevel factors #
####################
das <- within(das, period_built <- relevel(period_built, ref = 2))
das <- within(das, contour <- relevel(contour, ref = 2))

#########################
## Rename factor levels #
#########################
tmp <- levels(das$property_ownership_type)
das$property_ownership_type <- mapvalues(das$property_ownership_type, from = tmp, to = c("core_crown", "crown", "local_authority","private_company","private_individual"))

tmp <- levels(das$contour)
das$contour <- mapvalues(das$contour, from = tmp, to = c("level","easy_moderate","steep"))

tmp <- levels(das$period_built)
das$period_built <- mapvalues(das$period_built, from = tmp, to = c("1800s","1900to70","70s80s","post2000"))

rm(tmp)

###################
## Drop variables #
###################

v <- names(das) %in% c("sale_year.1","hnzc_rate","legal_description","ct_no","view_type","view_scope")
das <- das[!v]
rm(v)


############################################
## Generate dummy matrices and bind to das #
############################################

dummies <- data.frame(cbind(
#  genDummy(das$sale_year, "sale_year"),
 genDummy(das$period_built, "period_built")
#  genDummy(das$contour, "contour"),
#  genDummy(das$property_ownership_type, "ownership_type"),
#  genDummy(das$wall_construction_material, "wall_material")
))
dummy_vars_from_gen <- names(dummies)
das <- cbind(das,dummies)


##########################################################################
## Set ln_sale_price to use nominal or inflation adjusted log sale price #
##########################################################################
names(das)[names(das) == "ln_real_net_sale_price"] <- "ln_sale_price" # OR set "ln_net_sale_price"

############################
## Set vector of variables #
############################

## Index
# das_vars <- c("ln_sale_price", "bedrooms", "bathrooms", "ln_building_floor_area", "ln_land_area", "median_income", "homeowner_rate", "age_at_time_of_sale")

## Flooding
das_vars <- c("ln_sale_price", "bedrooms", "bathrooms", "carparks", "ln_building_floor_area", "ln_land_area", "median_income", "homeowner_rate", "arterial_street", "deck", dummy_vars_from_gen)

mah_vars <- c("bedrooms", "bathrooms", "ln_building_floor_area", "ln_land_area", "median_income", "homeowner_rate")

#############################################
## Generate descriptives on the full sample #
#############################################

# fs_means <- sapply(das[das_vars], mean, na.rm=TRUE)
# fs_stdev <- sapply(das[das_vars], sd, na.rm=TRUE)

# fs_ds <- data.frame(fs_means, fs_stdev)
# fs_ds <- format.data.frame(fs_ds, scientific=FALSE)

# rm(fs_means, fs_stdev)

#####################################
## Sample descriptives on area unit #
#####################################

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
