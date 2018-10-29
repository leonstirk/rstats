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

specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

clean_summary  <- function(lmcoef, digits) {
    coefs <- as.data.frame(lmcoef)
    coefs[] <- lapply(coefs, function(x) specify_decimal(x, digits))
    coefs
}

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
## og_das <- das

#####################################################################################################
## Remove a few observations that had no data we can find this data using the missing_data.R script #
#####################################################################################################
das <- na.omit(data.table(das), cols=c(1:47,51:53))
das <- data.frame(das)

################################
## Factor variable conversions #
################################
f <- c("sale_quarter","meshblock_id","area_unit_id","area_unit_name","decade_built","legal_description","ct_no","period_built","contour","property_ownership_type","view_type","wall_construction_material","view_scope", "full_roa")
das[f] <- lapply(das[f],as.factor)
rm(f)

##########################
## Character conversions #
##########################
c <- c("qpid","sale_id")
das[c] <- lapply(das[c],as.character)
rm(c)

#########################
## Rename factor levels #
#########################
tmp <- levels(das$property_ownership_type)
das$property_ownership_type <- mapvalues(das$property_ownership_type, from = tmp, to = c("core_crown", "crown", "local_authority","private_company","private_individual"))

tmp <- levels(das$contour)
das$contour <- mapvalues(das$contour, from = tmp, to = c("easy_moderate","level","steep"))

tmp <- levels(das$period_built)
das$period_built <- mapvalues(das$period_built, from = tmp, to = c("1900to70s","1800s","80s90s","post2000"))

rm(tmp)

###################
## Drop variables #
###################

v <- names(das) %in% c("sale_year.1","hnzc_rate","legal_description","ct_no","view_type","view_scope")
das <- das[!v]
rm(v)

##################
## Add variables #
##################

## Arterial road dummy
arterial_road_vec <- c("Andersons Bay Road","Bay View Road","Castle Street","Corstophine Road","Cumberland Street","Eglinton Road","Forbury Road","George Street","Great King Street","Highgate","High Street","Hillside Road","Kaikorai Valley Road","Kenmure Road","King Edward Street","Macandrew Road","Mailer Street","Main South Road","Maitland Street","Malvern Street","Musselburgh Rise","North Road","Opoho Road","Pine Hill Road","Prince Albert Road","Queens Drive","South Road","Stevenson Road","Victoria Road")
das$arterial_street <- ifelse(das$full_roa %in% arterial_road_vec,1,0)


############################################
## Generate dummy matrices and bind to das #
############################################

dummies <- data.frame(cbind(
    #  genDummy(das$sale_year, "sale_year"),
    genDummy(das$bedrooms, "bedrooms"),
    genDummy(das$bathrooms, "bathrooms"),
    genDummy(das$period_built, "period_built"),
    genDummy(das$contour, "contour")
    #  genDummy(das$decade_built, "decade_built")
    #  genDummy(das$property_ownership_type, "ownership_type"),
    #  genDummy(das$wall_construction_material, "wall_material")
))
dummy_vars_from_gen <- names(dummies)
das <- cbind(das,dummies)


##########################################################################
## Set ln_sale_price to use nominal or inflation adjusted log sale price #
##########################################################################
names(das)[names(das) == "ln_net_sale_price"] <- "ln_sale_price" # OR set "ln_real_net_sale_price"

############################
## Set vector of variables #
############################

## Index

## Flooding
das_vars <- c("ln_sale_price", "carparks", "building_floor_area", "land_area", "median_income", "homeowner_rate", "arterial_street", "offstreet_parking", "deck", "good_land_view", "good_water_view", dummy_vars_from_gen)
mah_vars <- c("carparks", "building_floor_area", "land_area", "median_income", "homeowner_rate")
exact_vars <- c("good_land_view", "good_water_view", "offstreet_parking", "arterial_street", "deck", dummy_vars_from_gen)
model_vars <- c(das_vars, "I(building_floor_area^2)", "I(land_area^2)","I(median_income^2)")

## Subset on area unit #
das_concord <- das[which(das$area_unit_id == '605920'),] 	 # 726
## das_brockville <- das[which(das$area_unit_id == '603930'),]	 # 1040
## das_musselburgh <- das[which(das$area_unit_id == '604611'),]	 # 1134
## das_wakari <- das[which(das$area_unit_id == '603910'),]	 # 1287
## das_vauxhall <- das[which(das$area_unit_id == '604620'),]	 # 1420
## das_stclair <- das[which(das$area_unit_id == '604500'),]	 # 1503
## das_mornington <- das[which(das$area_unit_id == '604110'),]	 # 1615
## das_nev <- das[which(das$area_unit_id == '603300'),]		 # 1626
## das_caversham <- das[which(das$area_unit_id == '604210'),]	 # 2214
## das_opoho <- das[which(das$area_unit_id == '603210'),]		 # 452
## das_roslynsouth <- das[which(das$area_unit_id == '604020'),]	 # 936
## das_maorihill <- das[which(das$area_unit_id == '603710'),]	 # 709

## Define model formulae #
model_all <- paste(tail(model_vars,-1), collapse = " + ")
model_mah <- paste(mah_vars, collapse = " + ")

## Import functions #
source('functions/match_samples.R')

## Other shit
au_names <- levels(das$area_unit_name)
