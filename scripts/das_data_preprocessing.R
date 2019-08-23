rm(list=ls(all=TRUE))

require(geosphere)
require(data.table)
require(MatchIt)
require(plyr)
require(dplyr)
require(ggplot2)
# require(McSpatial)
require(reshape2)
# require(zeligverse)
require(effects)
library(scales)
# require(xtable)
library(huxtable)
library(broom)
# require(cem)

#####################
## Define functions #
#####################

specify_decimal <- function(x, k) format(round(x, k), nsmall=k, scientific = FALSE)

clean_summary  <- function(lmcoef, digits) {
    coefs <- as.data.frame(lmcoef)
    coefs[] <- lapply(coefs, function(x) specify_decimal(x, digits))
    coefs
}

makeDescriptives <- function(x) {
    descriptives <- c('mean', 'median', 'sd', 'min', 'max')
    df <- data.frame(matrix(, nrow = ncol(x), ncol = 0))
    for(i in descriptives) {
        df[,i] <- sapply(x, i, na.rm = TRUE)
    }
    rownames(df) <- colnames(x)
    return(df)
}

## subsetByVar <- function(data, key, value) {
##  return(data[which(data[,key] == value),])
## }

## getArithmeticMeanIndexSeries <- function(data_subset, variable, period) {
##   meanSeries <- aggregate(data_subset[, variable], list(data_subset[,period]), mean)
##   meanSeries <-  data.frame(meanSeries)
##   names(meanSeries) <- c(period,variable)
##   index <- (meanSeries[,variable] - meanSeries[1,variable])+1
##   return(index)
## }

## getSampleSize <- function(data_subset, period) {
##   samplesize <- aggregate(data_subset[,1], list(data_subset[,period]), length)
##   samplesize <-  data.frame(samplesize)
##   names(samplesize) <- c(period, "n")
##   series <- (samplesize[,"n"])
##   return(series)
## }

## checkFactorLength <- function(data, v) {
##  return(length(levels(as.factor(as.vector(data[,v])))))
## }

## eliminateSingleLevelFactors <- function(df) {
##  factor_vars <- names(which(sapply(df, class) == "factor"))

##  factor_lengths <- sapply(factor_vars, function(x) {checkFactorLength(df, x)})
##  null_factors <- as.vector(names(which(factor_lengths < 2)))

##  l <- nrow(df)
##  z <- c(rep(0, times = l))
##  df[null_factors] <- z
##  return(df)
## }

## Generate a dataframe of dummy variables from a single factor variable with multiple levels #
## genDummy <- function(v, lab) {
##   f <- as.factor(v)
##   labs <- paste(lab, levels(f)[c(-1)], sep = "_")
##   dummies <- model.matrix(~f)
##   dummies <-  data.frame(dummies)[c(-1)]
##   names(dummies) <- labs
##   return(dummies)
## }

#######################################################################################################################################
#######################################################################################################################################

load("datasets/dud_allsales_2000to2018.Rda")

#####################################################################################################
## Remove a few observations that had no data we can find this data using the missing_data.R script #
#####################################################################################################

## convert entries for 0 land_area and 0 building_floor_area to NA (remove what could plausibly be 'apartments' and 'empty lots' from data) #
das$land_area[das$land_area==0] <- NA
das$building_floor_area[das$building_floor_area==0] <- NA

das <- na.omit(data.table(das), cols=c(1:47,51:53))
das <- data.frame(das)

##################
## Add variables #
##################

## Arterial road dummy
arterial_road_vec <- c("Andersons Bay Road","Bay View Road","Castle Street","Corstophine Road","Cumberland Street","Eglinton Road","Forbury Road","George Street","Great King Street","Highgate","High Street","Hillside Road","Kaikorai Valley Road","Kenmure Road","King Edward Street","Macandrew Road","Mailer Street","Main South Road","Maitland Street","Malvern Street","Musselburgh Rise","North Road","Opoho Road","Pine Hill Road","Prince Albert Road","Queens Drive","South Road","Stevenson Road","Victoria Road")
das$arterial_street <- ifelse(das$full_roa %in% arterial_road_vec,1,0)

rm(arterial_road_vec)

################################
## Factor variable conversions #
################################
f <- c("sale_year", "sale_quarter","meshblock_id","area_unit_id","area_unit_name","decade_built","legal_description","ct_no","period_built","contour","property_ownership_type","view_type","wall_construction_material","view_scope", "full_roa", "bedrooms", "bathrooms", "deck", "offstreet_parking", "arterial_street")
das[f] <- lapply(das[f],as.factor)
rm(f)

####################
## Relevel factors #
####################
das$bedrooms <- relevel(das$bedrooms, ref = '3')
das$bathrooms <- relevel(das$bathrooms, ref = '1')

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

######################
## Scale conversions #
######################

das$median_income <- das$median_income/10000

############################################
## Generate dummy matrices and bind to das #
############################################

## dummies <- data.frame(cbind(
##    ## genDummy(das$period_built, "period_built")
## ))
## dummy_vars_from_gen <- names(dummies)
## das <- cbind(das,dummies)

## sale_year_dummies <- data.frame(cbind(
##     genDummy(das$sale_year, "sale_year")
## ))
## das <- cbind(das,sale_year_dummies)

##########################################################################
## Set ln_sale_price to use nominal or inflation adjusted log sale price #
##########################################################################
names(das)[names(das) == "ln_net_sale_price"] <- "ln_sale_price" # OR set "ln_real_net_sale_price" instead of "ln_net_sale_price"

###################
## Set flood_date #
###################
flood_date <- as.Date('2015-06-04')
