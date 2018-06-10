library(MatchIt)
library(dplyr)
library(ggplot2)
library(McSpatial)

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
names(das)[39] <- "sale_year_index"

# Factor variable conversions
das$property_ownership_type <- as.factor(das$property_ownership_type)
das$wall_construction_material <- as.factor(das$wall_construction_material)
das$view_type <- as.factor(das$view_type)

# Generate descriptives

das_vars <- names(das)[c(5,6,8,11:17,35,68:71)]

fs_means <- sapply(das[das_vars], mean, na.rm=TRUE)
fs_stdev <- sapply(das[das_vars], sd, na.rm=TRUE)

fs_ds <- data.frame(fs_means, fs_stdev)
fs_ds$fs_means <- prettyNum(fs_ds$fs_means, scientific=FALSE, big.mark=",")
fs_ds$fs_stdev <- prettyNum(fs_ds$fs_stdev, scientific=FALSE, big.mark=",")
View(fs_ds)

# source("scripts/rep_s.R")