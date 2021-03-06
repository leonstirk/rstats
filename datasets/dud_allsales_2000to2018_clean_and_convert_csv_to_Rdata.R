## take dataset dud_allsales_2000to2018.csv clean it and convert to an R dataframe

rm(list=ls(all=TRUE))

das <- read.csv('dud_allsales_2000to2018.csv')

# Drop variables
v <- names(das) %in% c("area_no","new","shau","medincome","floorarea","med_income")
das <- das[!v]
rm(v)

names(das) <- c(
  'qpid',
  'sale_id',
  'sale_date',
  'sale_quarter',
  'cpi',
  'sale_year',
  'net_sale_price',
  'real_net_sale_price',
  'ln_real_net_sale_price',
  'ln_net_sale_price',
  'physical_address',
  'meshblock_id',
  'area_unit_id',
  'area_unit_name',
  'land_area',
  'building_floor_area',
  'year_built',
  'carparks',
  'bedrooms',
  'bathrooms',
  'legal_description',
  'ct_no',
  'period_built',
  'contour',
  'property_ownership_type',
  'view_type',
  'wall_construction_material',
  'deck',
  'view_scope',
  'good_water_view',
  'good_land_view',
  'decade_built',
  'offstreet_parking',
  'sale_year',
  'nearest_census_year',
  'median_income',
  'homeowner_rate',
  'hnzc_rate',
  'median_age',
  'european',
  'ex_state_house',
  'wkt_geom',
  'lon_gd2000_x',
  'lat_gd2000_y',
  'full_roa',
  'sale_order',
  'sale_max',
  'sale_multiplier',
  'years_between',
  'yearly_gain'
)

## convert all factor variables to character variables #
i <- sapply(das, is.factor)
das[i] <- lapply(das[i], as.character)

## convert blank entries to NA #
das[das==''] <- NA

## convert entries for 0 land area and 0 building floor area to NA #
das$land_area[das$land_area==0] <- NA
das$building_floor_area[das$building_floor_area==0] <- NA

## convert entries with 0 year built and 0 decade built to NA #
das$year_built[das$year_built==0] <- NA
das$decade_built[das$decade_built==0] <- NA

# Date variable conversion #
das$sale_date <- as.Date(das$sale_date, '%d/%m/%Y')

########################
# Create new variables #
########################

das$ln_building_floor_area <- log(das$building_floor_area)
das$land_area <- das$land_area*100 ## convert land_area from 100m^2 to m^2
das$ln_land_area <- log(das$land_area)

###########################
## Add distance variables #
###########################

## Set coordinates #
cbd <- c(170.503628,-45.874166)

## Calculate distance vectors #

coordinate_pairs <- cbind(das$lon_gd2000_x, das$lat_gd2000_y)
das$dist_cbd <- distm(coordinate_pairs, cbd, fun = distHaversine)/1000

#######################################
## Save the dataframe as an Rdatafile #
#######################################

save(das, file="~/Desktop/projects/matching_estimator_flooding_south_dunedin_2000_2018/datasets/dud_allsales_2000to2018.Rda")
