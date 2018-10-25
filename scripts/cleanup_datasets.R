rm(list=ls(all=TRUE))

das <- read.csv('datasets/dud_allsales_2000to2018.csv')

# Drop variables
v <- names(das) %in% c("area_no","new","shau","medincome","floorarea","med_income")
das <- das[!v]
rm(v)

names(das) <- c('qpid','sale_id','sale_date','sale_quarter','cpi','sale_year','net_sale_price','real_net_sale_price','ln_real_net_sale_price','ln_net_sale_price','physical_address','meshblock_id','area_unit_id','area_unit_name','land_area','building_floor_area','year_built','carparks','bedrooms','bathrooms','legal_description','ct_no','period_built','contour','property_ownership_type','view_type','wall_construction_material','deck','view_scope','good_water_view','good_land_view','decade_built','offstreet_parking','sale_year','nearest_census_year','median_income','homeowner_rate','hnzc_rate','median_age','european','ex_state_house','wkt_geom','lon_gd2000_x','lat_gd2000_y','full_roa','sale_order','sale_max','sale_multiplier','years_between','yearly_gain')

i <- sapply(das, is.factor)
das[i] <- lapply(das[i], as.character)

das[das==''] <- NA
das$land_area[das$land_area==0] <- NA
das$building_floor_area[das$building_floor_area==0] <- NA
das$year_built[das$year_built==0] <- NA
das$decade_built[das$decade_built==0] <- NA

# Date variable conversion
das$sale_date <- as.Date(das$sale_date, '%d/%m/%Y')

# Create new variables
das$age_at_time_of_sale <- das$sale_year - das$year_built
das$ln_building_floor_area <- log(das$building_floor_area)
das$land_area <- das$land_area*100
das$ln_land_area <- log(das$land_area)

save(das, file="/home/rstudio/dud_allsales_2000to2018.Rda")
