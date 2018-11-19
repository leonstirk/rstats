source('scripts/flooding_data_processing.R')

a <- das
## a <- flood_sub

## descriptives <- a %>% group_by(sale_year) %>% dplyr::summarise(n_sale_year = n(), mean_ln_sale_price = mean(ln_sale_price), std_error = sd(ln_sale_price)/sqrt(n_sale_year))

## tmp <- t(das[c(das_vars,"flood_analysis_group")] %>% group_by(flood_analysis_group) %>% summarise_all(funs(mean)))

## tmp <- a[c('net_sale_price',das_vars)] %>% summarise_all(funs(mean,median,sd,min,max))

mean <- clean_summary(t(a[c('net_sale_price','bedrooms','bathrooms','carparks','building_floor_area','land_area','median_income','homeowner_rate','arterial_street','offstreet_parking','deck','good_land_view','good_water_view')] %>% summarise_all(funs(mean))),4)

med <- clean_summary(t(a[c('net_sale_price','bedrooms','bathrooms','carparks','building_floor_area','land_area','median_income','homeowner_rate','arterial_street','offstreet_parking','deck','good_land_view','good_water_view')] %>% summarise_all(funs(median))),4)

sd <- clean_summary(t(a[c('net_sale_price','bedrooms','bathrooms','carparks','building_floor_area','land_area','median_income','homeowner_rate','arterial_street','offstreet_parking','deck','good_land_view','good_water_view')] %>% summarise_all(funs(sd))),4)

min <- clean_summary(t(a[c('net_sale_price','bedrooms','bathrooms','carparks','building_floor_area','land_area','median_income','homeowner_rate','arterial_street','offstreet_parking','deck','good_land_view','good_water_view')] %>% summarise_all(funs(min))),4)

max <- clean_summary(t(a[c('net_sale_price','bedrooms','bathrooms','carparks','building_floor_area','land_area','median_income','homeowner_rate','arterial_street','offstreet_parking','deck','good_land_view','good_water_view')] %>% summarise_all(funs(max))),4)
