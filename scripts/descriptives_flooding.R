source('scripts/flooding_data_processing.R')

a <- das
## a <- flood_sub

## descriptives <- a %>% group_by(sale_year) %>% dplyr::summarise(n_sale_year = n(), mean_ln_sale_price = mean(ln_sale_price), std_error = sd(ln_sale_price)/sqrt(n_sale_year))

## tmp <- t(das[c(das_vars,"flood_analysis_group")] %>% group_by(flood_analysis_group) %>% summarise_all(funs(mean)))

## tmp <- a[c('net_sale_price',das_vars)] %>% summarise_all(funs(mean,median,sd,min,max))


## mean <- clean_summary(t(a[c('net_sale_price','bedrooms','bathrooms','carparks','building_floor_area','land_area','median_income','homeowner_rate','arterial_street','offstreet_parking','deck','good_land_view','good_water_view')] %>% summarise_all(funs(mean))),4)

## med <- clean_summary(t(a[c('net_sale_price','bedrooms','bathrooms','carparks','building_floor_area','land_area','median_income','homeowner_rate','arterial_street','offstreet_parking','deck','good_land_view','good_water_view')] %>% summarise_all(funs(median))),4)

## sd <- clean_summary(t(a[c('net_sale_price','bedrooms','bathrooms','carparks','building_floor_area','land_area','median_income','homeowner_rate','arterial_street','offstreet_parking','deck','good_land_view','good_water_view')] %>% summarise_all(funs(sd))),4)

## min <- clean_summary(t(a[c('net_sale_price','bedrooms','bathrooms','carparks','building_floor_area','land_area','median_income','homeowner_rate','arterial_street','offstreet_parking','deck','good_land_view','good_water_view')] %>% summarise_all(funs(min))),4)

## max <- clean_summary(t(a[c('net_sale_price','bedrooms','bathrooms','carparks','building_floor_area','land_area','median_income','homeowner_rate','arterial_street','offstreet_parking','deck','good_land_view','good_water_view')] %>% summarise_all(funs(max))),4)


tmp <- as.data.frame(cbind(labels(homeowner_rates),homeowner_rates,median_incomes,dist_cbds))
tmp$homeowner_rates <- as.numeric(as.character(tmp$homeowner_rates))
tmp$median_incomes <- as.numeric(as.character(tmp$median_incomes))
tmp$dist_cbd <- as.numeric(as.character(tmp$dist_cbd))

tmp$V1 <- as.character(tmp$V1)

excluded_areas <- c(student_areas, harbour_areas)

tmp$excluded_area <- as.factor(ifelse(tmp$V1 %in% excluded_areas,"Excluded","Included"))

## Homeowner rates

## plot <- ggplot(tmp, aes(reorder(V1,-homeowner_rates), homeowner_rates, fill = excluded_area)) + geom_bar(stat='identity', width = 0.6, alpha = 0.8) + labs(x = "Area unit", y = "Rate of homeownership", fill = "Flooding sample") + theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

## h <- 0.45

## plot <- plot + geom_hline(aes(yintercept=h)) + scale_y_continuous(expand = c(0,0), breaks = sort(c(ggplot_build(plot)$layout$panel_ranges[[1]]$y.major_source, h)))


## Distance from CBD

plot <- ggplot(tmp, aes(reorder(V1,-dist_cbd), dist_cbd, fill = excluded_area)) + geom_bar(stat='identity', width = 0.6, alpha = 0.8) + labs(x = "Area unit", y = "Average distance from CBD", fill = "Flooding sample") + theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

h <- 5

plot <- plot + geom_hline(aes(yintercept=h)) + scale_y_continuous(expand = c(0,0), breaks = sort(c(ggplot_build(plot)$layout$panel_ranges[[1]]$y.major_source, h)))
