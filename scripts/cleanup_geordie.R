rm(list=ls(all=TRUE))

chas <- read.dta13('datasets/chcGeordieData.dta')

## cat steet names together
chas$full_roa <- do.call(paste, chas[c('street_name','street_name_suffix')])

# Drop variables
v <- names(chas) %in% c("land_area","agenum","floor_area2","twobeds","dafterxtc2","dafterxtc3","street_name","street_name_suffix","daynum","bvsa","dafter")
chas <- chas[!v]
rm(v)

names(chas) <- c("qpid","sale_date","tc","sale_year","sale_quarter","net_sale_price","ln_sale_price","suburb","meshblock_id","area_unit_id","area_unit_name","median_income","ln_median_income","land_area","decade_built","floor_area","building_footprint_area","contour","view","view_scope","deck","mas_no_main_roof_garages","mas_free_standing_garages","wall_construction_material","roof_construction_material","bedrooms","tc2","tc3","full_roa")

i <- sapply(chas, is.factor)
chas[i] <- lapply(chas[i], as.character)
rm(i)

chas[chas==''] <- NA
chas[chas==' -   '] <- NA

chas$deck <- mapvalues(chas$deck, from = c("Y","N"), to = c(1,0))
chas$deck <- as.numeric(chas$deck)

# Date variable conversion
chas$sale_date <- as.Date(chas$sale_date, '%Y-%m-%d')

## Save file
save(chas, file="/home/rstudio/chc_geordie_data.Rda")
