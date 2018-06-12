source("scripts/das_data_preprocessing.R")

library(scatterplot3d)
library(rgl)

das_vars <- names(das)[c(5,6,8,11:17,35,68:71)]

f1 <- ln_sale_price ~ ln_land_area + ln_building_floor_area + bedrooms + bathrooms + deck + contour + carparks + offstreet_parking
dta <- das[which(das$sale_year == "2000"),]
coord.lm = lm(f1, data=dta)
coord.res = resid(coord.lm)
scatterplot3d(dta$lon_gd2000, dta$lat_gd2000, coord.res, main = "Coordinate Residuals", highlight.3d=TRUE, type="h",angle=85)