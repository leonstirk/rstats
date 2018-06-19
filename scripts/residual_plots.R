source("scripts/das_data_preprocessing.R")

library(scatterplot3d)
library(rgl)

a <- das_caversham

model_lhs_vars <- paste(tail(das_vars,-1), collapse = " + ")

modelstring <- paste(das_vars[1],"~",model_lhs_vars)

dta <- a[which(a$sale_year == "2001"),]
coord.lm = lm(modelstring, data=dta)

coord.res = resid(coord.lm)

scatterplot3d(dta$lon_gd2000, dta$lat_gd2000, coord.res, main = "Coordinate Residuals", highlight.3d=TRUE, type="h", angle=80)