## Define variable vectors #
das_vars       <- c("ln_sale_price", "building_floor_area", "land_area", "median_income", "homeowner_rate", "arterial_street", "offstreet_parking", "deck", "good_land_view", "good_water_view", "bedrooms", "bathrooms", "period_built", "contour", "dist_cbd", "sale_year")

##########################
## Descriptive variables #
##########################

des_vars       <- das_vars[c(1:12,15:16)]

#######################
## Matching variables #
#######################
mah_vars       <- das_vars[-1][1:4]
exact_vars     <- das_vars[-1][5:13]

####################
## Model variables #
####################

## Polynomials #
poly2_vars     <- mah_vars[1:3]
poly2_strings  <- sapply(poly2_vars, function(x) { paste(c("poly(", x, ", 2)"), collapse = "") })

model_vars     <- c(das_vars[1], mah_vars, exact_vars[-6], "dist_cbd", "sale_year")
model_strings  <- c(das_vars[1], poly2_strings, setdiff(mah_vars, c(poly2_vars)), exact_vars[-6], "dist_cbd", "sale_year")

## Include "bedrooms" in the comparative regressions #
# model_vars     <- c(das_vars[1], mah_vars, exact_vars, "dist_cbd", "sale_year")
# model_strings  <- c(das_vars[1], poly2_strings, setdiff(mah_vars, c(poly2_vars)), exact_vars, "dist_cbd", "sale_year")

## Define model formula strings #
model_all <- paste(tail(model_strings,-1), collapse = " + ")
model_mah <- paste(mah_vars, collapse = " + ")
model_cem <- paste(c(mah_vars,exact_vars), collapse = " + ")
