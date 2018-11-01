# source("scripts/das_data_preprocessing.R")
# source("scripts/flooding_data_processing.R")

## library(scatterplot3d)
## library(rgl)

spatial_resid_model_formula <- as.formula(paste("ln_sale_price ~ ",model_all))

## control for time?? #
a_sub <- das[which(das$sale_year == "2002"),]

coord_lm <- lm(spatial_resid_model_formula, data=a_sub)

a_sub$coord_res <- resid(coord_lm)

m <- ggplot(a_sub, aes(lon_gd2000_x, lat_gd2000_y, color = coord_res)) + geom_point(position="jitter") + scale_colour_gradientn(colours=rainbow(7))
