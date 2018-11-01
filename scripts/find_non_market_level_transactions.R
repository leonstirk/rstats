## source('scripts/das_data_preprocessing.R')
## source("scripts/flooding_data_processing.R")

nonmktlvl_model_vars <- c(das_vars, sale_year_dummies)

nonmktlvl_resid_model_formula <- as.formula(paste("ln_sale_price ~ ",tail(nonmktlvl_model_vars, -1)))

coord_lm <- lm(nonmktlvl_resid_model_formula, data=das)

das$coord_res <- resid(coord_lm)

residual_plot <- ggplot(das, aes(lon_gd2000_x, lat_gd2000_y, color = coord_res)) + geom_point(position = "jitter") + scale_colour_gradientn(colours=rainbow(7), limits = c(-3.02,2.25)) + coord_cartesian(xlim = c(170.4375,170.58), ylim = c(-45.825,-45.925)) 

## Biggest n negative residuals
## n <- 10
## nonmktlvl_transactions <- das[which(das$coord_res %in% head(sort(das$coord_res),n)),]

## Residuals below t standard errors
t <- -2
nonmktlvl_transactions <- das[which(das$coord_res <= t),]

## Plot
nonmktlvl_plot <- ggplot(nonmktlvl_transactions, aes(lon_gd2000_x, lat_gd2000_y, color = coord_res)) + geom_point(position = "jitter") + scale_colour_gradientn(colours=rainbow(7), limits = c(-3.02,2.25)) + coord_cartesian(xlim = c(170.4375,170.58), ylim = c(-45.825,-45.925)) 

View(nonmktlvl_transactions)
