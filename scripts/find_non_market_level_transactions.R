nonmktlvl_resid_model_formula <- as.formula(paste("ln_sale_price ~ ",model_all))
full_model_lm <- lm(nonmktlvl_resid_model_formula, data=das)

das$full_model_residuals <- resid(full_model_lm)
limits <- c(min(das$full_model_residuals),max(das$full_model_residuals))

residual_plot <- ggplot(das, aes(lon_gd2000_x, lat_gd2000_y, color = full_model_residuals)) + geom_point(position = "jitter") + scale_colour_gradientn(colours=rainbow(7), limits = limits) + coord_cartesian(xlim = c(170.4375,170.58), ylim = c(-45.825,-45.925))

print(paste(c("Residuals:",min(das$full_model_residuals),max(das$full_model_residuals)), collapse = " "))

## Biggest n negative residuals
## n <- 50
## nonmktlvl_transactions <- das[which(das$full_model_residuals %in% head(sort(das$full_model_residuals),n)),]

## Residuals below t standard errors
t <- -1.6
nonmktlvl_transactions <- das[which(das$full_model_residuals <= t),]

das <- das[which(!das$sale_id %in% nonmktlvl_transactions$sale_id),]

## Plot
nonmktlvl_plot <- ggplot(nonmktlvl_transactions, aes(lon_gd2000_x, lat_gd2000_y, color = full_model_residuals)) + geom_point(position = "jitter") + scale_colour_gradientn(colours=rainbow(7), limits = limits) + coord_cartesian(xlim = c(170.4375,170.58), ylim = c(-45.825,-45.925)) 

View(nonmktlvl_transactions)
