## nonmktlvl_resid_model_formula <- as.formula(paste("ln_sale_price ~ ",model_all))
nonmktlvl_resid_model_formula <- as.formula(paste("net_sale_price ~ ",model_all))
full_model_lm <- lm(nonmktlvl_resid_model_formula, data=das)


## Set graph boundaries

graphBound<- function(data_x, data_y) {
    xlim <- c(min(data_x)-0.001, max(data_x)+0.001)
    ylim <- c(min(data_y)-0.001, max(data_y)+0.001)
    return(list(xlim, ylim))
}

xylim <- graphBound(das$lon_gd2000_x, das$lat_gd2000_y)

das$full_model_residuals <- resid(full_model_lm)
limits <- c(min(das$full_model_residuals),max(das$full_model_residuals))



residual_plot <- ggplot(das, aes(lon_gd2000_x, lat_gd2000_y, color = full_model_residuals)) + geom_point() + scale_colour_gradientn(colours=rainbow(7), limits = limits) + coord_cartesian(xlim = xylim[[1]], ylim = xylim[[2]]) + theme_grey()

residual_plot <- residual_plot + scale_x_continuous(breaks = sort(c(xylim[[1]],ggplot_build(residual_plot)$layout$panel_ranges[[1]]$x.major_source))) + scale_y_continuous(breaks = sort(c(xylim[[2]],ggplot_build(residual_plot)$layout$panel_ranges[[1]]$y.major_source)))







print(paste(c("Residuals:",min(das$full_model_residuals),max(das$full_model_residuals)), collapse = " "))










## Biggest n negative residuals
n <- 50
nonmktlvl_transactions <- das[which(das$full_model_residuals %in% head(sort(das$full_model_residuals),n)),]

## Residuals below t standard errors
## t <- -1.6
## nonmktlvl_transactions <- das[which(das$full_model_residuals <= t),]

das_mktlevel <- das[which(!das$sale_id %in% nonmktlvl_transactions$sale_id),]

## Plot
nonmktlvl_plot <- ggplot(nonmktlvl_transactions, aes(lon_gd2000_x, lat_gd2000_y, color = full_model_residuals)) + geom_point(position = "jitter") + scale_colour_gradientn(colours=rainbow(7), limits = limits) + coord_cartesian(xlim = c(170.4375,170.58), ylim = c(-45.825,-45.925)) 

## View(nonmktlvl_transactions)
