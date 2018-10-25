source('scripts/flooding_m.R')

flooding_geo_plot <- ggplot(flood_sub, aes(x=lon_gd2000_x, y=lat_gd2000_y)) + geom_jitter(aes(color = after_flood, shape = after_flood))
