library(ggmap)
library(ggplot2)

mapImage <- get_map(location = c(lon = 170.5150, lat = -45.8750), zoom = 12)

ggmap(mapImage) + geom_point(data = das, aes(lon_gd2000, lat_gd2000))