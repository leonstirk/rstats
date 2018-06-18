source('scripts/das_data_preprocessing.R')

mean <- aggregate(das_caversham[, c('ln_sale_price', 'net_sale_price', 'ln_building_floor_area')], list(das_caversham$sale_year), mean)

median <- aggregate(das_caversham[, c('ln_sale_price', 'net_sale_price', 'ln_building_floor_area')], list(das_caversham$sale_year), median)

a <- median

a$growth <- c(NA,diff(a$ln_sale_price, lags = 1))
a$log_index <- a$ln_sale_price - a$ln_sale_price[1]
a$index <- a$net_sale_price/a$net_sale_price[1]

iplot <- ggplot(data=a, aes(x=Group.1, y=index)) + geom_line()
lnplot <- ggplot(data=a, aes(x=Group.1, y=growth)) + geom_line()

# Concord == '605920'