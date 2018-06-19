source('scripts/das_data_preprocessing.R')

mean <- aggregate(das_caversham[, c('ln_sale_price', 'net_sale_price', 'ln_building_floor_area')], list(das_caversham$sale_year), mean)
names(mean)[1] <- 'sale_year'

median <- aggregate(das_caversham[, c('ln_sale_price', 'net_sale_price', 'ln_building_floor_area')], list(das_caversham$sale_year), median)
names(median)[1] <- 'sale_year'

a <- mean

# a$ln_period_change <- c(NA,diff(a$ln_sale_price, lags = 1))
# a$ln_index <- a$ln_sale_price - a$ln_sale_price[1]
a$index <- a$net_sale_price/a$net_sale_price[1]

iplot <- ggplot(data=a, aes(x=sale_year, y=index)) + geom_line()
# lnplot <- ggplot(data=a, aes(x=sale_year, y=growth)) + geom_line()