source('scripts/das_data_preprocessing.R')

a <- das_caversham

mean <- aggregate(a[, c('ln_sale_price', 'net_sale_price', 'ln_building_floor_area')], list(a$sale_year), mean)
names(mean)[1] <- 'sale_year'

median <- aggregate(a[, c('ln_sale_price', 'net_sale_price', 'ln_building_floor_area')], list(a$sale_year), median)
names(median)[1] <- 'sale_year'

b <- mean

# b$ln_period_change <- c(NA,diff(b$ln_sale_price, lags = 1))
# b$ln_index <- b$ln_sale_price - b$ln_sale_price[1]
b$index <- b$net_sale_price/b$net_sale_price[1]

iplot <- ggplot(data=b, aes(x=sale_year, y=index)) + geom_line()
# lnplot <- ggplot(data=a, aes(x=sale_year, y=growth)) + geom_line()

View(aggregate(a[,c('ln_sale_price')],list(a$sale_year),length)[,2])