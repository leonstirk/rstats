source('scripts/das_data_preprocessing.R')

getArithmeticMeanIndexSeries <- function(data_subset, variable, period) {
  meanSeries <- aggregate(data_subset[, variable], list(data_subset[,period]), mean)
  meanSeries <-  data.frame(meanSeries)
  names(meanSeries) <- c(period,variable)
  index <- (meanSeries[,variable] - meanSeries[1,variable])+1
  return(index)
}

meanIndicesByAU <- c('year', levels(as.factor(das[,'sale_year'])))

for(au in au_ids) {
 a <- subsetByVar(das, "area_unit_id", au)
 col <- c(au, getArithmeticMeanIndexSeries(a, "ln_sale_price", "sale_year"))
 meanIndicesByAU <- cbind(meanIndicesByAU, col)
}

meanIndicesByAU <- data.frame(meanIndicesByAU)

meanIndicesByAU<-tail(meanIndicesByAU, -1)
names(meanIndicesByAU) <- c('year', au_ids)

# constructLevelIndex <- function(series) {
#  return(series/series[1])
# }

constructLogIndex <- function(series) { 
  return(series - series[1])
}

# a <- das_caversham

# mean <- aggregate(a[, c('ln_sale_price', 'net_sale_price', 'ln_building_floor_area')], list(a$sale_year), mean)
# names(mean)[1] <- 'sale_year'

# median <- aggregate(a[, c('ln_sale_price', 'net_sale_price', 'ln_building_floor_area')], list(a$sale_year), median)
# names(median)[1] <- 'sale_year'

# b <- mean

# b$ln_period_change <- c(NA,diff(b$ln_sale_price, lags = 1))
# b$ln_index <- b$ln_sale_price - b$ln_sale_price[1]
# b$index <- b$net_sale_price/b$net_sale_price[1]

# iplot <- ggplot(data=b, aes(x=sale_year, y=index)) + geom_line()
# lnplot <- ggplot(data=a, aes(x=sale_year, y=growth)) + geom_line()

# countByYear <- function() {
#  return(aggregate(a[,c('ln_sale_price')],list(a$sale_year),length)[,2]))
# }