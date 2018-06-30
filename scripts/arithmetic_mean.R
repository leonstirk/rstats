source('scripts/das_data_preprocessing.R')

getArithmeticMeanIndexSeries <- function(data_subset, variable, period) {
  meanSeries <- aggregate(data_subset[, variable], list(data_subset[,period]), median)
  meanSeries <-  data.frame(meanSeries)
  names(meanSeries) <- c(period,variable)
  index <- (meanSeries[,variable] - meanSeries[1,variable])+1
  return(index)
}

meanIndexByAU <- c('year', levels(as.factor(das[,'sale_year'])))

for(au in au_names) {
 a <- subsetByVar(das, "area_unit_name", au)
 col <- c(au, getArithmeticMeanIndexSeries(a, "ln_sale_price", "sale_year"))
 meanIndexByAU <- cbind(meanIndexByAU, col)
}

meanIndexByAU <- data.frame(meanIndexByAU)

meanIndexByAU<-tail(meanIndexByAU, -1)
names(meanIndexByAU) <- c('year', au_names)

meanIndexByAU <- melt(meanIndexByAU, id.vars=c('year'))
meanIndexByAU$value <- as.numeric(meanIndexByAU$value)

iplot <- ggplot(data=meanIndexByAU, aes(x=year, y=value, color=variable)) + geom_line(aes(group = variable))

names(meanIndexByAU) <- c('year', 'area_unit_name', 'index')

# constructLevelIndex <- function(series) {
#  return(series/series[1])
# }

# constructLogIndex <- function(series) { 
#   return(series - series[1])
# }

# countByYear <- function() {
#  return(aggregate(a[,c('ln_sale_price')],list(a$sale_year),length)[,2]))
# }