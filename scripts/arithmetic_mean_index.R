source('scripts/das_data_preprocessing.R')

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

# nSalesByPeriod <- function(data, period) {
#  return(aggregate(data[,c('ln_sale_price')],list(data$sale_year),length)[,2]))
# }