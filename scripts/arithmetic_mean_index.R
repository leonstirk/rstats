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

exampleTable <- meanIndexByAU[which(meanIndexByAU$area_unit_name %in% c('Opoho', 'Forrester Park', 'North East Valley', 'Pine Hill')),]
exampleTable <- exampleTable[which(exampleTable$year %in% c(2000:2005)),]
exampleTable$index <- exampleTable$index*100


explot <- ggplot(data=exampleTable, aes(x=year, y=index, color=area_unit_name)) + geom_line(aes(group = area_unit_name))


exampleTable$index <- format(exampleTable$index, digits=1, nsmall=1)

exampleTable <- t(dcast(exampleTable, year ~ area_unit_name, value.var="index"))

names <- exampleTable[1,]
exampleTable <- data.frame(tail(exampleTable, -1))
names(exampleTable) <- names
rm(names)