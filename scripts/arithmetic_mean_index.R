source('scripts/das_data_preprocessing.R')

meanIndexByAU <- c('year', levels(as.factor(das[,'sale_year'])))

for(au in au_names) {
 a <- subsetByVar(das, "area_unit_name", au)
 col <- c(au, getArithmeticMeanIndexSeries(a, "ln_sale_price", "sale_year"))
 meanIndexByAU <- cbind(meanIndexByAU, col)
}

meanIndexByAU <- data.frame(meanIndexByAU) %>% tail(-1)
names(meanIndexByAU) <- c('year', au_names)
meanIndexByAU <- melt(meanIndexByAU, id.vars=c('year'))
meanIndexByAU$value <- as.numeric(meanIndexByAU$value) %>% "*"(100) %>% format(digits=1, nsmall=1)

iplot <- ggplot(data=meanIndexByAU, aes(x=year, y=value, color=variable)) + geom_line(aes(group = variable))

names(meanIndexByAU) <- c('year', 'area_unit_name', 'index')

nByAuYear <- c('year', levels(as.factor(das[,'sale_year'])))

for(au in au_names) {
  a <- subsetByVar(das, "area_unit_name", au)
  col <- c(au, getSampleSize(a, "sale_year"))
  nByAuYear <- cbind(nByAuYear, col)
}

nByAuYear <- data.frame(nByAuYear) %>% tail(-1)
names(nByAuYear) <- c('year', au_names)
nByAuYear <- melt(nByAuYear, id.vars=c('year'))
nByAuYear$value <- as.numeric(nByAuYear$value)

areaUnitNameSubsetVector <- c('Opoho', 'Forrester Park', 'North East Valley', 'Pine Hill')
salePeriodSubsetVector <- c(2000:2005)

getExampleTable <- function(data, area_unit_subset_vector, sale_period_subset_vector) {
  
  return()
}

exampleTable <- meanIndexByAU[which(meanIndexByAU$area_unit_name %in% areaUnitNameSubsetVector ),]
exampleTable <- exampleTable[which(exampleTable$year %in% salePeriodSubsetVector),]

explot <- ggplot(data=exampleTable, aes(x=year, y=index, color=area_unit_name)) + geom_line(aes(group = area_unit_name))

exampleTable <- t(dcast(exampleTable, year ~ area_unit_name, value.var="index"))

names <- exampleTable[1,]
exampleTable <- data.frame(tail(exampleTable, -1))
names(exampleTable) <- names
rm(names)

