library(sm)

source('scripts/das_data_preprocessing.R')

a <- das_caversham

n <- length(levels(as.factor(a$sale_year)))
colours <- rainbow(n, alpha = 1)
linetype <- rep(1, n)
xlabs <- das_vars[c(1,2,3,4,5,6,8,9,10,11)]
# xlabs <- das_vars[c(1)]
dates <- c('2002', '2008')

layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12),3,4)) # optional 4 graphs/page

for(xlab in xlabs) {

  sm.density.compare(a[which(a$sale_year %in% dates),][c(xlab)][,1], a[which(a$sale_year %in% dates),][c('sale_year')][,1], xlab = xlab)

  # sm.density.compare(a$ln_sale_price, a$sale_year, col = colours, lty = linetype)

}