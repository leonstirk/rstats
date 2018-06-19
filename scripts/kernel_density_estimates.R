library(sm)

a <- das_caversham

n <- length(levels(as.factor(a$sale_year)))
colours <- rainbow(n, alpha = 1)
linetype <- rep(1, n)


# sm.density.compare(a[which(a$sale_year %in% c('2000', '2015')),]$ln_sale_price, a[which(a$sale_year %in% c('2000', '2015')),]$sale_year)

sm.density.compare(a$ln_sale_price, a$sale_year, col = colours, lty = linetype)