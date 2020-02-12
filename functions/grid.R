## rm(list=ls())

source('scripts/baseline_analysis_prep.R')

cantorPair <- function(a, b) {
    out <- 0.5*(a+b)*(a+b+1)+b
    return(out)
}

ji <- function(xy, origin, cellsize) {
    t(apply(xy, 1, function(z) cellsize/2+origin+cellsize*(floor((z - origin)/cellsize))))
}

gridData <- function(data, x, y, res) {

    lon <- data[x]
    lat <- data[y]

    origin <- c(min(lon), min(lat))
    cellsize <- c(diff(range(lon))/res, diff(range(lat))/res)

    xy <- cbind(lon, lat)
    JI <- ji(xy, origin, cellsize)

    X <- JI[,1]
    Y <- JI[,2]
    cell <- paste(X, Y)

    dat <- cbind(data, X, Y, cell)

    f <- c('cell')
    n <- c('X', 'Y')
    dat[f] <- lapply(dat[f], as.factor)
    dat[n] <- lapply(dat[n], function(x) { as.numeric(as.character(x)) })

    dat$cantor <- as.factor(with(dat, cantorPair(X,Y)))

    return(dat)
}

plotGrid <- function(data, var) {
    counts <- as.data.frame(t(matrix(unlist(by(data, data$cantor, function(x) { c(x$X[1], x$Y[1], mean(as.numeric(as.character(x[,var])))) })), nrow = 3)))
    ## counts <<- as.data.frame(t(matrix(unlist(by(data, data$cantor, function(x) { c(x$X[1], x$Y[1], nrow(x) ) })), nrow = 3)))
    names(counts) <- c('X', 'Y', 'count')
    plot <- ggplot(counts, aes(X,Y)) + geom_tile(aes(fill = count)) + scale_fill_gradientn(colors = c('blue','purple','orange','yellow'))
    return(plot)
}

## plot <- plotGrid(gridData(flood_data_subsets[["IF"]], 'lon_gd2000_x', 'lat_gd2000_y', 20), 'ln_sale_price')
