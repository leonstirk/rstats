source('functions/grid.R')

aggGrid <- function(data) {

    agg <- as.data.frame(t(matrix(unlist(
        by(data, data$cell, function(x) { c(lapply(x[,-which(names(x) == 'cantor')], mean), as.numeric(as.character(x$cantor[1]))) })
    ), nrow = ncol(data))))

    names(agg) <- names(data)

    return(agg)
}


test <- gridData(flood_data_subsets[["IF"]], 'lon_gd2000_x', 'lat_gd2000_y', 20)

test <- test[which(test$sale_date > as.Date('01-01-2017')),]

boot <- function(data) {
    return(test[sample(1:nrow(data), floor(nrow(data)/3)),])
}

train <- aggGrid(test)

## train    <- boot(test)
## ## test     <- boot(test)
## validate <- boot(test)

urf <- randomForest(x = train[,c('ln_sale_price', 'homeowner_rate', 'median_income', 'building_floor_area', 'land_area', 'dist_cbd')], mtry = 2, ntree = 2000, proximity = TRUE)
print(urf)

prox <- urf$proximity
pam.rf <- pam(prox, 3)
## pred <- cbind(pam.rf$clustering, iris$Species)
## table(pred[,2], pred[,1])

Clusters <- as.factor(pam.rf$cluster)
## Species <- iris$Species
## ggplot(iris, aes(x = Petal.Length, y = Petal.Width, col = Clusters, pch = Species)) + geom_point(size = 3) + scale_color_manual(values = myColRamp(3))

test['genome'] <- as.numeric(mapvalues(test[,'cantor'], levels(test[,'cantor']), as.numeric(as.character(Clusters))))

plot <- plotGrid(test, 'genome')

## data(iris)
## set.seed(3984)

## ## set colours
## myColRamp <- colorRampPalette(colors = c("#25591f", "#818c3c", "#72601b"))

## ## split
## holdout <- sample(1:150, 50)

## ## random forest model
## srf <- randomForest(x = iris[-holdout,-5], y = iris$Species[-holdout], mtry = 2, ntree = 2000, proximity = TRUE)
## srf

## ## predict on test set
## y_predicted <- predict(srf, iris[holdout,-5])
## df1 <- data.frame(Orig=iris$Species[holdout], Pred=y_predicted)


