7require(randomForest)
require(MASS) ## Package which contains the Boston housing dataset
attach(Boston)
set.seed(101)

dim(Boston)

## training Sample with 300 observations
train=sample(1:nrow(Boston),300)
?Boston  #to search on the dataset

Boston.rf=randomForest(medv ~ . , data = Boston , subset = train)
Boston.rf

plot(Boston.rf)

oob.err=double(13)
test.err=double(13)

## mtry is no of Variables randomly chosen at each split
for(mtry in 1:13)
{
    rf=randomForest(medv ~ . , data = Boston , subset = train,mtry=mtry,ntree=200)
    oob.err[mtry] = rf$mse[200] ## Error of all Trees fitted

    pred<-predict(rf,Boston[-train,]) ## Predictions on Test Set for each Tree
    test.err[mtry]= with(Boston[-train,], mean( (medv - pred)^2)) ## Mean Squared Test Error

    cat(mtry," ") ## printing the output to the console
}

test.err

oob.err

matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))





out <- do.call(mod, list(formula = formula, data = data, subset = obj$matched == TRUE & obj$groups == obj$g.names[1], ntree = ntree))

response <- all.vars(formula)[attr(terms(formula), "response")]
tmp.data <- data
tmp.data[, obj$treatment] <- 0
prd <- predict(out, tmp.data)
TEi <- data[, response] - prd
g <- function(i) {
    idt <- which(obj$mstrata == i & obj$groups == obj$g.names[2])
    mean(TEi[idt])
}
mstrata <- na.omit(unique(obj$mstrata))
TE <- sapply(mstrata, g)
names(TE) <- mstrata
ww <- table(obj$mstrata, obj$groups)[, 2]
w.coef <- weighted.mean(TE, ww, na.rm = TRUE)
idx <- which(is.na(TE))
if (length(idx) > 0)
    TE <- TE[-idx]
rf.cem <- matrix(NA, 4, 1)
v0 <- var(data[obj$matched == TRUE & obj$groups == obj$g.names[2], response])
v1 <- var(prd[obj$matched == TRUE & obj$groups == obj$g.names[2]])

dimnames(rf.cem) <- list(c("Estimate", "Std. Error", "t value", "p-value"), obj$treatment)
rf.cem["Estimate", ] <- w.coef
rf.cem["Std. Error", ] <- sqrt((v1 + v0) * sum(ww^2)/sum(ww)^2)
rf.cem["t value", ] <- rf.cem["Estimate", ]/rf.cem["Std. Error", ]
rf.cem["p-value", ] <- 2 * (1 - pnorm(rf.cem["t value", ]))
att.model <- rf.cem
out <- list(att.model = att.model, tab = obj$tab, treatment = obj$treatment, extrapolate = extrapolate, mod.type = mod.type, TE = TE)
class(out) <- "cem.att"
return(out)
