source('scripts/newD.R')

dev.off()

strings[['model']] <- "DiD"
do.call(setStrings, strings)

hello <- function(data, M = 101, R = 100) {
    breaks <- setBreaks(data)
    data <- data[data_vars]
    rawL1 <- L1.profile(data[,treat_var], data[match_vars], grouping = breaks$grouping, M = M)

    ## L1_breaks <- list('cutpoints' = rawL1$medianCP, 'grouping' = rawL1$medianGR)
    ## cem_space <- cemspace(treat_var, data[,c(treat_var, match_vars)], R = R, grouping = breaks$grouping, L1.breaks = L1_breaks$cutpoints, L1.grouping = L1_breaks$grouping, plot = FALSE)

    cem_space <- cemspace(treat_var, data[,c(treat_var, match_vars)], R = R, grouping = breaks$grouping, raw.profile = rawL1, keep.weights = TRUE, plot = FALSE)

    return(cem_space)
}

## test <- hello(flood_data_subsets[["IF"]], 21, 20)
test <- hello(flood_data_subsets[["IF"]])
## hi <- 1/sqrt(test$space$G0)
## plot(test$space$ML1 ~ hi, xaxt = "n")
## axis(1, at=quantile(hi),labels=quantile(hi))

goodbye <- function(test) {
    data <- test$space
    ord <- data[order(data$ML1),]
    front_i <<- which(ord$G0 == cummax(ord$G0))
    front <- ord[front_i,]
    plot(data$ML1 ~ data$G0, xlim = rev(range(data$G0)))
    points(front$ML1 ~ front$G0, col = 'blue')
    lines(front$ML1 ~ front$G0, col = 'blue')
}

goodbye(test)

frontier_weights <- head(test$weights[front_i], -1)

fuck <- list()
models <- list()

for(i in 1:length(frontier_weights)) {
    something <- summary(lm(lm_model_formula, data = cbind(flood_data_subsets[["IF"]][c(data_vars)], "w" = frontier_weights[[i]]), weights = w))
    models[[i]] <- something
    fuck[[i]] <- c(something$coefficients[c('flood_prone1', 'after_flood1', 'flood_prone1:after_flood1'),'Estimate'], "L1" = test$space$ML1[i], "G0" = test$space$G0[i], "G1" = test$space$G1[i], 'matched' = TRUE)
}

something <- summary(lm(lm_model_formula, data = cbind(flood_data_subsets[["IF"]][c(data_vars)])))
something <- c(something$coefficients[c('flood_prone1', 'after_flood1', 'flood_prone1:after_flood1'),'Estimate'], "L1" = tail(test$space$ML1, 1), "G0" = tail(test$space$G0, 1), "G1" = tail(test$space$G1, 1), 'matched' = FALSE)

some <- as.data.frame(do.call(rbind, fuck))
some <- rbind(some, something)
names(some)[3] <- 'int'
names(something)[3] <- 'int'

par(mfrow = c(1,3))
with(some, plot(L1 ~ int))
abline(v = something['int'])
with(some, plot(L1 ~ flood_prone1))
abline(v = something['flood_prone1'])
with(some, plot(L1 ~ after_flood1))
abline(v = something['after_flood1'])
