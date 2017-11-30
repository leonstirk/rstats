#!/usr/bin/env Rscript

# import STATA file
model <- read.dta13("/home/ubuntu/rstats/thesisData/QVCensusMergeModel.dta")

# lag clusters 2-4
model$avclus2_1 <- c(0, model$avclus2[1:nrow(model)-1]) # etc

# clean the first observation of each laged variable
model$avclus2_1[1] <- NA # etc

# run the ARDL model
fit <- lm(model$avclus1 ~ model$avclus2 + model$avclus2_1) # etc
sum <- summary(fit)
fstat <- sum$fstatistic