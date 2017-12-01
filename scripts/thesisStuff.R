#!/usr/bin/env Rscript

# import STATA file
data <- read.dta13("/home/ubuntu/rstats/thesisData/QVCensusMergeModel.dta")

# lag clusters 2-4
data$avclus2_1 <- c(0, data$avclus2[1:nrow(data)-1]) # etc
data$avclus3_1 <- c(0, data$avclus3[1:nrow(data)-1])
data$avclus4_1 <- c(0, data$avclus4[1:nrow(data)-1])

# clean the first observation of each laged variable
data$avclus2_1[1] <- NA # etc
data$avclus3_1[1] <- NA
data$avclus4_1[1] <- NA


# run the ARDL model
fit <- lm(data$avclus1 ~ data$avclus2 + data$avclus2_1) # etc
sum <- summary(fit)
fstat <- sum$fstatistic

# windows

# n = number of windows
# T = number of years/observations
# W = window size

# n = T-(W-1)

# W = 7, n = 16-(7-1) = 10

w <- 10
t <- nrow(data)
N <- t-(w-1)
n <- c(1:N)

for(i in n) {

 # c(i:(i+w-1))

 fit <- lm(avclus2[i:(i+w-1)] ~ avclus3[i:(i+w-1)] + avclus3_1[i:(i+w-1)], data = data) # etc
 sum <- summary(fit)
 fstat <- sum$fstatistic
 print(paste(sum$coefficients[2,c(1,3)]))
 print(paste(fstat[1]))
}