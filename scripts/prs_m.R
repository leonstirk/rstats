library(MatchIt)
library(dplyr)
library(ggplot2)

setwd("/home/ubuntu/rstats")

load("datasets/dud_allsales_2000to2015.Rda")

das<-dud_allsales

rm(dud_allsales)

group_mean_summary <- das %>% group_by(sale_quarter) %>% summarise(n_sale_quarter = n(), mean_ln_sale_price = mean(ln_sale_price), std_error = sd(ln_sale_price/n_sale_quarter))

t <- as.numeric(levels(as.factor(das$sale_quarter)))
ti <- t[1]

das_sub <- subset(das, das$sale_quarter == ti | das$sale_quarter == ti+1)

t_test_output <- with(das_sub, t.test(ln_sale_price ~ sale_quarter))