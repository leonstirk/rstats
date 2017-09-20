#! /usr/bin/env Rscript

library('plyr')

i <- 3160
while ( i < 3170 ) {

df <- read.csv(sprintf("/home/ubuntu/rstats/dccRates%s.csv", i))

i=i+1

}

