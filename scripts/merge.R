#! /usr/bin/env Rscript

df <- do.call(rbind.fill, lapply(list.files(path = "./csv/", full.names = TRUE), read.csv))
write.csv(x = df, file = "dccRates.csv")

# names(df)[names(df) == 'old.var.name'] <- 'new.var.name'

