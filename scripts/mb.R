#!/usr/bin/env Rscript

rm(list = ls())

library('tidyr')
library('reshape2')

# load("/home/ubuntu/rstats/datasets/blocks.R")

a<-read.csv("/home/ubuntu/rstats/datasets/2013-mb-dataset-Total-New-Zealand-Individual-Part-3b.csv")

mb<-a[1:46629,]
# au<-a[46631:48642,]
# wd<-a[48644:48901,]
# ta<-a[48903:48970,]
# rn<-a[48972:48988,]
# lba<-a[48990:49010,]
tmp<-colsplit(mb$Area_Code_and_Description," ",c("text","code"))
mb$mb<-tmp$code

rm(tmp)

drops <- c("Area_Code_and_Description","Code","Description")
mb<-mb[ , !(names(mb) %in% drops)]

mb_cols<-ncol(mb)

mb_long<-gather(mb, key, value, names(mb[1:mb_cols-1]))

tmp<-colsplit(mb_long$key, "_", names=c("year_a", "text"))
tmp$year_b<-substring(tmp$year_a,2,nchar(tmp$year_a))
tmp$year<-as.numeric(tmp$year_b)
tmp$factor<-as.factor(tmp$text)

mb_long<-cbind(mb_long,tmp)

rm(tmp)

drops<-c("year_a","year_b","text", "key")
mb_long<-mb_long[,!(names(mb_long) %in% drops)]

names(mb_long)<-c("mb","value","year","key")

mb_wide<-spread(mb_long, key, value)








# mb_long$key_char<-as.character(mb_long$key)
# tmp<-colsplit(mb_long$key_char, "_(?=[A-Z])", names=c("something", "something_else"))
# tmp$something<-as.factor(tmp$something)
# tmp$something_else<-as.factor(tmp$something_else)

# mb_long<-cbind(mb_long,tmp)

# rm(tmp)

# drops<-c("key_char","something","key")
# mb_long<-mb_long[,!(names(mb_long) %in% drops)]

# mb_wide<-spread(mb_long,something_else,value)

# rm(mb_long)

# Start for loop over groups
# ngroups<-length(levels(mb_long$something))
# i=1

# tmp<-mb_long[which(mb_long$something==levels(mb_long$something)[1]),]

# drops<-c("key_char","something","key")
# tmp<-tmp[,!(names(tmp) %in% drops)]

# tmp_wide<-spread(tmp,something_else,value)

# names<-names(tmp_wide)
# ncats<-length(levels(droplevels(tmp$something_else)))
# cats<-tail(names, ncats)
# cats


