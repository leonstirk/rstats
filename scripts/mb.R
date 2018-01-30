#!/usr/bin/env Rscript

library('tidyr')
library('reshape2')

a<-read.csv("/home/ubuntu/rstats/datasets/2013-mb-dataset-Total-New-Zealand-Dwelling.csv")

mb<-c[1:46629,]
au<-c[46631:48642,]
wd<-c[48644:48901,]
ta<-c[48903:48970,]
rn<-c[48972:48988,]
lba<-c[48990:49010,]

a_long<-gather(a, key, value, names(a[4:ncol(a)]))

b<-colsplit(a_long$key, "_", names=c("year_a", "text"))

b$year_b<-substring(b$year_a,2,nchar(b$year_a))
b$year<-as.numeric(b$year_b)
b$factor<-as.factor(b$text)

c<-cbind(a_long,b)

c[c==""]<-NA
c$confidential<-as.numeric(c$value=="..C")
c$value[c$value=="..C"]<-NA
c$value<-as.numeric(c$value)


key<-read.csv("/home/ubuntu/rstats/datasets/mbKey.csv")
centroid<-read.csv("/home/ubuntu/rstats/datasets/meshblock-2018-centroid-true.csv")


# merge(mb, mbKey, by = c("