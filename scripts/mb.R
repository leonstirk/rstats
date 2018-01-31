#!/usr/bin/env Rscript

library('tidyr')
library('reshape2')

a<-read.csv("/home/ubuntu/rstats/datasets/2013-mb-dataset-Total-New-Zealand-Dwelling.csv")

mb<-a[1:46629,]
# au<-a[46631:48642,]
# wd<-a[48644:48901,]
# ta<-a[48903:48970,]
# rn<-a[48972:48988,]
# lba<-a[48990:49010,]
tmp<-colsplit(mb$Area_Code_and_Description," ",c("text","code"))
mb$mb<-tmp$code

drops <- c("Area_Code_and_Description","Code","Description")
mb<-mb[ , !(names(mb) %in% drops)]

mb_cols<-ncol(mb)

key<-read.csv("/home/ubuntu/rstats/datasets/mbKey.csv")
tmp<-colsplit(key$Meshblock..2013.areas.," ",c("text","code"))
key$mb<-tmp$code
drops<- c("Meshblock..2013.areas.")
key<-key[,!(names(key) %in% drops)]

centroid<-read.csv("/home/ubuntu/rstats/datasets/mb2013Centroid.csv")
centroid$mb<-centroid$MB2013_V1_00
drops<-c("MB2013_V1_00")
centroid<-centroid[,!(names(centroid) %in% drops)]

merge1<-merge(mb,key,by=c("mb"))
merge2<-merge(merge1,centroid,by=c("mb"))

mb_long<-gather(merge2, key, value, names(merge2[2:mb_cols]))

b<-colsplit(mb_long$key, "_", names=c("year_a", "text"))
b$year_b<-substring(b$year_a,2,nchar(b$year_a))
b$year<-as.numeric(b$year_b)
b$factor<-as.factor(b$text)

c<-cbind(mb_long,b)

c$confidential<-as.numeric(c$value=="..C")
c$value[c$value=="..C"]<-NA
c$value<-as.numeric(c$value)
drops<-c("year_a","year_b","text", "OBJECTID", "key")
c<-c[,!(names(c) %in% drops)]

names<-c("mbCode2013","auCode2013","auName2013","wdCode2013","wdName2013","taCode2013","taName2013","rcCode2013","rcName2013","landTypeCode2013","landTypeName2013","areaTotalSqKm","areaLandSqKm","NZTM2000Easting","NZTM2000Northing","WGS84Lat","WGS84Lon","value","year","key","confidential")

names(c)<-names

# c[c==""]<-NA

mb_wide<-spread(c,key,value)