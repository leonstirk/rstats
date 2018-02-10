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

rm(tmp)

drops <- c("Area_Code_and_Description","Code","Description")
mb<-mb[ , !(names(mb) %in% drops)]

mb_cols<-ncol(mb)

key<-read.csv("/home/ubuntu/rstats/datasets/mbKey.csv")
tmp<-colsplit(key$Meshblock..2013.areas.," ",c("text","code"))
key$mb<-tmp$code
drops<- c("Meshblock..2013.areas.")
key<-key[,!(names(key) %in% drops)]

rm(tmp)

centroid<-read.csv("/home/ubuntu/rstats/datasets/mb2013Centroid.csv")
centroid$mb<-centroid$MB2013_V1_00
drops<-c("MB2013_V1_00")
centroid<-centroid[,!(names(centroid) %in% drops)]

mb<-merge(mb,key,by=c("mb"))
mb<-merge(mb,centroid,by=c("mb"))

mb_long<-gather(mb, key, value, names(mb[2:mb_cols]))

tmp<-colsplit(mb_long$key, "_", names=c("year_a", "text"))
tmp$year_b<-substring(tmp$year_a,2,nchar(tmp$year_a))
tmp$year<-as.numeric(tmp$year_b)
tmp$factor<-as.factor(tmp$text)

mb_long<-cbind(mb_long,tmp)

rm(tmp)

mb_long$confidential<-as.numeric(mb_long$value=="..C")
mb_long$value[mb_long$value=="..C"]<-NA
mb_long$value<-as.numeric(mb_long$value)
drops<-c("year_a","year_b","text", "OBJECTID", "key")
mb_long<-mb_long[,!(names(mb_long) %in% drops)]

names<-c("mbCode2013","auCode2013","auName2013","wdCode2013","wdName2013","taCode2013","taName2013","rcCode2013","rcName2013","landTypeCode2013","landTypeName2013","areaTotalSqKm","areaLandSqKm","NZTM2000Easting","NZTM2000Northing","WGS84Lat","WGS84Lon","value","year","key","confidential")

names(mb_long)<-names

mb_long$key_char<-as.character(mb_long$key)
tmp<-colsplit(mb_long$key_char, "_(?=[A-Z])", names=c("something", "something_else"))
tmp$something<-as.factor(tmp$something)
tmp$something_else<-as.factor(tmp$something_else)

mb_long<-cbind(mb_long,tmp)

rm(tmp)

# Start for loop over groups
# ngroups<-length(levels(mb_long$something))
# i=1

tmp<-mb_long[which(mb_long$something==levels(mb_long$something)[1]),]

drops<-c("key_char","something","key")
tmp<-tmp[,!(names(tmp) %in% drops)]

tmp_wide<-spread(tmp,something_else,value)

names<-names(tmp_wide)
ncats<-length(levels(droplevels(tmp$something_else)))
cats<-tail(names, ncats)
cats