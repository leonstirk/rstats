#!/usr/bin/env Rscript

tmp<-mb_long[which(mb_long$something==levels(mb_long$something)[5]),]

drops<-c("key_char","something","key")
tmp<-tmp[,!(names(tmp) %in% drops)]

tmp_wide<-spread(tmp,something_else,value)

names<-names(tmp_wide)
ncats<-length(levels(droplevels(tmp$something_else)))
cats<-tail(names, ncats)

