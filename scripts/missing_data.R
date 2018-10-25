load("datasets/dud_allsales_2000to2018.Rda")

a <- lapply(das, anyNA)
contains_NA <- a[which(a==TRUE)]

b <- lapply(das, is.na)
count_NA <- lapply(b, function(b){table(b)["TRUE"]})
count_NA <- count_NA[names(count_NA) %in% names(contains_NA)]
count_NA <- unlist(count_NA)
