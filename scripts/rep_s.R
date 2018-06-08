
rs_p <- repsaledata(das$net_sale_price, das$sale_date, das$QPID)
names(rs_p)[1] <- "QPID"

rs_id <- unique(rs_p$QPID)

rs_ob <- subset(das, (QPID %in% rs_id))

uq_id <- unique(das$QPID)

ss_id <- setdiff(uq_id, rs_id)


rs_means <- sapply(rs_ob[das_vars], mean, na.rm=TRUE)
rs_stdev <- sapply(rs_ob[das_vars], sd, na.rm=TRUE)

rs_ds <- data.frame(rs_means, rs_stdev)
rs_ds$rs_means <- prettyNum(rs_ds$rs_means, scientific=FALSE, big.mark=",")
rs_ds$rs_stdev <- prettyNum(rs_ds$rs_stdev, scientific=FALSE, big.mark=",")
View(rs_ds)

