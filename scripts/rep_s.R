
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

rs_mean_summary <- rs_ob %>% group_by(sale_quarter) %>% summarise(n_sale_quarter = n(), mean_ln_sale_price = mean(ln_sale_price), std_error = sd(ln_sale_price/n_sale_quarter))

ms_vars<-names(ms)[c(4,6:16)]

ms_means <- sapply(ms[ms_vars], mean, na.rm=TRUE)
ms_stdev <- sapply(ms[ms_vars], sd, na.rm=TRUE)

ms_ds <- data.frame(ms_means, ms_stdev)
ms_ds$ms_means <- prettyNum(ms_ds$ms_means, scientific=FALSE, big.mark=",")
ms_ds$ms_stdev <- prettyNum(ms_ds$ms_stdev, scientific=FALSE, big.mark=",")
View(ms_ds)

ms_mean_summary <- ms %>% group_by(sale_quarter) %>% summarise(n_sale_quarter = n(), mean_ln_sale_price = mean(ln_sale_price), std_error = sd(ln_sale_price/n_sale_quarter))
