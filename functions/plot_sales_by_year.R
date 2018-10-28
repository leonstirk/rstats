n_time_das <- aggregate(ln_sale_price ~ sale_year, data = das, FUN = length)
n_time_og_das <- aggregate(net_sale_price ~ sale_year, data = og_das, FUN = length)
names(n_time_das) <- c('sale_year','n')
names(n_time_og_das) <- c('sale_year','n')

n_time_merge <- merge.data.frame(n_time_das, n_time_og_das, by = 'sale_year')
names(n_time_merge) <- c('sale_year','n_das','n_og_das')

n_time_melt <- melt(n_time_merge, id=c('sale_year'))
n_time_melt <- within(n_time_melt, variable <- relevel(variable, ref = 2))

plot_sales_by_year <- ggplot(data=n_time_melt, aes(x=sale_year, y=value)) + geom_bar(stat="identity", position="dodge", aes(color=variable, fill=variable))
