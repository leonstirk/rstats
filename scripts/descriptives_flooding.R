source('scripts/flooding_data_processing.R')

## mean_summary <- a %>% group_by(sale_year) %>% dplyr::summarise(n_sale_year = n(), mean_ln_sale_price = mean(ln_sale_price), std_error = sd(ln_sale_price)/sqrt(n_sale_year))



das[c(das_vars,"flooded","flood_prone","tainui")] %>% summarise_all(funs(mean))
