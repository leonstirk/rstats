source('scripts/flooding_data_processing.R')

## mean_summary <- a %>% group_by(sale_year) %>% dplyr::summarise(n_sale_year = n(), mean_ln_sale_price = mean(ln_sale_price), std_error = sd(ln_sale_price)/sqrt(n_sale_year))

tmp <- t(das[c(das_vars,"flood_analysis_group")] %>% group_by(flood_analysis_group) %>% summarise_all(funs(mean)))
