source('scripts/flooding_data_processing.R')

## Model
dnd_model_formula <- as.formula(paste("ln_sale_price ~ after_flood*flood_prone + ",model_all))

## Apply treatment
flood_sub$treatment <- flood_sub$flood_prone

## Subset on time
ldf_t <- list(subset(flood_sub, after_flood == 0), subset(flood_sub, after_flood == 1))

## Match
l_m <- lapply(ldf_t, function(df) { matchSamples(df) })

l_m_out <- lapply(l_m, function(l_m) { l_m[[1]] })
l_m_data <- lapply(l_m, function(l_m) { l_m[[2]] })
l_m_matches <- lapply(l_m, function(l_m) { l_m[[3]] })

dnd_sub <- rbind(l_m_data[[1]],l_m_data[[2]])


## Post-matching parametric analysis (linear regression model)
l_a_data <- list(flood_sub, dnd_sub)

l_a_fit <- lapply(l_a_data, function(data) { lm(dnd_model_formula, data = data) })
l_a_fit_summary <- lapply(l_a_fit, summary)

l_a_fit_summary_clean <- lapply(l_a_fit_summary, function(fit) {clean_summary(fit$coefficients,4)})


## Balance summaries
l_bal_sum <- lapply(l_m_out, summary)
## l_bal_plot <- lapply(l_m_out, plot)

l_bal_std_sum <- lapply(l_m_out, function(m_out) { summary(m_out, standardize=TRUE) })
## l_bal_plot_sum <- lapply(l_m_out, function(m_out) { plot(summary(m_out, standardize=TRUE)) }) 

dnd_regression_tables <- lapply(l_a_fit_summary_clean, function(reg) { xtable(reg, type = "latex") })

raw_plot <- ggplot(flood_sub, aes(lon_gd2000_x, lat_gd2000_y, color = flood_analysis_group)) + geom_point()
dnd_plot <- ggplot(dnd_sub, aes(lon_gd2000_x, lat_gd2000_y, color = flood_analysis_group)) + geom_point()
