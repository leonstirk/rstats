source('scripts/flooding_data_processing.R')

#############################################
## Create some subsets of dunedin all sales #
#############################################

flood_sub_F_NF <- subset(flood_sub, flood_prone == 1)
flood_sub_F_NFP <- subset(flood_sub, non_flood == 0)
ldf <- list(flood_sub_F_NF, flood_sub_F_NFP)

##############################
## Assign treatment variable #
##############################
## treatment == Forbury
ldf <- Map(cbind, ldf, treatment = lapply(ldf, function(df) { df$treatment <- ifelse(df$flood == 1,1,0) }))

ldf_t <- lapply(ldf, function(df) { list(subset(df, after_flood == 0), subset(df, after_flood == 1)) })

#######################################################
## Do matching on before_flood and after_flood groups #
#######################################################
l_m <- lapply(ldf_t, function(ldf) { lapply(ldf, function(df) { matchSamples(df) }) })

l_m_out <- lapply(l_m, function(area_diff) { lapply(area_diff, function(time_diff) { time_diff[[1]] }) })
l_m_data <- lapply(l_m, function(area_diff) { lapply(area_diff, function(time_diff) { time_diff[[2]] }) })
l_m_matches <- lapply(l_m, function(area_diff) { lapply(area_diff, function(time_diff) { time_diff[[3]] }) })

dndnd_sub <- rbind(l_m_data[[1]][[1]],l_m_data[[1]][[2]],l_m_data[[2]][[1]],l_m_data[[2]][[2]])

## Diff in diff in diff formula
dndnd_model_formula <- as.formula(paste("ln_sale_price ~ after_flood*flood_prone*flood + ",model_all))

## Post-matching parametric analysis (linear regression model)
l_a_data <- list(flood_sub, dndnd_sub)

l_a_fit <- lapply(l_a_data, function(data) { lm(dndnd_model_formula, data = data) })
l_a_fit_summary <- lapply(l_a_fit, summary)

l_a_fit_summary_clean <- lapply(l_a_fit_summary, function(fit) {clean_summary(fit$coefficients,4)})

####################
## Balance summary #
####################
l_bal_sum <- lapply(l_m_out, summary)
l_bal_plot <- lapply(l_m_out, plot)

l_bal_std_sum <- lapply(l_m_out, function(m_out) { summary(m_out, standardize=TRUE) })
l_bal_plot_sum <- lapply(l_m_out, function(m_out) { plot(summary(m_out, standardize=TRUE)) })

###################
## Zelig analysis #
###################
## z_out <- zelig(zelig_model_formula, model = "ls", data = m_data)
## c_out <- setx(z_out, treatment = 0)
## t_out <- setx(z_out, treatment = 1)
## s_out <- sim(z_out, c_out, t_out)

######################################
## Plot matched vs unmatched samples #
######################################

## raw_plot <- ggplot(flood_sub, aes(lon_gd2000_x, lat_gd2000_y, color = flood_analysis_group)) + geom_point() + coord_cartesian(xlim = c(170.445, 170.565),ylim = c(-45.915,-45.83))
## dndnd_plot <- ggplot(dndnd_sub, aes(lon_gd2000_x, lat_gd2000_y, color = flood_analysis_group)) + geom_point() + coord_cartesian(xlim = c(170.445, 170.565), ylim = c(-45.915,-45.83))

## comparison_plots <- lapply(l_a_data, function(data) { ggplot(data, aes(x=lon_gd2000_x, y=lat_gd2000_y)) + geom_point(aes(color = after_flood, shape = after_flood), position = "jitter") }) + coord_cartesian(xlim = c(170.4375,170.58), ylim = c(-45.825,-45.925))
