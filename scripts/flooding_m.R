source('scripts/flooding_data_processing.R')

flood_sub_tainui_forbury <- subset(flood_sub, flooded == 1 | tainui == 1)
flood_sub_tainui_dudall <- subset(flood_sub, flooded == 0)
flood_sub_forbury_dudall <- subset(flood_sub, tainui == 0)

flood_sub <- flood_sub_tainui_forbury
## flood_sub <- flood_sub_forbury_dudall
## flood_sub <- flood_sub_tainui_dudall

before_flood <- subset(flood_sub, after_flood == 0)
after_flood <- subset(flood_sub, after_flood == 1)

ldf <- list(before_flood, after_flood)

##############################
## Assign treatment variable #
##############################

## Forbury - Tainui
ldf <- Map(cbind, ldf, treatment = lapply(ldf, function(df) { df$treatment <- df$flooded }))

## Forbury - Dunedin
## ldf <- Map(cbind, ldf, treatment = lapply(ldf, function(df) { df$treatment <- df$flooded }))

## Tainui - Dunedin
## ldf <- Map(cbind, ldf, treatment = lapply(ldf, function(df) { df$treatment <- df$tainui }))

## Do matching on before_flood and after_flood groups #
l_m <- lapply(ldf, function(df) { matchSamples(df) })

l_m_out <- lapply(l_m, function(l_m) { l_m[[1]] })
l_m_data <- lapply(l_m, function(l_m) { l_m[[2]] })
l_m_matches <- lapply(l_m, function(l_m) { l_m[[3]] })

dnd_sub <- rbind(l_m_data[[1]],l_m_data[[2]])

dnd_model_formula <- as.formula(paste("ln_sale_price ~ after_flood*flooded+ ",model_all))
## dnd_model_formula <- as.formula(paste("ln_sale_price ~ after_flood*tainui + ",model_all))


## Post-matching parametric analysis (linear regression model)
l_a_data <- list(flood_sub, dnd_sub)

l_a_fit <- lapply(l_a_data, function(data) { lm(dnd_model_formula, data = data) })
l_a_fit_summary <- lapply(l_a_fit, summary)

l_a_fit_summary_clean <- lapply(l_a_fit_summary, function(fit) {clean_summary(fit$coefficients,4)})


####################
## Balance summary #
####################
l_bal_sum <- lapply(l_m_out, summary)
## l_bal_plot <- lapply(l_m_out, plot)
## l_bal_plot_sum <- lapply(l_m_out, function(m_out) { plot(summary(m_out, standardize=TRUE)) })

###################
## Zelig analysis #
###################
## z_out <- zelig(zelig_model_formula, model = "ls", data = m_data)
## c_out <- setx(z_out, treatment = 0)
## t_out <- setx(z_out, treatment = 1)
## s_out <- sim(z_out, c_out, t_out)

    
