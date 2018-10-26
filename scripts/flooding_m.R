source('scripts/flooding_data_processing.R')

flood_sub_tainui_forbury <- subset(flood_sub, flooded == 1 | flood_prone == 1)
flood_sub_tainui_dudall <- subset(flood_sub, flooded == 0)
flood_sub_forbury_dudall <- subset(flood_sub, flood_prone == 0)

flood_sub <- flood_sub_tainui_forbury

before_flood <- subset(flood_sub, after_flood == 0)
after_flood <- subset(flood_sub, after_flood == 1)

ldf <- list(before_flood, after_flood)

## Assign treatment variable

## Forbury - Tainui
ldf <- Map(cbind, ldf, treatment = lapply(ldf, function(df) { df$treatment <- 1-df$flooded }))

## Tainui - Dunedin
## ldf <- Map(cbind, ldf, treatment = lapply(ldf, function(df) { df$treatment <- df$flood_prone }))

## Forbury - Dunedin
## ldf <- Map(cbind, ldf, treatment = lapply(ldf, function(df) { df$treatment <- df$flooded }))

## Do matching on before_flood and after_flood groups #
ldf_m_out <- lapply(ldf, function(df) { matchSamples(das_vars, df) })

break

## Matched sample data
ldf_m_data <- lapply(ldf_m_out, match.data)

## Matched sample matches
ldf_m_matches <- lapply(ldf_m_out, function(m_out) { na.omit(data.frame(match.data(m_out)[m_out$match.matrix,das_vars],match.data(m_out)[rownames(m_out$match.matrix),das_vars])) })

dnd_data <- rbind(ldf_m_data[[1]],ldf_m_data[[2]])

dnd_model_formula <- as.formula(paste("ln_sale_price ~ after_flood + flooded + after_flood*flooded + ",model_lhs_vars))
# dnd_model_formula <- as.formula(paste("ln_sale_price ~ after_flood + flood_prone + after_flood*flood_prone + ",model_lhs_vars))

# dnd_model_formula <- as.formula(paste("ln_sale_price ~ after_flood + flooded + after_flood*flooded"))
# dnd_model_formula <- as.formula(paste("ln_sale_price ~ after_flood + flood_prone + after_flood*flood_prone"))

## Post-matching parametric analysis (linear regression model)
fit <- lm(dnd_model_formula, data=dnd_data)
raw_fit <- lm(dnd_model_formula, data=flood_sub)

lfit <- list(fit, raw_fit)
lfit <- lapply(lfit, summary)
lfit_clean <- lapply(lfit, function(fit) {clean_summary(fit$coefficients,4)})

####################
## Balance summary #
####################
l_bal_sum <- lapply(ldf_m_out, summary)
## l_bal_plot <- lapply(ldf_m_out, plot)
## l_bal_plot_sum <- lapply(ldf_m_out, function(m_out) { plot(summary(m_out, standardize=TRUE)) })

###################
## Zelig analysis #
###################
## z_out <- zelig(zelig_model_formula, model = "ls", data = m_data)
## c_out <- setx(z_out, treatment = 0)
## t_out <- setx(z_out, treatment = 1)
## s_out <- sim(z_out, c_out, t_out)
