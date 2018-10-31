source('scripts/flooding_data_processing.R')

flood_sub_tainui_forbury <- subset(flood_sub, flooded == 1 | tainui == 1)
flood_sub_tainui_dudall <- subset(flood_sub, flooded == 0)
flood_sub_forbury_dudall <- subset(flood_sub, tainui == 0)

## flood_sub <- flood_sub_tainui_forbury
## flood_sub <- flood_sub_forbury_dudall
flood_sub <- flood_sub_tainui_dudall

before_flood <- subset(flood_sub, after_flood == 0)
after_flood <- subset(flood_sub, after_flood == 1)

ldf <- list(before_flood, after_flood)

##############################
## Assign treatment variable #
##############################

## Forbury - Tainui
## ldf <- Map(cbind, ldf, treatment = lapply(ldf, function(df) { df$treatment <- 1-df$flooded }))

## Forbury - Dunedin
## ldf <- Map(cbind, ldf, treatment = lapply(ldf, function(df) { df$treatment <- df$flooded }))

## Tainui - Dunedin
ldf <- Map(cbind, ldf, treatment = lapply(ldf, function(df) { df$treatment <- df$flood_prone }))

## Do matching on before_flood and after_flood groups #
l_m <- lapply(ldf, function(df) { matchSamples(df) })

l_m_out <- lapply(l_m, function(l_m) { l_m[[1]] })
l_m_data <- lapply(l_m, function(l_m) { l_m[[2]] })
l_m_matches <- lapply(l_m, function(l_m) { l_m[[2]] })

dnd_data <- rbind(l_m_data[[1]],l_m_data[[2]])

## dnd_model_formula <- as.formula(paste("ln_sale_price ~ after_flood*flooded+ ",model_all))
dnd_model_formula <- as.formula(paste("ln_sale_price ~ after_flood*tainui + ",model_all))

## Post-matching parametric analysis (linear regression model)
fit <- lm(dnd_model_formula, data=dnd_data)
raw_fit <- lm(dnd_model_formula, data=flood_sub)

lfit <- list(fit, raw_fit)
lfit <- lapply(lfit, summary)
lfit_clean <- lapply(lfit, function(fit) {clean_summary(fit$coefficients,4)})

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

    
