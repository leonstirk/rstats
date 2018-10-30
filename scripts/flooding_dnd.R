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
l_m_matches <- lapply(l_m, function(l_m) { l_m[[2]] })

dnd_sub <- rbind(l_m_data[[1]],l_m_data[[2]])

## Post-matching parametric analysis (linear regression model)

## Stadard regression model
raw_fit <- lm(dnd_model_formula, data=flood_sub)

## Matched sample regression model
fit <- lm(dnd_model_formula, data=dnd_sub)

lfit <- list(fit, raw_fit)
lfit <- lapply(lfit, summary)
lfit_clean <- lapply(lfit, function(fit) {clean_summary(fit$coefficients,4)})

dnd_regression_tables <- lapply(lfit_clean, function(reg) { xtable(reg, type = "latex") })
