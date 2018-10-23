source('scripts/flooding_data_processing.R')

before_flood <- subset(flood_sub, after_flood == 0)
after_flood <- subset(flood_sub, after_flood == 1)

ldf <- list(before_flood, after_flood)

ldf <- Map(cbind, ldf, treatment = lapply(ldf, function(df) { df$treatment <- 1-df$flooded }))
# ldf <- Map(cbind, ldf, treatment = lapply(ldf, function(df) { df$treatment <- df$flood_prone })

matchSamples <- function(das_vars, a_sub) {

###################################################
## Define covariate space for pre-matched samples #
###################################################
a_sub_cov <- tail(das_vars,-1)

###############################################################
## Omit any observations with missing values in the variables #
###############################################################
a_sub_nomiss <- a_sub %>% dplyr::select(qpid, sale_id, sale_year, ln_sale_price, treatment, after_flood, flooded, one_of(a_sub_cov)) %>% na.omit()

########################
# Create model strings #
########################

model_lhs_vars <- paste(a_sub_cov, collapse = " + ")
model_formula <- as.formula(paste("treatment ~ ",model_lhs_vars))
zelig_model_formula <- as.formula(paste("ln_sale_price ~ treatment + ",model_lhs_vars))

################################
## Nearest match (mahalanobis) #
################################ 
m_out <- matchit(model_formula, distance = "mahalanobis", method = "nearest", data = a_sub_nomiss)

#####################
## Matched data set #
#####################
m_data <- match.data(m_out)

####################
## Balance summary #
####################
# b_sum <- summary(m_out)
# b_sum_std <- summary(m_out, standardize = TRUE)
# b_plot <- plot(m_out)
# b_plot_sum <- plot(b_sum_std)

###################
## Zelig analysis #
###################
# z_out <- zelig(zelig_model_formula, model = "ls", data = m_data)
# c_out <- setx(z_out, treatment = 0)
# t_out <- setx(z_out, treatment = 1)
# s_out <- sim(z_out, c_out, t_out)

return(m_data)

}

lapply(ldf, function(df) { matchSamples(das_vars, df) })


# diff_in_diff_data <- rbind(before_flood_m, after_flood_m)
# diff_in_diff_data$flooded <- 1-diff_in_diff_data$treatment

# diff_in_diff_data$flood_prone <- diff_in_diff_data$treatment

# diff_in_diff_model_formula <- as.formula(paste("ln_sale_price ~ after + flooded + after*flooded + ",model_lhs_vars))
# diff_in_diff_model_formula <- as.formula(paste("ln_sale_price ~ after + flood_prone + after*flood_prone + ",model_lhs_vars))

# fit <- lm(diff_in_diff_model_formula, data=diff_in_diff_data)
# summary(fit)