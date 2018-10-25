source('scripts/flooding_data_processing.R')

flood_sub_tainui_forbury <- subset(flood_sub, flooded == 1 | flood_prone == 1)
flood_sub_tainui_dudall <- subset(flood_sub, flooded == 0)

flood_sub <- flood_sub_tainui_forbury

before_flood <- subset(flood_sub, after_flood == 0)
after_flood <- subset(flood_sub, after_flood == 1)

ldf <- list(before_flood, after_flood)

ldf <- Map(cbind, ldf, treatment = lapply(ldf, function(df) { df$treatment <- 1-df$flooded }))
## ldf <- Map(cbind, ldf, treatment = lapply(ldf, function(df) { df$treatment <- df$flood_prone })

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

  ###############################
  ## Generate propensity scores #
  ###############################

  m_ps <- glm(model_formula, family = binomial(), data = a_sub)
  prs_df <- data.frame(pr_score = predict(m_ps, type = "response"), treatment = m_ps$model$treatment)

  #####################################################################################################
  ## Calculate caliper as \sigma = [(\sigma_1^2 + sigma_0^2)/2]^0.5 as per Rosenbaum and Rubin (1985) #
  #####################################################################################################
  caliper <- 0.25*((var(prs_df[which(prs_df$treatment == 0 ),]$pr_score) + var(prs_df[which(prs_df$treatment == 1 ),]$pr_score))/2)^0.5

  ################################
  ## Nearest match (mahalanobis) #
  ################################ 
  m_out <- matchit(model_formula, distance = "logit", method = "nearest", caliper = caliper, data = a_sub_nomiss, mahvars = mah_vars)

  return(m_out)

}
#######################################################################################################################
## The two elements in the below list are the matched samples for the "before the flood" and "after the flood" groups #
#######################################################################################################################
ldf_m_out <- lapply(ldf, function(df) { matchSamples(das_vars, df) })
ldf_m_data <- lapply(ldf_m_out, match.data)

ldf_m_matches <- lapply(ldf_m_out, function(m_out) { na.omit(data.frame(match.data(m_out)[m_out$match.matrix,das_vars],match.data(m_out)[rownames(m_out$match.matrix),das_vars])) })




dnd_data <- rbind(ldf_m_data[[1]],ldf_m_data[[2]])

dnd_model_formula <- as.formula(paste("ln_sale_price ~ after_flood + flooded + after_flood*flooded + ",model_lhs_vars))
# dnd_model_formula <- as.formula(paste("ln_sale_price ~ after_flood + flood_prone + after_flood*flood_prone + ",model_lhs_vars))

fit <- lm(dnd_model_formula, data=dnd_data)
raw_fit <- lm(dnd_model_formula, data=flood_sub)


####################
## Balance summary #
####################
## b_sum <- summary(m_out)
## b_sum_std <- summary(m_out, standardize = TRUE)
## b_plot <- plot(m_out)
## b_plot_sum <- plot(b_sum_std)

###################
## Zelig analysis #
###################
## z_out <- zelig(zelig_model_formula, model = "ls", data = m_data)
## c_out <- setx(z_out, treatment = 0)
## t_out <- setx(z_out, treatment = 1)
## s_out <- sim(z_out, c_out, t_out)
