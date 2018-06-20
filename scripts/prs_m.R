source('scripts/das_data_preprocessing.R')

## generate quarterly price index across all data (no spatial submarkets)

a <- das_caversham

mean_summary <- a %>% group_by(sale_year) %>% dplyr::summarise(n_sale_year = n(), mean_ln_sale_price = mean(ln_sale_price), std_error = sd(ln_sale_price)/sqrt(n_sale_year))

t <- as.numeric(levels(as.factor(a$sale_year)))
ti <- t[2]

a_sub <- subset(a, a$sale_year == ti | a$sale_year == ti+1)
a_sub$treatment <- ifelse(a_sub$sale_year == ti, 0 ,1)

# non-covariate-adjusted (non-matched) difference in mean test for log net sale price between treatment and control groups
t_test_output <- with(a_sub, t.test(ln_sale_price ~ sale_year))

# define covariate space for pre-treatment
a_sub_cov<-tail(das_vars,-1)

a_sub_pretreatment_covariate_means <- a_sub %>% group_by(treatment) %>% dplyr::select(one_of(a_sub_cov)) %>% summarise_all(funs(mean(., na.rm = T)))

t_tests <- lapply(a_sub_cov, function(v) { t.test(a_sub[, v] ~ a_sub[, 'treatment']) })

a_sub_pretreatment_covariate_t_stats <- unlist(lapply(t_tests, function(v) { v$statistic }))
a_sub_pretreatment_covariate_p_values <- unlist(lapply(t_tests, function(v) { v$p.value }))
a_sub_pretreatment_covariate_sig_dummy <- ifelse(a_sub_pretreatment_covariate_p_values <= 0.05, 1, 0)

a_sub_pretreatment_cov_test_tables <- data.frame(a_sub_cov, a_sub_pretreatment_covariate_t_stats, a_sub_pretreatment_covariate_p_values, a_sub_pretreatment_covariate_sig_dummy)

# propensity score estimation
covariates<- a_sub %>% dplyr::select(QPID, ln_sale_price, treatment, one_of(a_sub_cov))
m_ps <- glm(treatment ~ ., family = binomial(), data = covariates)
prs_df <- data.frame(pr_score = predict(m_ps, type = "response"), treatment = m_ps$model$treatment)

# Visual check of propensity score based distributional matching
# Kernel density estimates of propensity scores by treatment (t0, t1)
sm.density.compare(prs_df$pr_score, prs_df$treatment)

# Execute matching algorithm
a_sub_nomiss <- a_sub %>% select(QPID, sale_id, sale_quarter, ln_sale_price, treatment, one_of(a_sub_cov)) %>% na.omit()

modelstring <- 


mod_match <- matchit(modelstring, method = "nearest", data = das_sub_nomiss)

# dta_m <- match.data(mod_match)

