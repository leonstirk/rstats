source('scripts/das_data_preprocessing.R')

library('sm')

a <- das_caversham

mean_summary <- a %>% group_by(sale_year) %>% dplyr::summarise(n_sale_year = n(), mean_ln_sale_price = mean(ln_sale_price), std_error = sd(ln_sale_price)/sqrt(n_sale_year))

t_vec <- as.numeric(levels(as.factor(a$sale_year)))

# layout(matrix(seq(1,15,1),3,5)) # optional 12 graphs/page;

## Loop over each base year
# for(t0 in t_vec) {
t0 <- '2000'

  ## Loop over each year pair
  # for (t1 in t_vec) {
  t1 <- '2001'

    ## Only match where t0 != t1
    # if(t0 != t1) {

a_sub <- subset(a, a$sale_year == t0 | a$sale_year == t1)
a_sub$treatment <- ifelse(a_sub$sale_year == t0, 0 ,1)

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

# model strings
model_lhs_vars <- paste(a_sub_cov, collapse = " + ")
matchmodelstring <- paste("treatment ~ ",model_lhs_vars)

m_ps <- glm(matchmodelstring, family = binomial(), data = a_sub)
prs_df <- data.frame(pr_score = predict(m_ps, type = "response"), treatment = m_ps$model$treatment)

# Visual check of propensity score based distributional matching
# Kernel density estimates of propensity scores by treatment (t0, t1)
sm.density.compare(prs_df$pr_score, prs_df$treatment)

#    }
#  }
# }



# Execute matching algorithm
a_sub_nomiss <- a_sub %>% dplyr::select(QPID, sale_id, sale_year, ln_sale_price, treatment, one_of(a_sub_cov)) %>% na.omit()

mod_match <- matchit(treatment ~  bedrooms + bathrooms + carparks + offstreet_parking + deck + ex_state_house + contour + period_built + ln_building_floor_area + ln_land_area, method = "nearest", data = a_sub_nomiss)

dta_m <- match.data(mod_match)

source('functions/fn_bal.R')

fn_bal(dta_m, "bedrooms")

TE <- data.frame(dta_m[rownames(mod_match$match.matrix),"ln_sale_price"],dta_m[mod_match$match.matrix,"ln_sale_price"])

names(TE) <- c('ln_sale_t1','ln_sale_t0')

TE$diff <- TE$ln_sale_t1 - TE$ln_sale_t0