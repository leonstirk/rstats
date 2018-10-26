noMiss <- function(a_sub) {
    a_sub_nomiss <- a_sub %>% dplyr::select(qpid, sale_id, sale_year, ln_sale_price, treatment, after_flood, flooded, flood_prone, one_of(tail(das_vars,-1))) %>% na.omit()
    return(a_sub_nomiss)
}

matchSamples <- function(das_vars, a_sub) {
    ## Create model strings #
    match_model_formula <- as.formula(paste("treatment ~ ",model_lhs_vars))

    ## Generate propensity scores #
    m_ps <- glm(match_model_formula, family = binomial(), data = noMiss(a_sub))
    prs_df <- data.frame(pr_score = predict(m_ps, type = "response"), treatment = m_ps$model$treatment)

    ## Calculate caliper as \sigma = [(\sigma_1^2 + sigma_0^2)/2]^0.5 as per Rosenbaum and Rubin (1985) #
    caliper <- 0.25*((var(prs_df[which(prs_df$treatment == 0 ),]$pr_score) + var(prs_df[which(prs_df$treatment == 1 ),]$pr_score))/2)^0.5

    ## Nearest match (mahalanobis) #
    ## m_out <- matchit(match_model_formula, method = "nearest", distance = "logit", caliper = caliper, data = noMiss(a_sub), mahvars = mah_vars)
    m_out <- matchit(match_model_formula, method = "nearest", distance = "mahalanobis", data = noMiss(a_sub), exact = exact_vars)
    
    return(m_out)

}
