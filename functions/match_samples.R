noMiss <- function(a_sub) {
    a_sub_nomiss <- a_sub %>% dplyr::select(qpid, sale_id, dist_cbd, sale_year, lon_gd2000_x, lat_gd2000_y, ln_sale_price, net_sale_price, treatment, after_flood, flooded, flood_prone, tainui, flood_analysis_group, one_of(tail(das_vars,-1)), one_of(names(sale_year_dummies))) %>% na.omit()
    return(a_sub_nomiss)
}

matchSamples <- function(a_sub) {

    ## ########################################################
    ## ## Mahalanobis metric within propensity score calipers #
    ## ########################################################
    
    ## ## Create match model strings #
    ## match_model_formula <- as.formula(paste("treatment ~ ",model_all))

    ## ## Generate propensity scores #
    ## m_ps <- glm(match_model_formula, family = binomial(), data = noMiss(a_sub))
    ## prs_df <- data.frame(pr_score = predict(m_ps, type = "response"), treatment = m_ps$model$treatment)

    ## ## Calculate caliper as \sigma = [(\sigma_1^2 + sigma_0^2)/2]^0.5 as per Rosenbaum and Rubin (1985) #
    ## caliper <- 0.25*((var(prs_df[which(prs_df$treatment == 0 ),]$pr_score) + var(prs_df[which(prs_df$treatment == 1 ),]$pr_score))/2)^0.5

    ## ## Match #
    ## m_out <- matchit(match_model_formula, method = "nearest", distance = "logit", caliper = caliper, data = noMiss(a_sub), mahvars = mah_vars)


    ## ###########################################################    
    ## Mahalanobis metric with exact matching on dummy variables #
    ## ###########################################################

    ## Create match model strings #
    match_model_formula <- as.formula(paste("treatment ~ ",model_mah))

    ## Match #
    m_out <- matchit(match_model_formula, method = "nearest", distance = "mahalanobis", data = noMiss(a_sub), exact = exact_vars)
    
    ## Matched sample data
    m_data <- match.data(m_out)

    ## Matched sample matches
    m_match <- na.omit(data.frame(match.data(m_out)[m_out$match.matrix,das_vars],match.data(m_out)[rownames(m_out$match.matrix),das_vars]))
    
    return(list(m_out, m_data, m_match))


    ## ## Another possibility is Mahalanobis metric with exact matching on dummies WITH replacement #
    ## ## In that case the weight matrix will reflect the number of times a control unit is matched with a treatment unit #
    ## ## The subsequent parametric analysis would involve a weighted regression #

    ## ## Create match model strings #
    ## match_model_formula <- as.formula(paste("treatment ~ ",model_mah))

    ## ## Match #
    ## m_out <- matchit(match_model_formula, method = "nearest", distance = "mahalanobis", data = noMiss(a_sub), exact = exact_vars, replace = TRUE)
    
    ## ## Matched sample data
    ## m_data <- match.data(m_out)

    ## ## Matched sample matches
    ## m_match <- na.omit(data.frame(match.data(m_out)[m_out$match.matrix,das_vars],match.data(m_out)[rownames(m_out$match.matrix),das_vars]))
    
    ## return(list(m_out, m_data, m_match))

}
