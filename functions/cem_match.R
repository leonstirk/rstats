noMiss <- function(a_sub) {
    a_sub_nomiss <- a_sub %>% dplyr::select(qpid, sale_id, dist_cbd, sale_year, lon_gd2000_x, lat_gd2000_y, ln_sale_price, treatment, after_flood, flooded, flood_prone, tainui, flood_analysis_group, one_of(tail(das_vars,-1)), one_of(names(sale_year_dummies))) %>% na.omit()
    return(a_sub_nomiss)
}

matchSamples <- function(a_sub) {
    
    ########
    ## CEM #
    ########

    ## Create match model strings #
    match_model_formula <- as.formula(paste("treatment ~ ",model_cem))
    
    ## Match #
    m_out <- cem(treatment = "treatment", data = noMiss(a_sub), drop = c('qpid', 'sale_id', 'dist_cbd', 'sale_year', 'lon_gd2000_x', 'lat_gd2000_y', 'ln_sale_price', 'treatment', 'after_flood', 'flooded', 'flood_prone', 'tainui', 'flood_analysis_group'))

    return(m_out)
    
}
