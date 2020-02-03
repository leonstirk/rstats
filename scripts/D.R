###########################################A#########################################################################################################

source('scripts/baseline_analysis_prep.R')
source('functions/graphing.R')

####################################################################################################################################################

D <- function(unmatched_sub, resp_var, treat_var, lm_model_formula) {
    m <- matchMahExactNoRep(unmatched_sub)

    ## Send matched outputs to some variables #
    m_out            <- m[[1]]
    m_data           <- m[[2]]
    m_matches        <- m[[3]]

    ## Specify the linear regression model parameters #
    partial_vars     <- c(treat_var, model_vars)
    partial_strings  <- c(treat_var, lm_model_strings)

    ## Organise matched and unmatched samples into a list
    l_a_data         <- list(unmatched_data = unmatched_sub, matched_data = m_data)
    l_a_model        <- lapply(l_a_data, function(x) { doRegression(x, lm_model_formula, partial_strings) })

    l_a_fit          <- lapply(l_a_model, function(x) { x[['fit']] })
    l_a_fit_summary  <- lapply(l_a_fit, summary)
    l_a_partials     <- lapply(l_a_model, function(x) { x[['partials']] })

    ## Summarise post match balance improvement
    bal_sum          <- summary(m_out)
    bal_sum_std      <- summary(m_out, standardize=TRUE)
    imbalance        <- lapply(l_a_data, function(x) { imbalance(group=x[,treat_var], data=x[match_vars]) })

    return(list(
        'match_output' = m_out,
        'match_data' = m_data,
        ## 'match_matches' = m_matches,
        'model_data' = l_a_data,
        'model_fit' = l_a_fit,
        'model_partials' = l_a_partials,
        'model_summary' = l_a_fit_summary,
        'balance_summary' = bal_sum,
        'balance_summary_standardised' = bal_sum_std,
        'imbalance' = imbalance
    ))

}

D_MC <- function(unmatched_sub) {
    m <- matchMahExactNoRep(unmatched_sub)

    ## Send matched outputs to some variables #
    m_out            <- m[[1]]
    m_data           <- m[[2]]

    ## Specify the linear regression model parameters #
    model_formula    <- as.formula(paste("ln_sale_price ~ treatment + ",model_all))
    fit              <- lm(model_formula, data = m_data)

    ## Summarise post match balance improvement
    bal_sum_std      <- summary(m_out, standardize=TRUE)

    return(list(
        'match_output' = m_out,
        'balance_summary_standardised' = bal_sum_std,
        'match_data' = m_data,
        'model_fit' = fit
    ))

}

############################
## Montecarlo match object #
############################

## loop <- seq(1,100)
## montecarlo_match_D <- list()

## for(i in loop) {
##     start.time <- Sys.time()
##     set.seed(i)
##     montecarlo_match_D[[i]] <- D_MC(flood_data_subsets[["F1"]])
##     end.time <- Sys.time()
##     time.taken <- end.time - start.time
##     print(time.taken)
##     print(i)
## }

## montecarlo_match_D[[i]][["model_summary"]][["matched_data"]][["coefficients"]]["treatment","Pr(>|t|)"]
## sum(montecarlo_match_D[[i]][["balance_summary_standardised"]][["reduction"]][["Std. Mean Diff."]][1:5])/5

############################
## Single run match object #
############################

D_BF <- D(flood_data_subsets[["BF"]], "ln_sale_price", "flood_prone", lm_model_formula)

####################
## Graphing stuff ##
####################

## mc_BF <- readRDS('bootstrapped_regressions/D_BF.rds')
## set.seed(Most_Sig_Result_Seed(mc_BF, "treatment"))
## D_BF <- D(flood_data_subsets[["F1"]])

## layout(matrix(seq(1,2,1),2,1))
## KDE_MC_SIG(mc_BF, D_BF, 'treatment')
## KDE_MC_EST(mc_BF, D_BF, 'treatment')

##############
## Geo plots #
##############
## raw_plot <- ggplot(unmatched_sub, aes(lon_gd2000_x, lat_gd2000_y, color = flood_analysis_group)) + geom_point()
## dnd_plot <- ggplot(matched_sub, aes(lon_gd2000_x, lat_gd2000_y, color = flood_analysis_group)) + geom_point()
