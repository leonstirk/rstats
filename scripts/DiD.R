#########################A###########################################################################################################################

source('scripts/flooding_analysis_prep.R')
source('functions/graphing.R')

####################################################################################################################################################

##############################
## Assign treatment variable #
##############################

F_NF$treatment   <- as.factor(F_NF$flood)
NF_NFP$treatment <- as.factor(NF_NFP$non_flood)
F_NFP$treatment  <- as.factor(F_NFP$flood)
FP_NFP$treatment <- as.factor(FP_NFP$flood_prone)

flood_data_subsets <- list('F_NF' = F_NF, 'NF_NFP' = NF_NFP, 'F_NFP' = F_NFP, 'FP_NFP' = FP_NFP)

DiD <- function(unmatched_sub) {
    start.time <- Sys.time()

    ## Split into before_flood and after_flood groups respectively #
    ldf                   <- list('before_flood' = subset(unmatched_sub, after_flood == 0), 'after_flood' = subset(unmatched_sub, after_flood == 1))

    ## Do matching on before_flood and after_flood groups #
    l_m                   <- lapply(ldf, function(df) { matchMahExactNoRep(df) })

    ## Send matched outputs to some variables #
    l_m_out               <- lapply(l_m, function(l_m) { l_m[[1]] })
    l_m_data              <- lapply(l_m, function(l_m) { l_m[[2]] })

    ## Recombine the post matching sample #
    matched_sub           <- rbind(l_m_data[[1]],l_m_data[[2]])

    ## Specify the linear regression model parameters #
    model_formula         <- as.formula(paste("ln_sale_price ~ after_flood * treatment + ",model_all))
    partial_vars	  <- c("after_flood:treatment", model_vars[-1])
    partial_strings       <- c("after_flood:treatment", model_strings[-1])

    ## Organise matched and unmatched samples into a list
    l_a_data              <- list(unmatched_data = unmatched_sub, matched_data = matched_sub)

    l_a_descriptives      <- lapply(lapply(l_a_data, function(x) { as.data.frame(lapply(x[des_vars], function(x) { as.numeric(as.character(x)) })) }), makeDescriptives)

    ## Do the linear regression on the pre and post matched samples #
    l_a_model             <- lapply(l_a_data, function(data) {
        fit <- lm(model_formula, data = data)
        partials <- partial_resid_numeric_analysis(partial_strings, fit, data)
        return(list('fit' = fit, 'partials' = partials))
    })

    l_a_fit               <- lapply(l_a_model, function(x) { x[['fit']] })
    l_a_partials          <- lapply(l_a_model, function(x) { x[['partials']] })
    l_a_fit_summary       <- lapply(l_a_fit, summary)

    ## Summarise post match balance improvement
    l_bal_sum_std         <- lapply(l_m_out, function(m_out) { summary(m_out, standardize=TRUE) })

    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)

    return(list(
        'match_output' = l_m_out,
        'model_data' = l_a_data,
        'model_fit' = l_a_fit,
        'model_partials' = l_a_partials,
        'descriptives' = l_a_descriptives,
        'model_summary' = l_a_fit_summary,
        'balance_summary_standardised' = l_bal_sum_std
    ))
}

DiD_MC <- function(unmatched_sub) {

    ## Split into before_flood and after_flood groups respectively #
    ldf                   <- list('before_flood' = subset(unmatched_sub, after_flood == 0), 'after_flood' = subset(unmatched_sub, after_flood == 1))

    ## Do matching on before_flood and after_flood groups #
    l_m                   <- lapply(ldf, function(df) { matchMahExactNoRep(df) })

    ## Send matched outputs to some variables #
    l_m_out               <- lapply(l_m, function(l_m) { l_m[[1]] })
    l_m_data              <- lapply(l_m, function(l_m) { l_m[[2]] })

    ## Recombine the post matching sample #
    matched_sub           <- rbind(l_m_data[[1]],l_m_data[[2]])

    ## Specify the linear regression model parameters #
    model_formula         <- as.formula(paste("ln_sale_price ~ after_flood * treatment + ",model_all))

    ## Do the linear regression on the pre and post matched samples #
    fit <- lm(model_formula, data = matched_sub)

    ## Summarise post match balance improvement
    l_bal_sum_std         <- lapply(l_m_out, function(m_out) { summary(m_out, standardize=TRUE) })

    return(list(
        'match_output' = l_m_out,
        'balance_summary_standardised' = l_bal_sum_std,
        'model_data' = matched_sub,
        'model_fit' = fit,
    ))
}


############################
## Montecarlo match object #
############################

## loop <- seq(1,100)
## montecarlo_match_DiD <- list()

## for(i in loop) {
##     start.time <- Sys.time()
##     set.seed(i)
##     montecarlo_match_DiD[[i]] <- DiD_MC(flood_data_subsets[["FP_NFP"]])
##     end.time <- Sys.time()
##     time.taken <- end.time - start.time
##     print(time.taken)
##     print(i)
## }

############################
## Single run match object #
############################

DiD <- DiD(flood_data_subsets[["FP_NFP"]])

####################
## Graphing stuff ##
####################

## mc_DiD <- readRDS('bootstrapped_regressions/DiD_FP_NFP.rds')
## set.seed(Most_Sig_Result_Seed(mc_DiD, "after_flood1:treatment1"))
## DiD <- DiD(flood_data_subsets[["FP_NFP"]])

## layout(matrix(seq(1,6,1),2,3))
## KDE_MC_SIG(mc_DiD, DiD, 'after_flood1')
## KDE_MC_EST(mc_DiD, DiD, 'after_flood1')
## KDE_MC_SIG(mc_DiD, DiD, 'treatment1')
## KDE_MC_EST(mc_DiD, DiD, 'treatment1')
## KDE_MC_SIG(mc_DiD, DiD, 'after_flood1:treatment1')
## KDE_MC_EST(mc_DiD, DiD, 'after_flood1:treatment1')

##############
## Geo plots #
##############
## raw_plot <- ggplot(unmatched_sub, aes(lon_gd2000_x, lat_gd2000_y, color = flood_analysis_group)) + geom_point()
## dnd_plot <- ggplot(matched_sub, aes(lon_gd2000_x, lat_gd2000_y, color = flood_analysis_group)) + geom_point()
