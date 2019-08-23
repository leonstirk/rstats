####################################################################################################################################################

source('scripts/flooding_analysis_prep.R')

####################################################################################################################################################

ldf <- list(F_NF, F_NFP)

DiDiD <- function(ldf) {

    ## Assign treatment variable #
    ldf                   <- Map(cbind, ldf, treatment = lapply(ldf, function(df) { df$treatment <- ifelse(df$flood == 1,1,0) }))

    ## Split into before_flood and after_flood groups respectively #
    ldf_t                 <- lapply(ldf, function(df) { list('before_flood' = subset(df, after_flood == 0), 'after_flood' = subset(df, after_flood == 1)) })

    ## Do Exact + Mahalanobis (without replacement) matching on before_flood and after_flood groups #
    l_m                   <- lapply(ldf_t, function(ldf) { lapply(ldf, function(df) { matchSamples(df) }) })

    l_m_out               <- lapply(l_m, function(area_diff) { lapply(area_diff, function(time_diff) { time_diff[[1]] }) })
    l_m_data              <- lapply(l_m, function(area_diff) { lapply(area_diff, function(time_diff) { time_diff[[2]] }) })
    ## l_m_matches           <- lapply(l_m, function(area_diff) { lapply(area_diff, function(time_diff) { time_diff[[3]] }) })

    ## Do CEM matching on before_flood and after_flood groups



    ## Recombine the post matching sample #
    matched_sub           <- rbind(l_m_data[[1]][[1]],l_m_data[[1]][[2]],l_m_data[[2]][[1]],l_m_data[[2]][[2]])

    ## Specify the linear regression model parameters (diff in diff in diff)#
    model_formula         <- as.formula(paste("ln_sale_price ~ after_flood*flood_prone*flood + ",model_all))
    partial_vars	      <- c("after_flood:flood_prone:flood", model_vars[-1])
    partial_strings       <- c("after_flood:flood_prone:flood", model_strings[-1])

    ## Organise matched and unmatched samples into a list
    l_a_data              <- list(unmatched_data = flood_sub, matched_data = matched_sub)

    l_a_descriptives      <- lapply(lapply(l_a_data, function(x) { as.data.frame(lapply(x[des_vars], function(x) { as.numeric(as.character(x)) })) }), makeDescriptives)

    ## Do the linear regression on the pre and post matched samples #
    l_a_model             <- lapply(l_a_data, function(data) {
        fit      <- lm(model_formula, data = data)
        partials <- partial_resid_numeric_analysis(partial_strings, fit, data)
        return(list('fit' = fit, 'partials' = partials))
    })

    l_a_fit               <- lapply(l_a_model, function(x) { x[['fit']] })
    l_a_partials          <- lapply(l_a_model, function(x) { x[['partials']] })

    l_a_fit_summary       <- lapply(l_a_fit, summary)

    ## Summarise post match balance improvement
    l_bal_sum             <- lapply(l_m_out, summary)
    l_bal_sum_std         <- lapply(l_m_out, function(m_out) { summary(m_out, standardize=TRUE) })

    results <- list(
        ## 'match_output' = l_m_out,
        ## 'match_data' = l_m_data,
        ## 'match_matches' = l_m_matches,
        ## 'model_data' = l_a_data,
        ## 'model_fit' = l_a_fit,
        ## 'model_partials' = l_a_partials,
        ## 'descriptives' = l_a_descriptives,
        'model_summary' = l_a_fit_summary
        ## 'balance_summary' = l_bal_sum,
        ## 'balance_summary_standardised' = l_bal_sum_std
    )

    return(results)
}

## huxreg(results[["model_summary"]], stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1), statistics = c(N = "nobs", R2 = "r.squared"))


## loop <- seq(1,100)
## montecarlo <- list()

## for(i in loop) {

##     set.seed(i)

##     montecarlo[[i]] <- DiDiD(ldf)

## }


set.seed(20)
results <- DiDiD(ldf)


## Remove reserved 'data' variable used for partial residual analysis #
## rm(data)


#################################################################
## Generate balance improvement kernel density comparison plots #
#################################################################

# lapply(names(results), function(subset) {
#   for(i in 1:length(mah_vars)) {
#     png(filename = paste(c(subset, mah_vars[i], ".png"), collapse = '_'))
#     layout(matrix(seq(1,2,1),1,2))
#     lapply(names(results[[subset]][["model_data"]]), function(x) {
#       k <- results[[subset]][["model_data"]][[x]]
#       print(densityCompare(k[,mah_vars[i]], k[,'treatment'], mah_vars[i], x))
#     })
#     dev.off()
#   }
# })

###########################
## Generate partial plots #
###########################

##################################################################
## Generate numeric scale partial plots using model_strings[2:5] #
##################################################################

## png(filename = "scale_partial_plots.png",
##   width = 1500,
##   height = 1500,
##   units = "px",
##   pointsize = 25,
##   bg = "white",
##   res = NA
## )
## layout(matrix(seq(1,4,1),2,2))
## for(i in seq(2,5,1)) {
##   print(partial_comparison_plot_scale(results, model_strings[i], model_vars[i]))
## }
## dev.off()

###########################################################################################

##########################################################
## Generate factor partial plots using model_strings[6:] #
##########################################################

## png(filename = "factor_partial_plots.png",
##   width = 1500,
##   height = 1500,
##   units = "px",
##   pointsize = 10,
##   bg = "white",
##   res = 100
## )

## multiplot(
##   partial_comparison_plot_factor(results, "after_flood"),
##   partial_comparison_plot_factor(results, "treatment"),
##   partial_comparison_plot_factor(results, "arterial_street"),
##   partial_comparison_plot_factor(results, "offstreet_parking"),
##   partial_comparison_plot_factor(results, "deck"),
##   # partial_comparison_plot_factor(results, "good_land_view"),
##   # partial_comparison_plot_factor(results, "good_water_view"),
##   # partial_comparison_plot_factor(results, "bedrooms"),
##   partial_comparison_plot_factor(results, "bathrooms"),
##   partial_comparison_plot_factor(results, "period_built"),
##   partial_comparison_plot_factor(results, "contour"),
##   partial_comparison_plot_factor(results, "sale_year"),
##   cols = 3
## )

## dev.off()

###########################################################################################

######################
## Interaction plots #
######################

## png(filename = "interaction_partial_plot.png",
##   width = 1500,
##   height = 1500,
##   units = "px",
##   pointsize = 10,
##   bg = "white",
##   res = 200
## )

## partial_comparison_plot_interaction(results[["F_NFP"]], "after_flood","treatment","after_flood:treatment")

## dev.off()

###########################################################################################

##################
## Balance plots #
##################
## l_bal_plot <- lapply(l_m_out, plot)
## l_bal_plot_sum <- lapply(l_m_out, function(m_out) { plot(summary(m_out, standardize=TRUE)) })

##############
## Geo plots #
##############
## raw_plot <- ggplot(unmatched_sub, aes(lon_gd2000_x, lat_gd2000_y, color = flood_analysis_group)) + geom_point()
## dnd_plot <- ggplot(matched_sub, aes(lon_gd2000_x, lat_gd2000_y, color = flood_analysis_group)) + geom_point()
