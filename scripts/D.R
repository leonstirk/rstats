####################################################################################################################################################

source('scripts/baseline_analysis_prep.R')

####################################################################################################################################################

###############
## Functions  #
###############


doDescriptives <- function(data, des_vars) {

    x <- as.data.frame(lapply(data[des_vars], function(x) { as.numeric(as.character(x)) }))

    descriptives <- c('mean', 'median', 'sd', 'min', 'max')
    df <- data.frame(matrix(, nrow = ncol(x), ncol = 0))
    for(i in descriptives) {
        df[,i] <- sapply(x, i, na.rm = TRUE)
    }
    rownames(df) <- colnames(x)
    return(df)
}

doRegression <- function(data, model_formula, partial_strings) {
    fit <- lm(model_formula, data = data)
    partials <- partial_resid_numeric_analysis(partial_strings, fit, data)
    return(list('fit' = fit, 'partials' = partials))
}


##############################
## Assign treatment variable #
##############################

flood_data_subsets <- list('F1' = flood_sub_1, 'F2' = flood_sub_2, 'F3' = flood_sub_3)

flood_data_subsets <- Map(cbind, flood_data_subsets, treatment = lapply(flood_data_subsets, function(df) { df$treatment <- ifelse(df$flood_prone == 1,1,0) }))

## Specify the linear regression model parameters and partial strings #
model_formula         <- as.formula(paste("ln_sale_price ~ flood_prone + ",model_all))
partial_vars	  <- c("flood_prone", model_vars[-1])
partial_strings       <- c("flood_prone", model_strings[-1])

D_match <- function(df) {

    start.time <- Sys.time()

    ## Do matching on before_flood and after_flood groups #
    m <- matchSamples(df)

    ## Send matched outputs to some variables #
    m_out            <- m[[1]]
    m_data           <- m[[2]]
    m_matches        <- m[[3]]

    ## Make some descriptive statistics about the data
    descriptives     <- doDescriptives(m_data, des_vars)

    ## Do the linear regression on the pre and post matched samples #
    model            <- doRegression(m_data, model_formula, partial_strings)

    fit              <- model[['fit']]
    partials         <- model[['partials']]
    fit_summary      <- summary(fit)

    ## Summarise post match balance improvement
    bal_sum_std      <- summary(m_out, standardize=TRUE)

    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)

    return(list(
        'match_output' = m_out,
        'match_data' = m_data,
        'match_matches' = m_matches,
        'descriptives' = descriptives,
        'model_fit' = fit,
        'model_partials' = partials,
        'model_summary' = fit_summary,
        'balance_summary_standardised' = bal_sum_std
    ))
}

D_unmatch <- function() {

}

loop <- seq(1,100)
montecarlo <- list()

for(i in loop) {
    set.seed(i)
    montecarlo[[i]] <- D_match()
}


## results <- lapply(flood_data_subsets, function(unmatched_sub) {

##     ## Split into before_flood and after_flood groups respectively #
##     ldf                   <- list(unmatched_sub)

##     ######################################################################

##     ## Do matching on before_flood and after_flood groups #
##     l_m                   <- lapply(ldf, function(df) { matchSamples(df) })

##     ## Send matched outputs to some variables #
##     ## l_m_out               <- lapply(l_m, function(l_m) { l_m[[1]] })
##     l_m_data              <- lapply(l_m, function(l_m) { l_m[[2]] })
##     ## l_m_matches           <- lapply(l_m, function(l_m) { l_m[[3]] })

##     ## Recombine the post matching sample #
##     matched_sub           <- l_m_data[[1]]

##     ######################################################################

##     ## Specify the linear regression model parameters #
##     model_formula         <- as.formula(paste("ln_sale_price ~ flood_prone + ",model_all))
##     partial_vars	  <- c("flood_prone", model_vars[-1])
##     partial_strings       <- c("flood_prone", model_strings[-1])

##     ######################################################################

##     ## Organise matched and unmatched samples into a list
##     l_a_data              <- list(unmatched_data = unmatched_sub, matched_data = matched_sub)

##     ######################################################################

##     ## Make some descriptive statistics about the data

##     l_a_descriptives      <- lapply(l_a_data, function(x) {
##                                        df <- doDescriptives(x, des_vars)
##                                        return(df)
##                                    })

##     ######################################################################

##     ## Do the linear regression on the pre and post matched samples #

##     l_a_model <- lapply(l_a_data, function(x) {
##                            reg <- doRegression(x, model_formula, partial_strings)
##                            return(reg)
##                        })

##     ## l_a_fit               <- lapply(l_a_model, function(x) { x[['fit']] })
##     l_a_partials          <- lapply(l_a_model, function(x) { x[['partials']] })
##     l_a_fit_summary       <- lapply(l_a_fit, summary)

##     ######################################################################

##     ## Summarise post match balance improvement

##     ## l_bal_sum             <- lapply(l_m_out, summary)
##     ## l_bal_sum_std         <- lapply(l_m_out, function(m_out) { summary(m_out, standardize=TRUE) })

##     ######################################################################

##     return(list(
##         ## 'match_output' = l_m_out,
##         ## 'match_data' = l_m_data,
##         ## 'match_matches' = l_m_matches,
##         ## 'model_data' = l_a_data,
##         ## 'descriptives' = l_a_descriptives,
##         ## 'model_fit' = l_a_fit,
##         'model_partials' = l_a_partials,
##         'model_summary' = l_a_fit_summary
##         ## 'balance_summary' = l_bal_sum,
##         ## 'balance_summary_standardised' = l_bal_sum_std
##     ))

## })



## Remove reserved 'data' variable used for partial residual analysis #
rm(data)

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

# png(filename = "scale_partial_plots.png",
#   width = 1500,
#   height = 1500,
#   units = "px",
#   pointsize = 25,
#   bg = "white",
#   res = NA
# )
# layout(matrix(seq(1,4,1),2,2))
# for(i in seq(2,5,1)) {
#   print(partial_comparison_plot_scale(results[["F_NFP"]], model_strings[i], model_vars[i]))
# }
# dev.off()

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

## something <- c("flood_prone", "arterial_street", "offstreet_parking", "deck", "bathrooms", "period_built", "contour", "sale_year")

## multiplot(
##   for (thing in something) {
##     partial_comparison_plot_factor(results[["F1"]], thing),
##   }
##   cols = 3
## )

## multiplot(
##   partial_comparison_plot <- factor(results[["F2"]], 'flood_prone'),
##   partial_comparison_plot <- factor(results[["F1"]], 'flood_prone'),
##   partial_comparison_plot <- factor(results[["F3"]], 'flood_prone'),
##   cols = 3
## )

## dev.off()

###########################################################################################

rm(flood_data_subsets)

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
