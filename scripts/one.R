####################################################################################################################################################

source('scripts/flooding_data_processing.R')

####################################################################################################################################################

######################
## Set time blocking #
######################

years <- 6
days <- years*365
start_date <- flood_date - days
end_date <- flood_date

flood_sub <- subset(flood_sub,sale_date>start_date & sale_date<end_date)
flood_sub$after_flood <- ifelse(flood_sub$sale_date<flood_date,0,1)
flood_sub$after_flood <- as.factor(flood_sub$after_flood)

##################################
## Set model parameter variables #
##################################

source('scripts/set_model_strings.R')

## Import functions #
source('functions/match_samples.R')
## source('functions/cem_match.R')

source('functions/density_compare.R')

####################################################################################################################################################

F_NF <- subset(flood_sub, flood == 1 | non_flood == 1)
NF_NFP <- subset(flood_sub, flood == 0)
F_NFP <- subset(flood_sub, non_flood == 0)
FP_NFP <- flood_sub

##############################
## Assign treatment variable #
##############################

F_NF$treatment   <- F_NF$flood
NF_NFP$treatment <- NF_NFP$non_flood
F_NFP$treatment  <- F_NFP$flood
FP_NFP$treatment <- FP_NFP$flood_prone

flood_data_subsets <- list('F_NF' = F_NF, 'NF_NFP' = NF_NFP, 'F_NFP' = F_NFP, 'FP_NFP' = FP_NFP)

results <- lapply(flood_data_subsets, function(unmatched_sub) {

  ## Do matching on before_flood and after_flood groups #
  l_m                   <- matchSamples(unmatched_sub)

  ## Send matched outputs to some variables #
  l_m_out               <- lapply(l_m, function(l_m) { l_m[[1]] })
  l_m_data              <- lapply(l_m, function(l_m) { l_m[[2]] })
  l_m_matches           <- lapply(l_m, function(l_m) { l_m[[3]] })

  ## Recombine the post matching sample #
  matched_sub               <- rbind(l_m_data[[1]],l_m_data[[2]])

  ## Specify the linear regression model parameters #
  model_formula     <- as.formula(paste("ln_sale_price ~ after_flood*treatment+ ",model_all))

  ## Do the linear regression on the pre and post matched samples #
  l_a_data              <- list(unmatched_data = unmatched_sub, matched_data = matched_sub)

  l_a_fit               <- lapply(l_a_data, function(data) { lm(model_formula, data = data) })
  l_a_fit_summary       <- lapply(l_a_fit, summary)
  l_a_fit_summary_clean <- lapply(l_a_fit_summary, function(fit) {clean_summary(fit$coefficients,4)})

  ## Summarise post match balance improvement
  l_bal_sum             <- lapply(l_m_out, summary)
  l_bal_sum_std         <- lapply(l_m_out, function(m_out) { summary(m_out, standardize=TRUE) })

  return(list(
    'match_output' = l_m_out,
    'match_data' = l_m_data,
    'match_matches' = l_m_matches,
    'model_data' = l_a_data,
    'model_fit' = l_a_fit,
    'model_summary' = l_a_fit_summary,
    'model_summary_clean' = l_a_fit_summary_clean,
    'balance_summary' = l_bal_sum,
    'balance_summary_standardised' = l_bal_sum_std
  ))

})

# lapply(results[["FP_NFP"]][["model_data"]], function(x) { densityCompare(x[,mah_vars[1]], x[,'treatment']) })

lapply(names(results), function(subset) {
  for(i in 1:length(mah_vars)) {
    png(filename = paste(c(subset, mah_vars[i], ".png"), collapse = '_'))
    layout(matrix(seq(1,2,1),1,2))
    lapply(names(results[[subset]][["model_data"]]), function(x) {
      k <- results[[subset]][["model_data"]][[x]]
      print(densityCompare(k[,mah_vars[i]], k[,'treatment'], mah_vars[i], x))
    })
    dev.off()
  }
})

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