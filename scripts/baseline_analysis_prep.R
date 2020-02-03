####################################################################################################################################################

source('scripts/flooding_data_processing.R')

####################################################################################################################################################

#####################
## Import functions #
#####################

## Reserve variable name 'data' for the partial residual analysis functions #
data <- data.frame() ## rm(data) after partial residual analysis has run #
source('functions/partial_residual_analysis_numeric_variables.R')
source('functions/partial_plots.R')

######################
## Set time blocking #
######################

before_flood <- subset(das, sale_date > (flood_date - (365 * 4)) & sale_date < flood_date) ## Before flood only
after_flood  <- subset(das, sale_date < (flood_date + (365 * 3.5)) & sale_date > flood_date)  ## After flood only
flood_sub    <- subset(das, sale_date > (flood_date - (365 * 4)) & sale_date < (flood_date + (365 * 3.5)))  ## Before and after flood

restricted   <- subset(das, sale_date > (flood_date - (365 * 4)) & sale_date < (flood_date + (365 * 3.5)))
restricted   <- subset(restricted, flood_prone == 1)

flood_data_subsets   <- list('BF' = before_flood, 'AF' = after_flood, 'IF' = flood_sub, "RF" = restricted)

## Remove temporary variables #
rm(before_flood, after_flood, flood_sub, restricted)

bf_variables_summary <- floodVariablesSummary(flood_data_subsets[["BF"]])
bf_sample_descriptives <- sampleDescriptivesTable(flood_data_subsets[["BF"]])

if_variables_summary <- floodVariablesSummary(flood_data_subsets[["IF"]])
if_sample_descriptives <- sampleDescriptivesTable(flood_data_subsets[["IF"]])

rf_variables_summary <- floodVariablesSummary(flood_data_subsets[["RF"]])
rf_sample_descriptives <- sampleDescriptivesTable(flood_data_subsets[["RF"]])
