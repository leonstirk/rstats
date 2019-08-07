####################################################################################################################################################

source('scripts/flooding_data_processing.R')

####################################################################################################################################################

## set.seed(1000)

######################
## Set time blocking #
######################

years <- 3
days <- years*365
minus_6 <- flood_date - (days * 2)
minus_3 <- flood_date - days
plus_3  <- flood_date + days

flood_sub$after_flood <- ifelse(flood_sub$sale_date < flood_date,0,1)

flood_sub$after_flood     <- as.factor(flood_sub$after_flood)
flood_sub$flood           <- as.factor(flood_sub$flood)
flood_sub$non_flood       <- as.factor(flood_sub$non_flood)
flood_sub$flood_prone     <- as.factor(flood_sub$flood_prone)
flood_sub$non_flood_prone <- as.factor(flood_sub$non_flood_prone)

flood_sub_1 <- subset(flood_sub, sale_date > minus_3 & sale_date < flood_date)
flood_sub_2 <- subset(flood_sub, sale_date > minus_6 & sale_date < flood_date)
flood_sub_3 <- subset(flood_sub, sale_date > minus_3 & sale_date < plus_3)

time_window_1 <- c(minus_3, flood_date)
time_window_2 <- c(minus_6, flood_date)
time_window_3 <- c(minus_3, minus_6)

## Remove temporary variables #
rm(days, minus_6, minus_3, plus_3, years)

##################################
## Set model parameter variables #
##################################

source('scripts/set_model_strings.R')

#####################
## Import functions #
#####################

source('functions/match_samples.R')
## source('functions/cem_match.R')

source('functions/density_compare.R')

## Reserve variable name 'data' for the partial residual analysis functions #
data <- data.frame() ## rm(data) after partial residual analysis has run #
source('functions/partial_residual_analysis_numeric_variables.R')
source('functions/multiplot.R')
source('functions/partial_plots.R')

####################################################################################################################################################
