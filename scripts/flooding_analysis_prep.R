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

years <- 3
days <- years*365
start_date <- flood_date - days
end_date <- flood_date + days

flood_sub <- subset(das, sale_date > start_date & sale_date < end_date)

## flood_sub$after_flood     <- as.factor(flood_sub$after_flood)
## flood_sub$flood           <- as.factor(flood_sub$flood)
## flood_sub$non_flood       <- as.factor(flood_sub$non_flood)
## flood_sub$flood_prone     <- as.factor(flood_sub$flood_prone)
## flood_sub$non_flood_prone <- as.factor(flood_sub$non_flood_prone)

## flood_sub_time_window <- c(start_date, end_date)

## Remove temporary variables #
rm(days, start_date, end_date, years)


####################################################################################################################################################

flood_data_subsets <- list("IF" = flood_sub)

## F_NF   <- subset(flood_sub, flood == 1 | non_flood == 1)
## NF_NFP <- subset(flood_sub, flood == 0)
## F_NFP  <- subset(flood_sub, non_flood == 0)
## FP_NFP <- flood_sub
