rm(list=ls(all=TRUE))

suppressPackageStartupMessages({
    ##    require(geosphere)
    require(data.table)
    require(MatchIt)
    require(cem)
    ## require(zeligverse)
    require(randomForest)
    require(plyr)
    require(dplyr)
    require(ggplot2)
    require(reshape2)
    require(effects)
    require(scales)
    require(huxtable)
    require(broom)
    require(sm)
    require(pwr)
})

#####################
## Import functions #
#####################

source('functions/density_compare.R')
source('functions/multiplot.R')
source('functions/random_functions.R') ## inherited arguments issue need to be sorted out
source('functions/match_samples.R')
source('functions/set_strings.R')

##############
## Load data #
##############

das <- readRDS("datasets/dud_allsales_RDS.rds")

## Set ln_sale_price to use nominal or inflation adjusted log sale price #
names(das)[names(das) == "ln_net_sale_price"] <- "ln_sale_price" # OR set "ln_real_net_sale_price" instead of "ln_net_sale_price"

## Define the variable types
c <- c("sale_id", "qpid", "physical_address")
i <- c("good_land_view", "good_water_view", "year_built")
n <- c("ln_sale_price", "building_floor_area", "land_area", "median_income", "homeowner_rate", "dist_cbd")
f <- c("meshblock_id", "area_unit_name", "bedrooms", "bathrooms", "period_built", "contour", "sale_year", "deck", "offstreet_parking")

das[c] <- lapply(das[c],as.character)
das[i] <- lapply(das[i],as.integer)
das[n] <- lapply(das[n],as.numeric)
das[f] <- lapply(das[f],as.factor)

das$sale_date_1 <- as.numeric(das$sale_date)

## ## create sale_month variable in ordered or unordered factor format
## das[order(das$sale_date), c('sale_month')] <- factor(format(das[order(das$sale_date),c('sale_date')], "%Y-%m"), ordered = TRUE)
## das[order(das$sale_date), c('sale_quarter')] <- factor(format(das[order(das$sale_date),c('sale_date')], "%Y-%m"))

das$sale_quarter <- as.factor(das$sale_quarter)

## Relevel factors
das$bedrooms <- relevel(das$bedrooms, ref = '3')

## tmp <- levels(das$property_ownership_type)
## das$property_ownership_type <- mapvalues(das$property_ownership_type, from = tmp, to = c("core_crown", "crown", "local_authority","private_company","private_individual"))

tmp <- levels(das$contour)
das$contour <- mapvalues(das$contour, from = tmp, to = c("easy_moderate","level","steep"))

tmp <- levels(das$period_built)
das$period_built <- mapvalues(das$period_built, from = tmp, to = c("1900to70s","1800s","80s90s","post2000"))

rm(tmp)

rm(c, i, n , f)

########################################################################################################################################################

############################
## Define variable vectors #
############################

## Descriptive variables #
des_vars <- c("ln_sale_price",
              "building_floor_area",
              "land_area",
              "median_income",
              "homeowner_rate",
              "bedrooms",
              "bathrooms",
              "offstreet_parking",
              "deck",
              "good_land_view",
              "good_water_view",
              "year_built",
              "period_built",
              "contour",
              "sale_year",
              "dist_cbd"
              )

ht <- hux(
    "Variable name" = des_vars,
    Description = c("Natural log of the net sale price",
                    "Total floor area of all buildings ('00' sqm)",
                    "Total land area (hectares)",
                    "Median income of the meshblock in which the observation is located ('0000' dollars)",
                    "Proportion of owner-occupied housing in the meshblock",
                    "Number of bedrooms",
                    "Number of bathrooms",
                    "Dummy variable indicating whether the property has offstreet parking",
                    "Dummy variable indicating whether the property has a deck",
                    "Generated dummy variable indicating the property has a 'good', non-water view",
                    "Generated dummy variable indicating the property has a 'good', water view",
                    "Year that the primary dwelling was constructed",
                    "Varible derived from 'year_built', coarsening the varible into four categories (1800's, 1900-70s, 80's-90's, post 2000's)",
                    "Variable with three categories assesing the overall property contour (level, easy-moderate, steep)",
                    "Year of sale",
                    "Straight line distance from the centre of the Octagon (km's)"
                    ),
    add_colnames = TRUE
)

bottom_border(ht)[1,]  <- 2
right_padding(ht)      <- 10
left_padding(ht)       <- 10
width(ht)              <- 1
col_width(ht)          <- c(0.3, 0.7)
valign(ht)             <- 'bottom'
des_vars_summary <- map_wrap(ht, by_cols(FALSE, TRUE))
rm(ht)

########################################################################################################################################################

#########################
## Handle missing data ##
#########################

## Find missing data in raw
missing_das <- das[!complete.cases(das[des_vars]),]

## Summarise missing casees
ht <- hux(
    Data = c('Raw data', 'Incomplete cases', 'Complete cases'),
    N = c(nrow(das), nrow(missing_das), nrow(das)-nrow(missing_das)),
    add_colnames = TRUE
)

bottom_border(ht)[1,]  <- 2
align(ht)[,2]          <- 'right'
right_padding(ht)      <- 10
left_padding(ht)       <- 10
number_format(ht)      <- 0
width(ht)              <- 1
col_width(ht)          <- c(0.5, 0.5)
missing_das_summary    <- map_wrap(ht, by_cols(FALSE))
rm(ht)

## Continue with only complete cases
das <- das[complete.cases(das),]

## remove factor variables from descriptive vars
des_vars <- des_vars[!(des_vars %in% c("year_built","period_built","contour","sale_year"))]

##########################################################

## ## Add variables
## ## Arterial road dummy
## arterial_road_vec <- c("Andersons Bay Road","Bay View Road","Castle Street","Corstophine Road","Cumberland Street","Eglinton Road","Forbury Road","George Street","Great King Street","Highgate","High Street","Hillside Road","Kaikorai Valley Road","Kenmure Road","King Edward Street","Macandrew Road","Mailer Street","Main South Road","Maitland Street","Malvern Street","Musselburgh Rise","North Road","Opoho Road","Pine Hill Road","Prince Albert Road","Queens Drive","South Road","Stevenson Road","Victoria Road")
## das$arterial_street <- ifelse(das$full_roa %in% arterial_road_vec,1,0)

## rm(arterial_road_vec)

## Scale conversions
das$median_income <- das$median_income/10000
das$building_floor_area <- das$building_floor_area/100
das$land_area <- das$land_area/10000

## Set flood_date #
flood_date <- as.Date('2015-06-04')
das$after_flood <- as.factor(ifelse(das$sale_date < flood_date,0,1))

sampleDescriptivesTable <- function(das) {
 ht <- hux(sampleDescriptives(das, des_vars, 4), add_colnames = TRUE, add_rownames = TRUE)

 bottom_border(ht)[1,]  <- 2
 align(ht)[,2:6]          <- 'right'
 right_padding(ht)      <- 10
 left_padding(ht)       <- 10
 number_format(ht)      <- 4
 width(ht)              <- 1
 col_width(ht)          <- c(2/7, 1/7, 1/7, 1/7, 1/7, 1/7)
 ht[1,]                <- c("Variable name", "Mean", "Median", "Std. Dev", "Min", "Max")
 return(ht)
}

whole_sample_descriptives <- sampleDescriptivesTable(das)
