setStrings <- function(data_vars, resp_var, treat_var, inf_var, event_var, mah_vars, exact_vars, model_vars, poly2_vars, poly3_vars, model) {
    data_vars     <<- data_vars

    resp_var      <<- resp_var
    event_var     <<- event_var
    treat_var     <<- treat_var
    inf_var       <<- inf_var

    mah_vars      <<- mah_vars
    exact_vars    <<- exact_vars
    match_vars    <<- c(mah_vars, exact_vars) ## for imbalance checking

    model_vars    <<- model_vars ## variables in modelling that are not the response or treatment variable
    poly2_vars    <<- poly2_vars
    poly3_vars    <<- poly3_vars

    int_model_var <<- paste(c(treat_var, event_var), collapse = " * ")
    int_var       <<- paste(c(treat_var, event_var), collapse = ":")

    trip_int_model_var <<- paste(c(treat_var, inf_var, event_var), collapse = " * ")
    trip_int_var       <<- paste(c(treat_var, inf_var, event_var), collapse = ":")

    ## create strings
    poly2_strings <<- unlist(sapply(poly2_vars, function(x) { paste(c("poly(", x, ", 2, raw = TRUE)"), collapse = "") }))
    poly3_strings <<- unlist(sapply(poly3_vars, function(x) { paste(c("poly(", x, ", 3, raw = TRUE)"), collapse = "") }))
    lm_model_strings <<- model_vars
    names(lm_model_strings) <<- model_vars
    for(i in names(poly2_strings)) { lm_model_strings[i] <<- poly2_strings[i] }
    for(i in names(poly3_strings)) { lm_model_strings[i] <<- poly3_strings[i] }

    ## mah match formula
    mah_match_formula <<- as.formula(paste(treat_var, " ~ ", paste(mah_vars, collapse = " + ")))

    model_formula_list <<- list()
    partial_strings_list <<- list()

    for(i in 0:length(lm_model_strings)) {
        if(model == 'D') {
            lm_model_formula <<- as.formula(paste(resp_var, " ~ ", paste(c(treat_var, lm_model_strings[1:i]), collapse = " + ")))
            partial_strings  <<- c(treat_var, lm_model_strings[1:i])
            if(i == 0) {
                lm_model_formula <<- as.formula(paste(resp_var, " ~ ", paste(c(treat_var), collapse = " + ")))
                partial_strings  <<- c(treat_var)
            }
            model_formula_list[[i+1]]   <<- lm_model_formula
            partial_strings_list[[i+1]] <<- partial_strings

            ## rf_model_formula  <<- as.formula(paste(resp_evar, " ~ ", paste(c(treat_var, model_vars[1:i]), collapse = " + ")))
        }
        if(model == 'DiD') {
            lm_model_formula <<- as.formula(paste(resp_var, " ~ ", paste(c(int_model_var, lm_model_strings[1:i]), collapse = " + ")))
            partial_strings  <<- c(int_var, lm_model_strings[1:i])
            if(i == 0) {
                lm_model_formula <<- as.formula(paste(resp_var, " ~ ", paste(c(int_model_var), collapse = " + ")))
                partial_strings  <<- c(int_var)
            }
            model_formula_list[[i+1]]   <<- lm_model_formula
            partial_strings_list[[i+1]] <<- partial_strings

            ## rf_model_formula  <<- as.formula(paste(resp_var, " ~ ", paste(c(int_model_var, model_vars[1:i]), collapse = " + ")))
        }
        if(model == 'DiDiD') {
            lm_model_formula <<- as.formula(paste(resp_var, " ~ ", paste(c(trip_int_model_var, lm_model_strings[1:i]), collapse = " + ")))
            partial_strings  <<- c(trip_int_var, lm_model_strings[1:i])
            if(i == 0) {
                lm_model_formula <<- as.formula(paste(resp_var, " ~ ", paste(c(trip_int_model_var), collapse = " + ")))
                partial_strings  <<- c(trip_int_var)
            }
            model_formula_list[[i+1]]   <<- lm_model_formula
            partial_strings_list[[i+1]] <<- partial_strings

            ## rf_model_formula  <<- as.formula(paste(resp_var, " ~ ", paste(c(int_model_var, model_vars[1:i]), collapse = " + ")))
        }

        names(partial_strings)[1] <<- treat_var
    }
}

strings <- list(
    ## data_vars
    data_vars = c(
        "sale_id",
        "ln_sale_price",
        "flood_prone",
        "non_flood_prone",
        "flood",
        "non_flood",
        "after_flood",
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
        "sale_date_1",
        "sale_year",
        "dist_cbd"
    ),
    ## resp_ var
    resp_var = "ln_sale_price",
    ## treat_var
    treat_var = "flood_prone",
    ## information_var
    inf_var = "flood",
    ## event_var
    event_var = "after_flood",
    ## mah_vars
    mah_vars = c(
        "building_floor_area"
       ,"land_area"
       ,"median_income"
       ,"homeowner_rate"
       ,"sale_date_1"
    ),
    ## exact_vars
    exact_vars = c(
        ## "after_flood",
        "bedrooms",
        "bathrooms",
        "offstreet_parking",
        "deck",
        "good_land_view",
        "good_water_view",
        "period_built",
        "contour"
        ## "sale_year"
    ),
    ## model_vars
    model_vars = c(
        "sale_year"

       ,"building_floor_area"
       ,"land_area"

       ,"offstreet_parking"
       ,"deck"
       ,"good_land_view"
       ,"good_water_view"
        ## ,"contour"
       ,"bathrooms"

       ,"median_income"
       ,"homeowner_rate"
       ,"dist_cbd"

        ## ,"bedrooms"

        ## ,"period_built"
    ),
    ## poly2_vars
    poly2_vars = c(
        "building_floor_area"
       ,"land_area"
       ,"homeowner_rate"
    ),
    ## poly3_vars
    poly3_vars = c(
        "dist_cbd"
        ## ,"land_area"
    )
)

setBreaks <- function(data) {
    dates <- seq(min(flood_data_subsets[["IF"]]$sale_date_1), max(flood_data_subsets[["IF"]]$sale_date_1), (max(flood_data_subsets[["IF"]]$sale_date_1) - min(flood_data_subsets[["IF"]]$sale_date_1))/6 )
    cutpoints <- list(
        building_floor_area = c(75,105),
        land_area = c(290,420),
        sale_date_1 = c(dates),
        offstreet_parking = c(0.5),
        deck = c(0.5),
        good_land_view = c(0.5),
        good_water_view = c(0.5)
    )

    grouping <- list(
        bedrooms = list(c("1"),c("2"),c("3"),c("4","5","6","7","8")),
        bathrooms = list(c("1"),c("2","3","4","5","6")),
        period_built = as.list(levels(data$period_built)),
        contour = as.list(levels(data$contour))
        ## sale_year = as.list(levels(data$sale_year))
    )
    return(list("cutpoints" = cutpoints, "grouping" = grouping))
}

## coefs <- c(
##     '(Intercept)',
##     'flood' = 'flood1',
##     'flood_prone' = 'flood_prone1',
##     'after_flood' = 'after_flood1',
##     'flood_prone:after_flood' = 'flood_prone1:after_flood1',
##     'flood:flood_prone:after_flood' = 'flood1:after_flood1'
## )


###########
## DiD

coefs <- c(
    '(Intercept)',
    'flood_prone:after_flood' = 'flood_prone1:after_flood1',
    'flood_prone' = 'flood_prone1',
    'after_flood' = 'after_flood1',
    'sale_year2012',
    'sale_year2013',
    'sale_year2014',
    'sale_year2015',
    'sale_year2016',
    'sale_year2017',
    'sale_year2018',
    'building_floor_area' = 'poly(building_floor_area, 2, raw = TRUE)1',
    'building_floor_area\\textsuperscript{2}' = 'poly(building_floor_area, 2, raw = TRUE)2',
    'land_area' = 'poly(land_area, 2, raw = TRUE)1',
    'land_area \\textsuperscript{2}' = 'poly(land_area, 2, raw = TRUE)2',
    'offstreet_parking' = 'offstreet_parking1',
    'deck' = 'deck1',
    'good_land_view',
    'good_water_view',
    'bathrooms2',
    'bathrooms3',
    'bathrooms4',
    'median_income',
    'homeowner_rate' = 'poly(homeowner_rate, 2, raw = TRUE)1',
    'homeowner_rate\\textsuperscript{2}' = 'poly(homeowner_rate, 2, raw = TRUE)2',
    'dist_cbd' = 'poly(dist_cbd, 3, raw = TRUE)1',
    'dist_cbd \\textsuperscript{2}' = 'poly(dist_cbd, 3, raw = TRUE)2',
    'dist_cbd \\textsuperscript{3}' = 'poly(dist_cbd, 3, raw = TRUE)3'
)

#############
## D

## coefs <- c(
##     '(Intercept)',
##     'flood_prone' = 'flood_prone1',
##     'sale_year2012',
##     'sale_year2013',
##     'sale_year2014',
##     'sale_year2015',
##     ## 'period_built1800s',
##     ## 'period_built80s90s',
##     ## 'period_builtpost2000',
##     'building_floor_area' = 'poly(building_floor_area, 2, raw = TRUE)1',
##     'building_floor_area\\textsuperscript{2}' = 'poly(building_floor_area, 2, raw = TRUE)2',
##     ## 'bedrooms1',
##     ## 'bedrooms2',
##     ## 'bedrooms4',
##     ## 'bedrooms5',
##     ## 'bedrooms6',
##     ## 'bedrooms7',
##     'land_area' = 'poly(land_area, 2, raw = TRUE)1',
##     'land_area \\textsuperscript{2}' = 'poly(land_area, 2, raw = TRUE)2',
##     'offstreet_parking1',
##     'deck1',
##     'good_land_view',
##     'good_water_view',
##     'bathrooms2',
##     'bathrooms3',
##     'bathrooms4',
##     'median_income',
##     'homeowner_rate' = 'poly(homeowner_rate, 2, raw = TRUE)1',
##     'homeowner_rate\\textsuperscript{2}' = 'poly(homeowner_rate, 2, raw = TRUE)2',
##     'dist_cbd' = 'poly(dist_cbd, 3, raw = TRUE)1',
##     'dist_cbd \\textsuperscript{2}' = 'poly(dist_cbd, 3, raw = TRUE)2',
##     'dist_cbd \\textsuperscript{3}' = 'poly(dist_cbd, 3, raw = TRUE)3'
## )
