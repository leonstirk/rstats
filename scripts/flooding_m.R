source('scripts/flooding_data_processing.R')

before_flood <- subset(flood_sub, after_flood == 0)
after_flood <- subset(flood_sub, after_flood ==1)

a_sub <- before_flood

###################################################
## Define covariate space for pre-matched samples #
###################################################
a_sub_cov<-tail(das_vars,-1)

###############################################################
## Omit any observations with missing values in the variables #
###############################################################
a_sub_nomiss <- a_sub %>% dplyr::select(qpid, sale_id, sale_year, ln_sale_price, flooded, one_of(a_sub_cov)) %>% na.omit()

########################
# Create model strings #
########################

model_lhs_vars <- paste(a_sub_cov, collapse = " + ")
model_formula <- as.formula(paste("flooded ~ ",model_lhs_vars))

################################
## Nearest match (mahalanobis) #
################################ 
m_out <- matchit(model_formula, distance = "mahalanobis", method = "nearest", data = a_sub_nomiss)

#####################
## Matched data set #
#####################
m_data <- match.data(m_out)

####################
## Balance summary #
####################
b_sum <- summary(m_out)
b_sum_std <- summary(m_out, standardize = TRUE)
b_plot <- plot(m_out)
b_plot_sum <- plot(b_sum_std)

###################
## Zelig analysis #
###################
# z_out <- zelig(ln_sale_price ~ treatment + bedrooms + bathrooms + ln_building_floor_area + ln_land_area, model = "ls", data = m_data)
# c_out <- setx(z_out, treatment = 0)
# t_out <- setx(z_out, treatment = 1)
# s_out <- sim(z_out, c_out, t_out)

