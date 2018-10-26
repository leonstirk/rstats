source('scripts/das_data_preprocessing.R')

library('sm')

a <- das_concord
# for(au in au_names) {
#  a <- subsetByVar(das, "area_unit_name", au)
# }

mean_summary <- a %>% group_by(sale_year) %>% dplyr::summarise(n_sale_year = n(), mean_ln_sale_price = mean(ln_sale_price), std_error = sd(ln_sale_price)/sqrt(n_sale_year))

t_vec <- as.numeric(levels(as.factor(a$sale_year)))
index_table <- c(levels(as.factor(das[,'sale_year'])))

#############################
## Loop over each base year #
#############################
for(t_C in t_vec) {

index <- vector()

 #############################
 ## Loop over each year pair #
 #############################
 for (t_T in t_vec) {

 ################################
 ## Only match where t_C != t_T #
 ################################

 if(t_C == t_T) { diff <- 0 }
 if(t_C != t_T) {

   ##########################################################
   ## Subset regional data subset to the relevant year pair #
   ##########################################################
   a_sub <- subset(a, a$sale_year == t_C | a$sale_year == t_T) # %>% eliminateSingleLevelFactors()
   a_sub$treatment <- ifelse(a_sub$sale_year == t_C,0,1)

   ###############################
   ## Generate propensity scores #
   ###############################

   m_ps <- glm(match_model_formula, family = binomial(), data = a_sub)
   prs_df <- data.frame(pr_score = predict(m_ps, type = "response"), treatment = m_ps$model$treatment)

   #####################################################################################################
   ## Calculate caliper as 0.25*\sigma = [(\sigma_1^2 + sigma_0^2)/2]^0.5 as per Rosenbaum and Rubin (1985) #
   #####################################################################################################
   caliper <- 0.25*((var(prs_df[which(prs_df$treatment == 0 ),]$pr_score) + var(prs_df[which(prs_df$treatment == 1 ),]$pr_score))/2)^0.5

   ###############################################################
   ## Omit any observations with missing values in the variables #
   ###############################################################
   a_sub_nomiss <- a_sub %>% dplyr::select(qpid, sale_id, sale_year, ln_sale_price, treatment, one_of(tail(das_vars,-1))) %>% na.omit()

   ################################
   ## Nearest match (mahalanobis) #
   ################################
   m_out <- matchit(match_model_formula, distance = "logit", method = "nearest", caliper = caliper, data = a_sub_nomiss, mahvars = mah_vars)
     
   #####################
   ## Matched data set #
   #####################
   m_data <- match.data(m_out)

   ####################
   ## Balance summary #
   ####################
   # b_sum <- summary(m_out)
   # b_sum_std <- summary(m_out, standardized = TRUE)
   # b_plot <- plot(m_out)
   # b_plot_sum <- plot(b_sum_std)

   ###################
   ## Zelig analysis #
   ###################
   # z_out <- zelig(ln_sale_price ~ treatment + bedrooms + bathrooms + ln_building_floor_area + ln_land_area, model = "ls", data = m_data)
   # c_out <- setx(z_out, treatment = 0)
   # t_out <- setx(z_out, treatment = 1)
   # s_out <- sim(z_out, c_out, t_out)

   #####################################################################################################################################
   ## Find mean difference in matches (treatment variables are given by the "rownames" and control varibles are given by the "values") #
   #####################################################################################################################################
   matched_price_pairs <- data.frame(m_data[m_out$match.matrix,c("sale_year","ln_sale_price","treatment")],m_data[rownames(m_out$match.matrix),c("sale_year","ln_sale_price","treatment")])
     
   ifelse(t_C < t_T, names(matched_price_pairs) <- c('sale_year_t0','ln_sale_price_t0','treatment_t0','sale_year_t1','ln_sale_price_t1','treatment_t1'), names(matched_price_pairs) <- c('sale_year_t1','ln_sale_price_t1','treatment_t1','sale_year_t0','ln_sale_price_t0','treatment_t0'))
     
   names(matched_price_pairs) <- c('sale_year_t0','ln_sale_price_t0','treatment_t0','sale_year_t1','ln_sale_price_t1','treatment_t1')
     
   matched_price_pairs$diff <- matched_price_pairs$ln_sale_price_t1 - matched_price_pairs$ln_sale_price_t0
     
   diff <- mean(matched_price_pairs$diff, na.rm = TRUE)

   #######################################################################
   ## Visual check of propensity score based distributional matching      #
   ## Kernel density estimates of propensity scores by treatment (t_C, t_T) #
   #######################################################################
   # sm.density.compare(prs_df$pr_score, prs_df$treatment)

   }
   index <- c(index, diff)
  }

####################
## Normalise index #
####################
index <- index - index[1]

#############################
## Add index to index table #
#############################
index_table <- cbind(index_table, exp(index)*100)

}

index_table <- data.frame(index_table)
names(index_table) <- c('year', levels(as.factor(das[,'sale_year'])))
# names(index_table) <- c('year', "2000")

index_table <- melt(index_table, id.vars=c('year'))
index_table$value <- as.numeric(index_table$value)

iplot <- ggplot(data=index_table, aes(x=year, y=value, color=variable)) + geom_line(aes(group = variable))

jplot <- ggplot(data=mean_summary, aes(x=sale_year, y=n_sale_year)) + geom_line()

# matches <- data.frame(a_sub[rownames(m_out$match.matrix),'physical_address'],a_sub[m_out$match.matrix,'physical_address'])
# names(matches) <- c('treatment', 'control')
