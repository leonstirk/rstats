

#############################################################################
## Currently abandoned due to rank deficiency problem potentially making   ##
## this index totally fucking unusable in samples of even a moderate size. ##
#############################################################################


source('scripts/das_data_preprocessing.R')

getWeightedTransactionIndexSeries <- function(data, model) {
 return(lm(model, data=data))
}

model_lhs_vars <- paste(tail(das_vars,-1), collapse = " + ")
modelstring <- paste(das_vars[1],"~",model_lhs_vars)

year_vector <- levels(as.factor(das[,'sale_year']))

weighted_transaction_index_by_au <- c(levels(as.factor(das[,'sale_year'])))

for(au in au_names) {
 subset <- subsetByVar(das, 'area_unit_name', au)
 subset <- eliminateSingleLevelFactors(subset)

 base <- subset[which(subset$sale_year == year_vector[1]),]
 # This is a pretty big deal b/c the base period sample has singular vector in one of the variables but does not have that issue in subsequent period samples
 # base <- eliminateSingleLevelFactors(base)

 fit <- getWeightedTransactionIndexSeries(base, modelstring)

 ## Produce predicted values on base year preferences
 y_hat <- as.vector(predict(fit, subset))
 subset_predicted <- data.frame(subset, y_hat)

 col <- getArithmeticMeanIndexSeries(subset_predicted, "ln_sale_price", "sale_year")
 weighted_transaction_index_by_au <- cbind(weighted_transaction_index_by_au, col)
}


weighted_transaction_index_by_au <- tail(data.frame(weighted_transaction_index_by_au),-1)
names(weighted_transaction_index_by_au) <- c('year', au_names)

weighted_transaction_index_by_au <- melt(weighted_transaction_index_by_au, id.vars=c('year'))
weighted_transaction_index_by_au$value <- as.numeric(weighted_transaction_index_by_au$value)

iplot <- ggplot(data=weighted_transaction_index_by_au, aes(x=year, y=value, color=variable)) + geom_line(aes(group = variable))

names(weighted_transaction_index_by_au) <- c('year', 'area_unit_name', 'index')


# ## Test for matrix invertability/singularity
# ###### I had some probelms with this I think I need to force the variables to numeric rather than factor i.e. dummy out the factors manually and and have dummy vars as class == numeric.
# isMatrixInvertable <- function(m, v) class(try(solve(m[v]),silent=T))=="matrix"

## Show results
summary(fit)










## Correlation tables
# x <- cor(subset[das_vars])
# symnum(x)

## Other useful functions
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics

## Diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)
