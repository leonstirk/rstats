source('scripts/das_data_preprocessing.R')

year.f <- factor(das$sale_year)
dummies = model.matrix(~year.f)
dumm_var_names <- tail(colnames(dummies),-1)
das_year_dummies <- data.frame(dummies, das)

model_lhs_year_dumm <- paste(dumm_var_names, collapse = " + ")
model_lhs_vars <- paste(tail(das_vars,-1), collapse = " + ")
modelstring <- paste(das_vars[1],"~",model_lhs_year_dumm," + ",model_lhs_vars)

getDummyHedonicIndexSeries <- function(data_subset, modelstring) {
 fit <- lm(modelstring, data=data_subset)
 series <- c(0,as.vector(fit$coefficients[2:16]))
 series <- series+1
 return(series)
}

dummy_hedonic_index_by_au <- c('year', levels(as.factor(das[,'sale_year'])))

for(au in au_names) {
 subset <- subsetByVar(das_year_dummies, 'area_unit_name', au)
 subset <- eliminateSingleLevelFactors(subset)

 col <- c(au, getDummyHedonicIndexSeries(subset, modelstring))
 dummy_hedonic_index_by_au <- cbind(dummy_hedonic_index_by_au, col)
}

dummy_hedonic_index_by_au <- tail(data.frame(dummy_hedonic_index_by_au),-1)
names(dummy_hedonic_index_by_au) <- c('year', au_names)

dummy_hedonic_index_by_au <- melt(dummy_hedonic_index_by_au, id.vars=c('year'))
dummy_hedonic_index_by_au$value <- as.numeric(dummy_hedonic_index_by_au$value)

iplot <- ggplot(data=dummy_hedonic_index_by_au, aes(x=year, y=value, color=variable)) + geom_line(aes(group = variable))

names(dummy_hedonic_index_by_au) <- c('year', 'area_unit_name', 'index')

## Show results
# summary(fit)

## Correlation tables
# x <- cor(b[das_vars])
# symnum(x)

## Other useful functions 
# coefficients(fit) # model coefficients
# confint(fit, level=0.95) # CIs for model parameters 
# fitted(fit) # predicted values
# residuals(fit) # residuals
# anova(fit) # anova table 
# vcov(fit) # covariance matrix for model parameters 
# influence(fit) # regression diagnostics

## diagnostic plots 
# layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
# plot(fit)

