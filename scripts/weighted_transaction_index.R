source('scripts/das_data_preprocessing.R')

a <- das_caversham

model_lhs_vars <- paste(tail(das_vars,-1), collapse = " + ")
modelstring <- paste(das_vars[1],"~"," + ",model_lhs_vars)

year_vector <- levels(as.factor(das[,'sale_year']))

subset <- a[which(a$sale_year == year_vector[1]),]
# subset <- a

fit <- lm(modelstring, data=subset)

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
