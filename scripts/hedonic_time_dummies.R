source('scripts/das_data_preprocessing.R')

a <- das_caversham

year.f <- factor(a$sale_year)
dummies = model.matrix(~year.f)
dumm_var_names <- tail(colnames(dummies),-1)
b <- data.frame(dummies, a)

model_lhs_year_dumm <- paste(dumm_var_names, collapse = " + ")

model_lhs_vars <- paste(tail(das_vars,-1), collapse = " + ")

modelstring <- paste(das_vars[1],"~",model_lhs_year_dumm," + ",model_lhs_vars)

# modelstring <- paste(das_vars[1],"~",model_lhs_vars)

fit <- lm(modelstring, data=b)
# fit <- lm(modelstring, data=a)

summary(fit) # show results

# x <- cor(b[das_vars])
# symnum(x)

# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)