## for propensity score matching diagnosic output
## plots the treated vs untreated values of a variable against the propensity score
## i.e. checks to see if, post-matching, for a given propensity score the  mean value of the covariate for the treated group is equal to the untreated group
## See examples of applications here: https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html

fn_bal <- function(dta, var) { dta$var <- dta[,var] 
  dta$treatment <- as.factor(dta$treatment) 
  support <- c(min(dta$var), max(dta$var)) 
  ggplot(dta, aes(x = distance, y = var, color = treatment)) + 
    geom_point(alpha = 0.2, size = 1.3) + 
    geom_smooth(method = "loess", se = F) + 
    xlab("Propensity score") + 
    ylab(var) + 
    theme_bw() + 
    ylim(support)
}