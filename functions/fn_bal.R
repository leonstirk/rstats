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