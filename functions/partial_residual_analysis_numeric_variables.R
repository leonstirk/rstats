partial_resid_numeric_analysis <- function(vars, fit, data) {
  data <<- data

  fit <<- fit

  ## Terms matrix
  a_terms <<- as.data.frame(predict(fit, type = "terms"))

  ## Partials matrix #
  a_partial_residuals <<- as.data.frame(apply(a_terms, 2, function(x) { (data$ln_sale_price - (fit$fitted.values - mean(data$ln_sale_price))) + x }))

  ## Effects output #
  l_eff <- list()
  for(i in 1:length(vars)) {
    l_eff[[vars[i]]]            <- effect(vars[i], fit, partial.residuals = TRUE)
    l_eff[[vars[i]]]$terms      <- a_terms[,c(vars[i])]
    l_eff[[vars[i]]]$partials_y <- a_partial_residuals[,c(vars[i])]
    l_eff[[vars[i]]]$sale_id    <- data$sale_id
  }

  l_eff <<- l_eff

  data <- data.frame()
  return(l_eff)
}
