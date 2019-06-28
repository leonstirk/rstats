pool_point_data <- function(u_data, m_data, var_list) {

  u_data$matched <- rep("unmatched", length(u_data$sale_id))
  m_data$matched <- rep("matched", length(m_data$sale_id))

  names <- var_list
  l <- list(u_data, m_data)
  l <- lapply(l, function(x) {
    df <- data.frame(x[names])
    names(df) <- names
    return(df)
  })

  data <- l

  pruned_obs_ids <- setdiff(u_data$sale_id, m_data$sale_id)
  pruned_data <- data[[1]][which(data[[1]]$sale_id %in% pruned_obs_ids),]
  pruned_data$matched <- 'pruned'
  
  data <- list("unmatched_data" = data[[1]], "matched_data" = data[[2]], "pruned_data" = pruned_data)

  return(data)
}

pool_fit_data <- function(u_data, m_data) {
  u <- as.data.frame(u_data)
  m <- as.data.frame(m_data)
  u$matched <- "unmatched"
  m$matched <- "matched"
  data <- list("unmatched_data" = u, "matched_data" = m)
  return(data)
}


plot_data <- function(reg_result_obj, var) {

  x <- reg_result_obj[['model_partials']]

  u_data <- x[['unmatched_data']][[var]]
  m_data <- x[['matched_data']][[var]]

  fit_data   <- pool_fit_data(u_data, m_data)
  point_data <- pool_point_data(u_data, m_data, c("sale_id", "x.all", "partials_y", "matched"))

  return(list("fit_data" = fit_data, "point_data" = point_data))
}


###############################################################################################




partial_comparison_plot_factor <- function(reg_result_obj, var) {

  data <- plot_data(reg_result_obj, var)

  pch = 4 ## 20, 4, '.'
  cex = 0.35

  df <- do.call("rbind", data[["fit_data"]])

  ggplot(df, aes(x = df[var][,1], y = fit, color=matched)) + geom_point(position = position_dodge(width = 0.2)) + geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.2, position = 'dodge') + labs(title ="New title", x = var, y = "ln_sale_price")

  # ggplot(plot_data, aes(as.factor(x), partials_y, fill=matched)) + geom_errorbar()
  # ggplot(data[["fit_data"]][["matched_data"]], aes(as.factor(var), partials_y, color = 'black')) + geom_errorbar(data = data[["fit_data"]][["matched_data"]], aes(ymin=fit-se, ymax=fit+se), width=0.4) + theme_bw(base_size=12)
  # plot(plot_data$x, plot_data$partials_y, pch = pch, cex = cex)
  
}


partial_comparison_plot_scale <- function(reg_result_obj, var, var_name) {

  data <- plot_data(reg_result_obj, var)

  pch = '.' ## 20, 4, '.'
  cex = 0.35
  spar = 1
  xlim = c(min(data[["point_data"]][["unmatched_data"]]$x.all), max(data[["point_data"]][["unmatched_data"]]$x.all))
  trim_xlim = c(min(data[["point_data"]][["unmatched_data"]]$x.all), quantile(data[["point_data"]][["unmatched_data"]]$x.all, c(.995)))

  ## Plot the matched partial residuals (BLACK) #
  with(data[["point_data"]][["matched_data"]], plot(x.all, partials_y, pch = 4, cex = cex, col = 'black',
    main="title",
    sub="subtitle",
    xlab=var_name,
    ylab="ln_sale_price",
    xlim = xlim
  ))

  # ## Plot the pruned partial residuals (RED) #
  # with(data[["point_data"]][["pruned_data"]], points(x.all, partials_y, pch = pch, cex = cex, col = 'red'))

  ## Plot the unmatched partial residuals (BLUE) #
  with(data[["point_data"]][["unmatched_data"]], points(x.all, partials_y, pch = pch, cex = cex, col = 'blue'))

  ## Plot the OLS and spline regression lines for the UNMATCHED data (BLUE) #
  lines(data[["fit_data"]][["unmatched_data"]][var_name][,1], data[["fit_data"]][["unmatched_data"]]$fit, col = 'blue')
  lines(smooth.spline(data[["point_data"]][["unmatched_data"]]$x.all, data[["point_data"]][["unmatched_data"]]$partials_y, spar=spar), lty = 2, col = 'blue')

  ## Plot the OLS and spline regression lines for the MATCHED data (BLACK) #
  lines(data[["fit_data"]][["matched_data"]][var_name][,1], data[["fit_data"]][["matched_data"]]$fit, col = 'black')
  lines(smooth.spline(data[["point_data"]][["matched_data"]]$x.all, data[["point_data"]][["matched_data"]]$partials_y, spar=spar), lty = 2, col = 'black')

  ## Plot the spline regression line for the PRUNED data (RED) #
  lines(smooth.spline(data[["point_data"]][["pruned_data"]]$x.all, data[["point_data"]][["pruned_data"]]$partials_y, spar=spar), lty = 2, col = 'red')

}


# function(u_obj, m_obj, x1, x2, x1_x2) {
  
# }


## layout(matrix(seq(1,4,1),2,2))
layout(1)

## partial_comparison_plot(results[["F_NF"]][["model_partials"]][["unmatched_data"]][["poly(building_floor_area, 2)"]], results[["F_NF"]][["model_partials"]][["matched_data"]][["poly(building_floor_area, 2)"]])