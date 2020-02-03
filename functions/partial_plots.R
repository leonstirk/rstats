pool_data <- function(p_list, method_list, var, var_list) {
    fit_data <- lapply(method_list, function(m) {
        tmp <- as.data.frame(p_list[[m]][[var]])
        tmp$method <- m
        return(tmp)
    })
    names(fit_data) <- unlist(method_list)


    point_data <- lapply(method_list, function(m) {
        p_list[[m]][[var]]$method <- rep(m, length(p_list[[m]][[var]]$sale_id))
        p_list[[m]][[var]]$pruned <- 0
        tmp <- data.frame(p_list[[m]][[var]][var_list])
        return(tmp)
    })
    names(point_data) <- unlist(method_list)


    pruned_obs_ids_list <- lapply(method_list[2:length(method_list)], function(x) {
        do.call(setdiff, args = list(point_data[['raw']]$sale_id, point_data[[x]]$sale_id))
    })
    names(pruned_obs_ids_list) <- unlist(method_list)[2:length(method_list)]


    pruned_data_list <- lapply(method_list[2:length(method_list)], function(x) {
        pruned_data <- point_data[['raw']][which(point_data[['raw']]$sale_id %in% pruned_obs_ids_list[[x]]),]
        pruned_data$method <- rep(x, nrow(pruned_data))
        pruned_data$pruned <- rep(1, nrow(pruned_data))
        return(pruned_data)
    })
    names(pruned_data_list) <- unlist(method_list)[2:length(method_list)]

    point_data <- c(point_data, pruned_data_list)

    return(list("fit_data" = fit_data, "point_data" = point_data))
}

################################################################################################################

partialComparisonPlotTripleInteraction <- function(p_list, method_list, x_axis, int_var, inf_var, x1_x2_x3) {

    facet  <- as.formula(paste(c('~', int_var, inf_var)))

    data <- pool_data(p_list, method_list, x1_x2_x3, c("sale_id", "x.all", "partials_y", "method", "pruned"))

    pch       <- 4 ## 20, 4, '.'
    cex       <- 0.35
    color_pal <- hue_pal(h.start = 25)(4)

    df_fit    <- do.call("rbind", data[["fit_data"]])
    df_points <- do.call("rbind", data[["point_data"]])

    df_fit$flood_analysis_group <- as.factor(as.numeric(as.character(df_fit$flood_prone))*1 + as.numeric(as.character(df_fit$flood))*2)
    df_fit$flood_analysis_group <- mapvalues(df_fit$flood_analysis_group, from = levels(df_fit$flood_analysis_group), to = c("non_flood_prone", "non_flood", "flood", "flood"))
    df_fit$after_flood    <- as.factor(mapvalues(df_fit$after_flood, c(0,1), c('before_flood','after_flood')))
    df_fit$flood_prone    <- as.factor(mapvalues(df_fit$flood_prone, c(0,1), c('not_flood_prone','flood_prone')))
    df_fit$flood          <- as.factor(mapvalues(df_fit$flood, c(0,1), c('not_flooded','flooded')))

    df_fit$method         <- factor(as.factor(df_fit$method), levels = c(method_list))

    names(df_points)      <- c("sale_id", "flood", "flood_prone", "after_flood", "partials_y", "method", "pruned")

    df_points$flood_analysis_group <- as.factor(as.numeric(as.character(df_points$flood_prone))*1 + as.numeric(as.character(df_points$flood))*2)
    df_points$flood_analysis_group <- mapvalues(df_points$flood_analysis_group, from = levels(df_points$flood_analysis_group), to = c("non_flood_prone", "non_flood", "flood"))

    df_points$after_flood <- mapvalues(df_points$after_flood, c(0,1), c('before_flood','after_flood'))
    df_points$flood_prone <- mapvalues(df_points$flood_prone, c(0,1), c('not_flood_prone','flood_prone'))
    df_points$flood       <- mapvalues(df_points$flood, c(0,1), c('not_flooded','flooded'))

    df_points$method      <- factor(as.factor(df_points$method), levels = c(method_list, "pruned"))

    df_fit    <- subset(df_fit, flood_prone == 'flood_prone' | flood == 'not_flooded')
    df_points <- subset(df_points, flood_prone == 'flood_prone' | flood == 'not_flooded')

    title <- "Predicted log sale price of properties given by the fully specified linear model with three-way interaction"

    ## ## plot with partial residual violin kernels
    ## violins <- ggplot(data = df_fit, aes(x = df_fit[x_axis][,1], y = fit, color = method)) + labs(title =title, x = x_axis, y = "ln_sale_price") + geom_point(position = position_dodge(width = 0.2)) + geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.2, position = 'dodge') + geom_violin(data = df_points, aes(x = df_points[x_axis][,1], y = partials_y, color = method), position = "identity", alpha = 0) + facet_grid(~after_flood, labeller = label_parsed) + scale_color_manual(breaks = c(method_1, method_2, "pruned"), values=c(color_pal[2], color_pal[3], color_pal[1])) + theme_bw()

    ## plot without partial residual violin kernels
    no_violins <- ggplot(data = df_fit, aes(x = df_fit[x_axis][,1], y = fit, color = method)) + labs(title =title, x = x_axis, y = "ln_sale_price") + geom_point(position = position_dodge(width = 0.2)) + geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.2, position = 'dodge') + facet_grid(~after_flood+flood_prone, labeller = label_parsed) + scale_color_manual(values = color_pal) + theme_bw()

    return(list(
        ## "violins" = violins,
        "no_violins" = no_violins))
}

################################################################################################################

partialComparisonPlotInteraction <- function(p_list, method_list, x_axis, int_var, x1_x2) {

    facet  <- as.formula(paste(c('~', int_var)))

    data <<- pool_data(p_list, method_list, x1_x2, c("sale_id", "x.all", "partials_y", "method", "pruned"))

    pch <- 4 ## 20, 4, '.'
    cex <- 0.35
    color_pal <- hue_pal(h.start = 25)(4)

    df_fit <- do.call("rbind", data[["fit_data"]])
    df_points <- do.call("rbind", data[["point_data"]])

    df_fit$flood       <- mapvalues(df_fit$flood, c(0,1), c('not_flood','flood'))
    df_fit$flood_prone <- mapvalues(df_fit$flood_prone, c(0,1), c('not_flood_prone','flood_prone'))
    df_fit$after_flood <- mapvalues(df_fit$after_flood, c(0,1), c('before_flood','after_flood'))

    df_fit <<- df_fit
    df_points <<- df_points

    ## names(df_points) <- c("sale_id", x_axis, int_var, "partials_y", "method")

    ## df_points$flood       <- mapvalues(df_points$flood, c(0,1), c('not_flood','flood'))
    ## df_points$flood_prone <- mapvalues(df_points$flood_prone, c(0,1), c('not_flood_prone','flood_prone'))
    ## df_points$after_flood <- mapvalues(df_points$after_flood, c(0,1), c('before_flood','after_flood'))

    ## df_fit$method <- factor(as.factor(df_fit$method), levels = c(method_1, method_2))
    ## df_points$method <- factor(as.factor(df_points$method), levels = c(method_1, method_2, "pruned"))

    title <- "Predicted log sale price of properties given by the fully specified linear model with two-way interaction"

    ## violins <- ggplot(data = df_fit, aes(x = df_fit[x_axis][,1], y = fit, color = method)) + labs(title = title, x = x_axis, y = "ln_sale_price") + geom_point(position = position_dodge(width = 0.2)) + geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.2, position = 'dodge') + geom_violin(data = df_points, aes(x = df_points[x_axis][,1], y = partials_y, color = method), position = "identity", alpha = 0) + facet_grid(facet, labeller = label_parsed) + scale_color_manual(breaks = c(method_1, method_2, "pruned"), values=c(color_pal[2], color_pal[3], color_pal[1])) + theme_bw()

    no_violins <- ggplot(data = df_fit, aes(x = df_fit[x_axis][,1], y = fit, color = method)) + labs(title = title, x = x_axis, y = "ln_sale_price") + geom_point(position = position_dodge(width = 0.2)) + geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.2, position = 'dodge') + facet_grid(facet, labeller = label_parsed) + scale_color_manual(values=color_pal) + theme_bw()


    return(list(## 'violins' = violins,
                "no_violins" = no_violins))
}

################################################################################################################

partialComparisonPlotFactor <- function(p_list, method_list, var) {

    dev.off()

    data <- pool_data(p_list, method_list, var, c("sale_id", "x.all", "partials_y", "method", "pruned"))

    pch = 4 ## 20, 4, '.'
    cex = 0.35

    df_fit <<- do.call("rbind", data[["fit_data"]])
    df_points <<- do.call("rbind", data[["point_data"]])

    df_fit$method <<- factor(as.factor(df_fit$method), levels = method_list)
    df_points$method <<- factor(as.factor(df_points$method), levels = method_list)

    violins <- ggplot(data = df_fit, aes(x = df_fit[var][,1], y = fit, color=method)) + labs(x = var , y = "ln_sale_price") + geom_violin(data = df_points[which(df_points$pruned == 0),], aes(x = get(var), y = partials_y, color = method), position = "identity", alpha = 0) + geom_point(position = position_dodge(width = 0.2)) + geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.2, position = 'dodge') + theme_bw()

    no_violins <- ggplot(data = df_fit, aes(x = df_fit[var][,1], y = fit, color=method)) + labs(x = var , y = "ln_sale_price") + geom_point(position = position_dodge(width = 0.2)) + geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.2, position = 'dodge') + theme_bw()

    return(list('violins' = violins, "no_violins" = no_violins))
}

################################################################################################################

partialComparisonPlotScale <- function(p_list, method_list, var, var_name) {

    dev.off()
    par(oma = c(1, 1, 1, 15))

    data <- pool_data(p_list, method_list, var_name, c("sale_id", "x.all", "partials_y", "method", "pruned"))

    data <<- data

    color_pal <- hue_pal()(4)
    pch <- 20 ## 20, 4, '.'
    cex <- 0.2
    cex_big <- cex
    lwd = 1.5
    spar <- 1

    df_fit <- do.call("rbind", data[["fit_data"]])
    df_points <- do.call("rbind", data[["point_data"]])

    df_fit <<- df_fit
    df_points <<- df_points

    x_points <- df_points[which(df_points$method == method_list[1]),][,var]
    y_points <- df_points[which(df_points$method == method_list[1]),][,"partials_y"]

    a <- 0.1
    b <- 0.1
    xlim <- c(min(x_points), max(x_points))
    trim_xlim <- c(min(x_points), quantile(x_points, c(1-a)))

    ylim <- c(min(y_points), max(y_points))
    trim_ylim <- c(quantile(y_points, c(b)), quantile(y_points, c(1-b)))

    par(mfrow=c(2,2))

    for(j in 1:length(method_list)) {
        with(df_fit, plot(df_fit[var][,1], fit,
                          type = "n",
                          ## ,sub="subtitle",
                         ,xlab=var
                         ,ylab="ln_sale_price"
                          ,xlim = xlim
                          ,ylim = ylim
                          ))

        with(df_points[which(df_points$method == method_list[j] & df_points$pruned == 0),], points(get(var), partials_y, pch = pch, cex = cex, col = color_pal[j]))

        for(i in 1:length(method_list)) {
            with(df_fit[which(df_fit$method == method_list[i]),], lines(get(var), fit, col = color_pal[i], lwd = lwd))
            with(df_points[which(df_points$method == method_list[i] & df_points$pruned == 0),], lines(smooth.spline(get(var), partials_y, spar = spar), lty = 2, col = color_pal[i], lwd = lwd))
        }
    }

    ## legend(0.03, 0.025, legend=c("raw data", "cem", "mahalanobis/exact with replacement", "mahalanobis/exact without replacement"), lty=1, col = color_pal, bty = "n")

    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    plot(0, 0,
         ## main=paste(c("Effect + partial plot", "ln_sale_price by", var), collapse=" "),
         type = "n", bty = "n", xaxt = "n", yaxt = "n")
    legend("right", c(method_list, 'model', 'spline reg'), xpd = TRUE, inset = c(0,0), bty = "n", col = c(color_pal, 'black', 'black'), lty = c(rep(1,5),2))

    layout(1)
}
