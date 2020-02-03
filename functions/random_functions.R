## in:
## "scripts/das_data_preprocessing.R"

specify_decimal <- function(x, k) format(round(x, k), nsmall=k, scientific = FALSE)

clean_summary  <- function(lmcoef, digits) {
    coefs <- as.data.frame(lmcoef)
    coefs[] <- lapply(coefs, function(x) specify_decimal(x, digits))
    coefs
}

makeDescriptives <- function(x) {
    descriptives <- c('mean', 'median', 'sd', 'min', 'max')
    df <- data.frame(matrix(, nrow = ncol(x), ncol = 0))
    for(i in descriptives) {
        df[,i] <- sapply(x, i, na.rm = TRUE)
    }
    rownames(df) <- colnames(x)
    return(df)
}

sampleDescriptives <- function(df, des_vars, digits) {
    clean_summary(makeDescriptives(as.data.frame(lapply(df[des_vars], function(x) { as.numeric(as.character(x)) }))),digits)
}

doDescriptives <- function(data, des_vars) {
    x <- as.data.frame(lapply(data[des_vars], function(x) { as.numeric(as.character(x)) }))

    descriptives <- c('mean', 'median', 'sd', 'min', 'max')
    df <- data.frame(matrix(, nrow = ncol(x), ncol = 0))
    for(i in descriptives) {
        df[,i] <- sapply(x, i, na.rm = TRUE)
    }
    rownames(df) <- colnames(x)
    return(df)
}

doRegression <- function(data, model_formula, partial_strings) {
    fit <- lm(model_formula, data = data)
    partials <- partial_resid_numeric_analysis(partial_strings, fit, data)
    return(list('fit' = fit, 'partials' = partials))
}

tcDescriptives <- function(unmatched_sub, treatment, des_vars) {
    t <- doDescriptives(unmatched_sub[which(unmatched_sub[treatment] == 1),], des_vars)
    c <- doDescriptives(unmatched_sub[which(unmatched_sub[treatment] == 0),], des_vars)
    tc <- list()
    for (i in c('mean', 'sd')) {
        tc[[i]] <- cbind(t[i],c[i])
        names(tc[[i]]) <- c('treatment', 'control')
    }
    ht_1 <- hux(tc, add_colnames = TRUE, add_rownames = TRUE)
    ht_1 <- ht_1 %>% set_bottom_border(1, everywhere, 2) %>% set_align(everywhere, everywhere, 'right') %>% set_number_format(2) %>% map_text_color(by_cols('green', 'red')) %>% set_text_color(everywhere, 1, 'black') %>% set_right_border(everywhere, c(1,3,5), 1)

    tc <- list()
    for (i in c('min', 'median', 'max')) {
        tc[[i]] <- cbind(t[i],c[i])
        names(tc[[i]]) <- c('treatment', 'control')
    }
    ht_2 <- hux(tc, add_colnames = TRUE, add_rownames = TRUE)
    ht_2 <- ht_2 %>% set_bottom_border(1, everywhere, 2) %>% set_align(everywhere, everywhere, 'right') %>% set_number_format(2) %>% map_text_color(by_cols('green', 'red')) %>% set_text_color(everywhere, 1, 'black') %>% set_right_border(everywhere, c(1,3,5,7), 1)

    return(list(ht_1, ht_2))
}

imbalanceSummary <- function(imb) {
    table <- rbind(
        unlist(lapply(imbalance_diagnostics[["imb"]], function(x) { x[["L1"]][["L1"]] })),
        unlist(lapply(imbalance_diagnostics[["imb"]], function(x) { x[["L1"]][["LCS"]] }))
    )
    rownames(table) <- c("Multivariate Imbalance Measure (L1)", "Percentage of local common support (LCS)")
    ht <- hux(table, add_colnames = TRUE, add_rownames = TRUE)

    ht[1,1]                <- ""
    bottom_border(ht)[1,]  <- 2
    align(ht)[,1]        <- 'left'
    align(ht)[,2:5]        <- 'right'
    right_padding(ht)      <- 10
    left_padding(ht)       <- 10
    number_format(ht)[,2:5]      <- 4
    width(ht)              <- 1
    col_width(ht)          <- c(2/6,rep(1/6,4))
    return(ht)
}

sensitivityCheckSummary <- function(stepwise_reg_list, var, var_name) {
    table <- rbind(
        unlist(lapply(stepwise_reg_list, function(x) { mean(unlist(lapply(x, function(y) { y[["coefficients"]][var_name, "Estimate"] }))) })),
        unlist(lapply(stepwise_reg_list, function(x) { sd(unlist(lapply(x, function(y) { y[["coefficients"]][var_name, "Estimate"] }))) }))
    )
    rownames(table) <- c("Mean", "Std. Dev")
    ht <- hux(table, add_colnames = TRUE, add_rownames = TRUE)

    ht[1,1]                <- var
    bottom_border(ht)[1,]  <- 2
    align(ht)[,1]        <- 'left'
    align(ht)[,2:5]        <- 'right'
    right_padding(ht)      <- 10
    left_padding(ht)       <- 10
    number_format(ht)[,2:5]      <- 4
    width(ht)              <- 1
    col_width(ht)          <- c(2/6,rep(1/6,4))
    return(ht)
}
