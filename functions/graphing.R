
## seed of most significant result
MaxSeed <- function(mc, varname) {
    estimates <- unlist(lapply(mc, function(x) { summary(x[["model_fit"]])[["coefficients"]][[varname, "Estimate"]] }))
    seed <- which.min(significances)
    return(seed)
}

which.median <- function(x) {
    if (length(x) %% 2 != 0) {
        which(x == median(x))
    } else if (length(x) %% 2 == 0) {
        a = sort(x)[c(length(x)/2, length(x)/2+1)]
        c(which(x == a[1]), which(x == a[2]))
    }
}

MedianSeed <- function(mc, varname) {
    estimates <- unlist(lapply(mc, function(x) { summary(x[["model_fit"]])[["coefficients"]][[varname, "Estimate"]] }))
    seed <- which.median(estimates)
    return(seed)
}


## KDE of estimates
KDE_MC_EST <- function(mc, comp, varname, title) {
    color_pal <- hue_pal()(3)
    estimates <- unlist(lapply(mc, function(x) { summary(x[["model_fit"]])[["coefficients"]][[varname, "Estimate"]] }))
    sm.density(estimates, xlim = c(-0.1, 0.1), col = color_pal[2], xlab="Coefficient estimates")
    title(main = title)
    abline(v = median(estimates), col = color_pal[2])
    abline(v = comp[["model_summary"]][["unmatched_data"]][["coefficients"]][varname, "Estimate"], col = color_pal[1])
    axis(1, c(median(estimates),comp[["model_summary"]][["unmatched_data"]][["coefficients"]][varname, "Estimate"]), c(round(median(estimates),2),round(comp[["model_summary"]][["unmatched_data"]][["coefficients"]][varname, "Estimate"],2)))
}

## KDE of significances
KDE_MC_SIG <- function(mc, comp, varname, title) {
    color_pal <- hue_pal()(3)
    significances <- unlist(lapply(mc, function(x) { summary(x[["model_fit"]])[["coefficients"]][[varname, "Pr(>|t|)"]] }))
    sm.density(significances, xlim = c(0,1), col = color_pal[2], xlab = "Coefficient p-values")
    title(main = title)
    abline(v = 0.05)
    abline(v = 0.1)
    abline(v = median(significances), col = color_pal[2])
    abline(v = comp[["model_summary"]][["unmatched_data"]][["coefficients"]][varname, "Pr(>|t|)"], col = color_pal[1])
    axis(1, c(median(significances),comp[["model_summary"]][["unmatched_data"]][["coefficients"]][varname, "Pr(>|t|)"]), c(round(median(significances),2),round(comp[["model_summary"]][["unmatched_data"]][["coefficients"]][varname, "Pr(>|t|)"], 2)))
}



L1 <- function(model_data, var_list) {
    todrop <- names(model_data[["unmatched_data"]])[! names(model_data[["unmatched_data"]]) %in% var_list]
    uL1 <- imbalance(model_data[["unmatched_data"]]$treatment, model_data[["unmatched_data"]], drop = todrop)
    todrop <- names(model_data[["matched_data"]])[! names(model_data[["matched_data"]]) %in% var_list]
    mL1 <- imbalance(model_data[["matched_data"]]$treatment, model_data[["matched_data"]], drop = todrop)
    return(list(uL1, mL1))
}

L1m <- function(match_data, var_list) {
    todrop <- names(match_data)[! names(match_data) %in% var_list]
    mL1 <- imbalance(match_data$treatment, match_data, drop = todrop)
    return(mL1)
}
