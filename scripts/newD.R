source('scripts/baseline_analysis_prep.R')

##############
## Matching ##
##############

RAW_match <- function(u_data) {
    u_data   <- u_data[data_vars]
    u_data$w <- rep(1,nrow(u_data))
    return(list(
        "m_data" = u_data,
        "m_out"  = u_data
    ))
}

CEM_match <- function(u_data, breaks) {
    if(breaks == "")
        breaks <- setBreaks(u_data)
    u_data <- u_data[data_vars]
    m_out  <- cem(treatment = treat_var, data = u_data[c(treat_var, match_vars)], cutpoints = breaks[["cutpoints"]], grouping = breaks[["grouping"]])
    m_data <- cbind(u_data[which(m_out$matched),c(data_vars)], "w" = m_out$w[which(m_out$matched)])
    return(list(
        "m_summary" = m_out$tab,
        "m_data"    = m_data,
        "m_out"     = m_out
    ))
}

MAH_match <- function(u_data, replace) {
    u_data   <- u_data[data_vars]
    m_out    <- matchit(mah_match_formula, method = "nearest", distance = "mahalanobis", data = u_data, exact = exact_vars, replace = replace)
    m_data   <- match.data(m_out)
    names(m_data)[names(m_data) == 'weights'] <- 'w'
    names(m_out)[names(m_out) == 'weights'] <- 'w'
    return(list(
        "m_summary" = m_out$nn,
        "m_data"    = m_data,
        "m_out"     = m_out
    ))
}

###########################
## Imbalance diagnostics ##
###########################

calculateImbalance <- function(m_data, breaks) {
    if(breaks == "")
        breaks <- setBreaks(m_data)
    imb <- imbalance(group = m_data[,treat_var], data = m_data[c(match_vars)], weights = m_data$w, breaks = breaks[["cutpoints"]], grouping = breaks[["grouping"]])
    return(imb)
    }

calculateL1Profile <- function(u_data, m_out, num) {
    add <- TRUE
    if(num == 1)
        add <- FALSE
    return(L1.profile(u_data[,treat_var], u_data[match_vars], weights=m_out$w>0, add=add, col=num, lty=num))
}

makeSpacegraphDataframe <- function(u_data, m_out, method) {
    if(method == "cem")
        method <- "user cem"
    sp_match <- data.frame(rownames(u_data))
    names(sp_match)[1] <- "id"
    sp_match$weight <- m_out$w
    sp_match$method <- method
    return(sp_match)
}

plotL1Profiles <- function(u_data, match_objects) {
    lapply(1:length(match_objects), function(i) { calculateL1Profile(u_data, match_objects[[i]][["m_out"]], i) })
    legend(50, 0.4, legend=c("raw data", "cem", "mahalanobis/exact with replacement", "mahalanobis/exact without replacement"), lty=1:4, col=c("black", "red", "green", "blue"), bty = "n")
}

getImbalanceDiagnostics <- function(u_data, match_objects, cem_no) {
    ## match_objects_imbalance <- lapply(1:length(match_objects), function(i) { calculateImbalance(match_objects[[i]][["m_data"]], "") })
    ## names(match_objects_imbalance) <- names(match_objects)

    sp_df <- lapply(names(match_objects[-1]), function(x) { makeSpacegraphDataframe(u_data, match_objects[[x]][["m_out"]], x) })
    sp <- spacegraph(treat_var, cbind(u_data[c(treat_var)], as.data.frame(lapply(u_data[c(match_vars)], function(x) { as.numeric(x) }))), M=100, R=list(cem=cem_no), other.matches=sp_df)

    ## out <- list("imb" = match_objects_imbalance, "spacegraph" = sp)
    out <- sp

    return(out)
}

###############
## Modelling ##
###############

SATT_lm <- function(m_data, lm_model_formula, partial_strings) {
    m_data   <<- m_data
    lm       <- lm(lm_model_formula, data = m_data, weights = w)
    partials <- partial_resid_numeric_analysis(partial_strings, lm, m_data)
    reg      <- summary(lm)
    SATT_lm  <- list("reg" = reg, "partials" = partials)
    return(SATT_lm)
    rm(m_data)
}

SATT_rf <- function(m_data, rf_model_formula) {
    m_data <<- m_data

    rf                   <- randomForest(rf_model_formula, data = m_data, subset = m_data[treat_var] == 0, mtry = 5, ntree = 500)

    tmp_data             <- m_data
    tmp_data[,treat_var] <- 0
    prd                  <- predict(rf, tmp_data)
    TEi                  <- m_data[,resp_var] - prd
    TEj                  <- TEi[which(m_data[treat_var] == 1)]
    TE                   <- mean(TEj)

    ww <- table(m_data[treat_var])
    v0 <- var(m_data[m_data[treat_var] == 1, resp_var])
    v1 <- var(prd[m_data[treat_var] == 1])

    out                  <- matrix(NA, 4, 1)
    dimnames(out)        <- list(c("Estimate", "Std. Error", "t value", "p-value"), treat_var)
    out["Estimate", ]    <- TE
    out["Std. Error", ]  <- sqrt((v1 + v0) * sum(ww^2)/sum(ww)^2)
    out["t value", ]     <- out["Estimate", ]/out["Std. Error", ]
    out["p-value", ]     <- 2 * (1 - pnorm(abs(out["t value", ])))
    return(out)
    rm(m_data)
}

stepwiseRegList <- function(match_objects) {
    stepwise_reg_list <- lapply(match_objects, function(x) { lapply(1:length(model_formula_list), function(i) { SATT_lm(x$m_data,model_formula_list[[i]], partial_strings_list[[i]])[["reg"]] }) })
    return(stepwise_reg_list)
}

fullSpecRegList <- function(match_objects) {
    full_spec_reg_list <- lapply(match_objects, function(x) { SATT_lm(x$m_data, lm_model_formula, partial_strings)[["reg"]] })
    return(full_spec_reg_list)
}

resSpecRegList <- function(match_objects) {
    res_spec_reg_list <- lapply(match_objects, function(x) { SATT_lm(x$m_data, res_model_formula, partial_strings[-1])[["reg"]] })
    return(res_spec_reg_list)
}

fullSpecPartialsList <- function(match_objects) {
    full_spec_partials_list <- lapply(match_objects, function(x) { SATT_lm(x$m_data, lm_model_formula, partial_strings)[["partials"]] })
    return(full_spec_partials_list)
}

FS_HT <- function(full_spec_reg_list, coefs) {
    ht <- huxreg(full_spec_reg_list, statistics = c(N = "nobs", R2 = "r.squared"), coefs = coefs)
    width(ht)     <- 0.7
    col_width(ht) <- c(0.3, rep(0.1, 4))
    return(ht)
}

runModel <- function(data, model_type, treat_var, inf_var, n_cem) {
    strings[["model"]] <<- model_type
    strings[["treat_var"]] <<- treat_var
    strings[["inf_var"]] <<- inf_var

    do.call(setStrings, strings)

    match_objects <<- list(
    "raw" = RAW_match(data),
    "cem" = CEM_match(data, ""),
    "mah" = MAH_match(data, TRUE),
    "mah_nr" = MAH_match(data, FALSE)
    )

    ## plotL1Profiles(data, match_objects)
    imbalance_diagnostics <<- getImbalanceDiagnostics(data, match_objects, n_cem)

    full_spec_reg_list <<- fullSpecRegList(match_objects)
    full_spec_partials_list <<- fullSpecPartialsList(match_objects)
    stepwise_reg_list <<- stepwiseRegList(match_objects)

    fs_ht <<- FS_HT(full_spec_reg_list, coefs)
    st_ht <<- lapply(stepwise_reg_list, function(x) { huxreg(x, statistics = c(N = "nobs", R2 = "r.squared"))})

}

##########################################

## start.time <- Sys.time()
## set.seed(20)

## ## runModel(flood_data_subsets[["BF"]], "D", "flood_prone", "", 10)
## runModel(flood_data_subsets[["IF"]], "DiD", "flood_prone", "", 10)
## ## runModel(flood_data_subsets[["RF"]], "DiD", "flood", "" 10)
## ## runModel(flood_data_subsets[["IF"]], "DiDiD", "flood", "flood_prone", 10)

## space <- plot(imbalance_diagnostics, scale.var = F, N = 'all')
## print(fs_ht)

## end.time <- Sys.time()
## time.taken <- end.time - start.time
## print(time.taken)

##########################################

## tmp1 <- c(rep('', length(match_vars)))
## tmp2 <- c(rep('', length(match_vars)))
## tmp1[which(match_vars %in% exact_vars)] <- 'Y'
## tmp2[which(match_vars %in% mah_vars)] <- 'Y'

## ht <- hux(
##     "Variable name" = match_vars,
##     "Variable class" = sapply(das[match_vars], class),
##     "CEM" = rep('Y', length(match_vars)),
##     "Mahalanobis distance" = tmp2,
##     "Exact" = tmp1,
##     add_colnames = TRUE
## )

## bottom_border(ht)[1,]  <- 2
## right_padding(ht)      <- 10
## left_padding(ht)       <- 10
## width(ht)              <- 1
## col_width(ht)          <- c(rep(0.2,5))
## valign(ht)             <- 'bottom'
## align(ht)[,2:4]        <- 'center'
## match_vars_summary     <- map_wrap(ht, by_cols(FALSE, TRUE))
## rm(ht, tmp1, tmp2)
