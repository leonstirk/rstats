## source('functions/grid.R')

fitness <- function(data, model) {
    fit <- lm(model, data = data)
    r2 <- summary(fit)$r.squared
    return(r2)
}

first_gen_model <- function(raw_cm, data, model, agg_group, n_sub) {
    genome <- floor(runif(length(raw_cm), 0, n_sub))
    data['genome'] <- mapvalues(data[,agg_group], raw_cm, genome)
    r2 <- fitness(data, model)
    genome <- paste(genome, collapse = "")
    return(list("r2" = r2, "genome" = genome))
}

n_gen_model <- function(raw_cm, data, model, agg_group, n_sub, genome) {
    data['genome'] <- mapvalues(data[,agg_group], raw_cm, genome)
    r2 <- fitness(data, model)
    genome <- paste(genome, collapse = "")
    return(list("r2" = r2, "genome" = genome))
}

mutate <- function(genome, n_sub) {
    l <- length(genome)
    a <- ceiling(runif(3,0,l))
    b <- floor(runif(3,0,n_sub))
    genome[a] <- b
    return(genome)
}

bootstrap <- function(d, n, i) {
    part <- c(0,seq(1:n)/n)
    return(sample(rep(1:n, diff(floor(nrow(d) * part)))) == i)
}

##########################################
strings[["model"]] <- "D"
do.call(setStrings, strings)

##########################################

genetic <- function(data = raw, model_strings = model_strings, agg_group = 'cantor', n_sub = 4, generations = 3000) {

    model <- as.formula(paste(resp_var, " ~ ", paste(c("genome", model_strings), collapse = " + ")))
    raw <- data[c('X', 'Y', 'cantor', resp_var, names(model_strings))]
    raw_cm <- levels(raw[,agg_group])

    set_names <- c('train', 'test', 'validate')
    data_sets <- list()

    for(i in 1:length(set_names)) {
        data_sets[[set_names[i]]] <- raw[bootstrap(raw, length(set_names), i),]
    }

    data <- data_sets[["train"]]

###########################

    models <- list()

    cantor <- as.numeric(levels(data[,agg_group]))
    pop <- 52
    generation <- 1
    train_fit <- c()
    test_fit <- c()
    validation_fit <- c()

    for(i in 1:pop) {
        models[[i]] <- first_gen_model(raw_cm, data, model, agg_group, n_sub)
    }

    m_df <- as.data.frame(rbindlist(lapply(models, as.list)))
    m_df$gen <- generation

###########################

    while(generation <= generations) {
        m_df <- m_df[with(m_df, order(-r2))[1:(pop/2)],]

        train_fit[generation] <- m_df[1,'r2']
        test_fit[generation] <- n_gen_model(raw_cm, data_sets[['test']], model, agg_group, n_sub, unlist(strsplit(m_df[1,'genome'], character(0))))['r2']
        validation_fit[generation] <- n_gen_model(raw_cm, data_sets[['validate']], model, agg_group, n_sub, unlist(strsplit(m_df[1,'genome'], character(0))))['r2']

        print(paste('Generation:', generation, ". Max r2: ", m_df[1,'r2']))

        xlen <- ceiling(length(cantor)/2)
        x <- lapply(strsplit(m_df[,'genome'], character(0)), function(x) { paste(x[1:xlen], collapse = "") })
        y <- lapply(strsplit(m_df[,'genome'], character(0)), function(x) { paste(x[(xlen+1):length(cantor)], collapse = "") })

        offspring_genomes <- lapply(strsplit(c(paste(x[1:(pop/4)],y[((pop/4)+1):(pop/2)], sep = ""), paste(y[1:(pop/4)],x[((pop/4)+1):(pop/2)], sep = "")), character(0)), as.numeric)

        models <- list()
        generation <- generation + 1

        offspring_genomes <- lapply(offspring_genomes, function(x) { mutate(x, n_sub) })

        for(i in 1:length(offspring_genomes)) {
            models[[i]] <- n_gen_model(raw_cm, data, model, agg_group, n_sub, offspring_genomes[[i]])
        }

        o_df <- as.data.frame(rbindlist(lapply(models, as.list)))
        o_df$gen <- generation

        m_df <- rbind(m_df, o_df)
    }

###########################

    evolution <- as.data.frame(cbind('generation' = seq(1:generations), 'train' = unlist(train_fit), 'test' = unlist(test_fit), 'validation' = unlist(validation_fit)))
    winner <- as.numeric(strsplit(m_df[1, 'genome'], character(0))[[1]])
    raw['genome'] <- mapvalues(raw[,agg_group], raw_cm, winner)

    model <- as.formula(paste(resp_var, " ~ ", paste(c(agg_group, model_strings), collapse = " + ")))
    theoretical_max <- fitness(raw, model)

    model <- as.formula(paste(resp_var, " ~ ", paste(c(model_strings), collapse = " + ")))
    baseline <- fitness(raw, model)

    return(list('final_generation' = m_df, 'evolution' = evolution, 'plot_lines' = c('baseline' = baseline, 'max' = theoretical_max),'winner' = winner, data = raw))
}

## test <- genetic(gridData(flood_data_subsets[["IF"]], 'lon_gd2000_x', 'lat_gd2000_y', 20), c('sale_quarter' = 'sale_quarter', lm_model_strings[2:3]), generations = 1000)

test <- genetic(gridData(flood_data_subsets[["IF"]], 'lon_gd2000_x', 'lat_gd2000_y', 20), c(lm_model_strings[c(1:5,8)]), generations = 100)

## plotGrid(test[['data']], 'genome')
plot(test$evolution$train ~ test$evolution$generation, type = 'l', col = 'red', ylim = c(test$plot_lines['baseline'], test$plot_lines['max']))
with(test$evolution, lines(test ~ generation, col = 'blue'))
with(test$evolution, lines(validation ~ generation, col = 'green'))
abline(h = test$plot_lines['baseline'])
abline(h = test$plot_lines['max'])



## A good genome for 4 submarkets
## "1 2 2 3 2 2 1 2 2 2 3 0 3 3 1 1 3 0 2 2 2 0 3 2 2 1 2 0 3 2 1 2 1 0 3 0 1 3 2 1 0 3 1 2 2 3 2 2 3 3 1 2 3 2 2 2 3 3 1 3 3 2 2 3 3 2 1 0 3 1 2 3 2 3 1 2 0 3 2 2 0 1 2 2 1 2 0 1 1 1 1 2 1 0 1 0 2 2 0 1 1 1 1 1 2 1 0 1 1 1 1 2 0 1 1 1 3 1 0 1 0 2 0 0 2 0 1 0 1 0 1 1 0 1 3 0 2 1 3 3 2 1 2 2 1 3 3 3 1 2 1 1 3 1 3 3 1 3 0 3 2 3 2 2 1 2 3 3 1"
