source('scripts/das_data_preprocessing.R')

library('xtable')

n_sum <- vector()

for(au in au_names) {
 a <- subsetByVar(das, "area_unit_name", au)
 mean_summary <- a %>% group_by(sale_year) %>% dplyr::summarise(n_sale_year = n())
 au_name <- rep(au, nrow(mean_summary))
 au_n_sum <- cbind(au_name, mean_summary)
 n_sum <- rbind(n_sum, au_n_sum)
}

n_sum_dcast <- dcast(n_sum, formula = au_name~sale_year,fun.aggregate = sum,value.var = "n_sale_year")

# print(xtable(n_sum_dcast, type = "latex"))

binWidth <- 15
h <- hist(n_sum$n_sale_year, col="gray", xlab = "", labels = FALSE, w=binWidth)

cen_dw_2013 <- read.csv('datasets/censusDwellingUnits2013.csv')

cen_dw_2013_names <- c("au_id","au_name","2001","2006","2013")

names(cen_dw_2013) <- cen_dw_2013_names

cen_dw_2013 <- subset(cen_dw_2013, au_id %in% au_ids)

cen_dw_2013[,1:2] <- lapply(cen_dw_2013[,1:2], as.character)
cen_dw_2013[,3:5] <- lapply(cen_dw_2013[,3:5], as.numeric)

cen_dw_2013 <- melt(cen_dw_2013,id=c("au_id","au_name"))
names(cen_dw_2013) <- c("au_id","au_name","year","value")

iplot <- ggplot(data=cen_dw_2013, aes(x=year, y=value, color=au_name)) + geom_line(aes(group = au_name))

# alph <- ((cen_dw_2013$2006 - cen_dw_2013$2001)/5)
# beta <- cen_dw_2013$2001
# cen_dw_2013$2002 <- beta + alph
# cen_dw_2013$2003 <- beta + 2*alph
# cen_dw_2013$2004 <- beta + 3*alph
# cen_dw_2013$2005 <- beta + 4*alph

cen_dw_nplot <- ggplot(data=cen_dw_2013, aes(x=reorder(au_name,au_id,length))) + geom_bar() + theme(axis.text.x=element_text(angle = -90, hjust = 0))
