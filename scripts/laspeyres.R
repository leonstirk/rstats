source('scripts/das_data_preprocessing.R')

table <- table(das$area_unit_id, das$sale_year)

a <- melt(table, id = "Var1", value.name = "value")
a$Var1 <- as.factor(a$Var1)

ggplot(data=a, aes(x=Var2, y=value, colour=Var1)) + geom_line()

