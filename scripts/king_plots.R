source('scripts/mah_m.R')

library(plyr)
library(gridExtra)

layout(matrix(seq(1,1,1),2,1))

a_sub_nomiss$treatment_f<-mapvalues(a_sub_nomiss$treatment, from = c(0, 1), to = c("C", "T"))

a_plot <- ggplot(data=a_sub_nomiss, aes(x=land_area, y=building_floor_area, color=treatment_f)) + geom_text(aes(label=treatment_f)) + scale_colour_manual(values=c("Red","Blue")) + theme(legend.position="none")

m_data$treatment_f<-mapvalues(m_data$treatment, from = c(0, 1), to = c("C", "T"))

b_plot <- ggplot(data=m_data, aes(x=land_area, y=building_floor_area, color=treatment_f)) + geom_text(aes(label=treatment_f)) + scale_colour_manual(values=c("Red","Blue")) + theme(legend.position="none")

grid.arrange(a_plot, b_plot, ncol=2)
