source('scripts/mah_m.R')

layout(matrix(seq(1,12,1),3,4)) # optional 12 graphs/page

a_sub_nomiss$treatment_f<-mapvalues(a_sub_nomiss$treatment, from = c(0, 1), to = c("C", "T"))
ggplot(data=a_sub_nomiss, aes(x=ln_land_area, y=ln_building_floor_area, color=treatment_f)) + geom_text(aes(label=treatment_f)) + scale_colour_manual(values=c("Red","Blue"))

m_data$treatment_f<-mapvalues(m_data$treatment, from = c(0, 1), to = c("C", "T"))
ggplot(data=m_data, aes(x=ln_land_area, y=ln_building_floor_area, color=treatment_f)) + geom_text(aes(label=treatment_f)) + scale_colour_manual(values=c("Red","Blue"))