//mapping


spmap _clus_4 using ~/Desktop/thesis/akl_coordinates.dta if year==2001, id(_ID) clnumber(4) clmethod(quantile) fcolor(BuRd) ndfcolor(gs8) ndlab("Missing") legend(size(*1.4))
