clear
import excel "/Users/stirkwang/Desktop/thesis/oia_area-unit-true-centroid_061115.xlsx", sheet("Distances") firstrow
rename AREA_SQ_KM sausqkm
rename sausqkm sausqkmtt
rename LAND_AREA_SQ_KM sausqkmln
rename LATITUDE saulat
rename LONGITUDE saulon
rename dist saudist
rename saudist saudistcbd
save ~/Desktop/thesis/distanceData.dta, replace
