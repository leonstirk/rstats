clear

do ~/Desktop/thesis/mergeCensusData.do

do ~/Desktop/thesis/renameQVData.do

do ~/Desktop/thesis/renameDistancesData.do

clear
use ~/Desktop/thesis/censusMerge.dta, clear
drop _merge*
sort sau year
save, replace

use ~/Desktop/thesis/distanceData.dta
sort sau year
save, replace

use ~/Desktop/thesis/renamedQVData.dta, clear
sort sau year
merge sau year using ~/Desktop/thesis/distanceData.dta ~/Desktop/thesis/censusMerge.dta
tab _merge
save ~/Desktop/thesis/QVCensusMerge.dta, replace
