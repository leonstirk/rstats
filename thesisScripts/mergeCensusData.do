clear
do ~/Desktop/thesis/renameDwellingData.do
do ~/Desktop/thesis/renameFamilyData.do
do ~/Desktop/thesis/renameHouseholdData.do
do ~/Desktop/thesis/renameIndividual1Data.do
do ~/Desktop/thesis/renameIndividual2Data.do
do ~/Desktop/thesis/renameIndividualData3a.do
do ~/Desktop/thesis/renameIndividualData3b.do
do ~/Desktop/thesis/renameDistancesData.do

use ~/Desktop/thesis/familyData.dta, clear
sort sau year
save, replace

use ~/Desktop/thesis/householdData.dta, clear
sort sau year
save, replace

use ~/Desktop/thesis/individual1Data.dta, clear
sort sau year
save, replace

use ~/Desktop/thesis/individual2Data.dta, clear
sort sau year
save, replace

use ~/Desktop/thesis/individual3aData.dta, clear
sort sau year
save, replace

use ~/Desktop/thesis/individual3bData.dta, clear
sort sau year
save, replace

use ~/Desktop/thesis/dwellingData.dta, clear
sort sau year

merge sau year using ~/Desktop/thesis/familyData.dta ~/Desktop/thesis/householdData.dta ~/Desktop/thesis/individual1Data.dta ~/Desktop/thesis/individual2Data.dta ~/Desktop/thesis/individual3aData.dta ~/Desktop/thesis/individual3bData.dta
tab _merge
save ~/Desktop/thesis/censusMerge.dta, replace

