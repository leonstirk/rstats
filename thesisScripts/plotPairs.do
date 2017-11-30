
use ~/Desktop/thesis/QVCensusMergeModelVis.dta

tsset cluster year

xtline adjavclus if cluster==1|cluster==2, overlay name(pair1and2)
local graphs `graphs' pair1and2

xtline adjavclus if cluster==2|cluster==3, overlay name(pair2and3)
local graphs `graphs' pair2and3

xtline adjavclus if cluster==3|cluster==4, overlay name(pair3and4)
local graphs `graphs' pair3and4

graph combine `graphs'
