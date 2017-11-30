use ~/Desktop/thesis/QVCensusMerge.dta

tsset sau year

forval i = 1/4 { 
	xtline rocmv if _clus_4 == `i' , overlay legend(off) name(cluster`i') yscale(range(-0.2,0.4))
	local graphs `graphs' cluster`i'
} 

graph combine `graphs'
