use ~/Desktop/thesis/rawQVData.dta, clear

rename lqValue lqv
rename uqValue uqv
rename medValue mv
rename lqFloor lqf
rename uqFloor uqf
rename medFloor mf
rename mixVal mxv
rename mixFloor mxf
rename logMedVal lnmv
rename logMixVal lnmxv
rename annualROCVal rocmv
rename threeYearROCVal marocmv
rename annualROCMix rocmx
rename threeYearROCMix marocmx

save ~/Desktop/thesis/renamedQVData.dta, replace
