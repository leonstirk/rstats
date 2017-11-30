levelsof sau

foreach lev in `r(levels)'  { 
        if _clus_4 == 1 { 
                local plotline1  "`plotline1' tsline rocmv if sau == `lev', lc(black)  || " 
        } 
        else if _clus_4 == 2 { 
                local plotline2  "`plotline2' tsline rocmv if sau == `lev', lc(blue) || " 
        } 
		else if _clus_4 == 3 {
				local plotline3  "`plotline3' tsline rocmv if sau == `lev', lc(red) || "
		}
		else {
				local plotline4  "`plotline4' tsline rocmv if sau == `lev', lc(green) || "
		}
} 
twoway `plotline1' `plotline2' `plotline3' `plotline4', legend(off)
