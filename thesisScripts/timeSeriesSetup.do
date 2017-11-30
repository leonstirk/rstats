// time series setup
clear

do ~/Desktop/thesis/QVCensusMerge.do

tsset sau year

drop _merge*
drop CE
drop inocc*
drop inind*
drop if name=="Redoubt East"
drop if name=="Grange"
drop if name=="Puhinui South"
drop if name=="Greenmount"
drop if name=="Mt Wellington Domain"
drop if name=="Mt Wellington South"
drop if name=="St Johns"


gen ed0 = ined0+ined1+ined2+inedoss+inednf
gen ed1 = ined3+ined4
gen ed2 = inedbclr+ineddip
gen ed3 = inedhons+inedmast+inedphd

label variable inetma "Ethnic group, URP - Maori"
gen inetother = inetmelaa + inetot

gen prRentals = hhsltt/hhtntt
gen hhslgovt =  hhslla + hhslhnz + hhsloso + hhslnf

drop insb*
drop inrl*

gen lninc = ln(inincmed)

sort sau year

save, replace

use ~/Desktop/thesis/akl_data_edit.dta, clear
sort sau year
save, replace
use ~/Desktop/thesis/QVCensusMerge.dta, clear
drop _merge*
sort sau year
merge sau using ~/Desktop/thesis/akl_data_edit.dta
tab _merge
drop _merge*

drop AU_NAME 
drop rm*
drop br*
drop rtpv rtnp rttt fmtt fmin20l fmin20t30 fmin30t50 fmin50t70 fmin70t100 fmin100p fmints fminnf fmintt fminmed
drop fmsi*
drop hhur*
drop hhin*
drop hhsi*
drop hhrp*
drop hhmv* hhtl*
//drop insx*  inage0t4 inage5t9 inage10t14 inage15t19 inage20t24 inage25t29 inage30t34 inage35t39 inage40t44 inage45t49 inage50t54 inage55t59 inage60t64 inage65p inagett inabtt iniurs iniure iniurnb iniuros iniurnfa iniurts iniurnf iniurtt inrsmar inrssep inrsnv inrsts inrsnf inrstt inptpt inptnp inptts inptnf inpttt inchl* inedp*  ininc5l ininc5t10 ininc10t20 ininc20t30 ininc30t50 ininc50p inincts inincnf ininctt insi*  inlf* insie* inhrw*  
//drop inmtw*
drop marocm*
drop lnmxv 


