clear
import excel "/Users/stirkwang/Desktop/thesis/2013_mb_dataset_Auckland_Region/2013-mb-dataset-Auckland-Region-family.xlsx", sheet("Proportions") firstrow
rename Totalfamiliesinoccupiedprivat fmtt
rename Familytypeoccupiedprivatedwe fnpdnc
rename F fmpdcc
rename G fmpdsp
rename H fmpdtt
rename Totalfamilyincomegrouped1 fmin20l
rename J fmin20t30
rename K fmin30t50
rename L fmin50t70
rename M fmin70t100
rename N fmin100p
rename O fmints
rename P fminnf
rename Q fmintt
rename R fminmed
rename Sourcesoffamilyincome678 fmsiws
rename T fmsise
rename U fmsiid
rename V fmsiac
rename W fmsisu
rename X fmsipe
rename Y fmsiub
rename Z fmsisb
rename AA fmsidpb
rename AB fmsiib
rename AC fmsisa
rename AD fmsiob
rename AE fmsiot
rename AF fmsini
rename AG fmsits
rename AH fmsinf
rename AI fmsitt
save "/Users/stirkwang/Desktop/thesis/familyData.dta", replace


