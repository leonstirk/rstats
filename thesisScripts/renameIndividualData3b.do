clear
import excel "/Users/stirkwang/Desktop/thesis/2013_mb_dataset_Auckland_Region/2013-mb-dataset-Auckland-Region-individual-part-3b.xlsx", sheet("Proportions") firstrow
rename HoursworkedpwemploymentURP inhrw1t9
rename E inhrw10t19
rename F inhrw20t29
rename G inhrw30t39
rename H inhrw40t49
rename I inhrw50t59
rename J inhrw60p
rename K inhrwts
rename L inhrwnf
rename M inhrwtt
rename MainmeansoftraveltoworkEUR inmtwhm
rename O inmtwdng
rename P inmtwpvt
rename Q inmtwcom
rename R inmtwpas
rename S inmtwbus
rename T inmtwtrn
rename U inmtwmcy
rename V inmtwbcy
rename W inmtwwlk
rename X inmtwot
rename Y inmtwts
rename Z inmtwnf
rename AA inmtwtt
save "/Users/stirkwang/Desktop/thesis/individual3bData.dta", replace
