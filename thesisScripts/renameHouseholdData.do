clear
import excel "/Users/stirkwang/Desktop/thesis/2013_mb_dataset_Auckland_Region/2013-mb-dataset-Auckland-Region-household.xlsx", sheet("Proportions") firstrow
rename Censustotalhouseholdsinoccup hhtt
rename HouseholdcompositionOPDOne hhcm1f
rename HouseholdcompositionOPDTwo hhcm2f
rename HouseholdcompositionOPDThre hhcm3fp
rename HouseholdcompositionOPDOthe hhcmom
rename I hhcmop
rename HouseholdcompositionOPDTota hhcmts
rename HouseholdcompositionOPDHous hhcmnf
rename L hcmtt
rename Numberofusualresidentsinhous hhur1
rename N hhur2
rename O hhur3
rename P hhur4
rename Q hhur5
rename R hhur6
rename S hhur7
rename T hhur8
rename U hhurtt
rename V hhurmn
rename Totalhouseholdincomegrouped hhin20l
rename X hhin20t30
rename Y hhin30t50
rename Z hhin5070
rename hhin5070 hhin50t70
rename AA hhinc70y100
rename AB hhin100p
rename hhinc70y100 hhin70t100
rename AC hhints
rename AD hhinnf
rename AE hintt
rename AF hhinmed
rename Sourcesofhouseholdincome67 hhsi
rename hhsi hhsiws
rename AH hhsise
rename AI hhsiid
rename AJ hhsiac
rename AK hhsisu
rename AL hhsipe
rename AM hhsiub
rename AN hhsisb
rename AO hhsidpb
rename AP hhsiib
rename AQ hhsisa
rename AR hhsiob
rename AS hhsiot
rename AT hhsini
rename AU hhsits
rename AV hhsinf
rename AW hhsitt
rename Tenureofhousehold10OPDDw hhtnow
rename AY hhtnno
rename AZ hhtnft
rename Tenureofhousehold10OPDTo hhtnts
rename Tenureofhousehold10OPDNo hhtnnf
rename BC hhtntt
rename SectoroflandlordOPDPrivate hhslpv
rename SectoroflandlordOPDLocalA hhslla
rename SectoroflandlordOPDHousing hhslhnz
rename SectoroflandlordOPDOtherS hhsloso
rename SectoroflandlordOPDTotalh hhslts
rename SectoroflandlordOPDNotEls hhslnf
rename BJ hhsltt
rename HouseholdweeklyrentpaidOPD hhrp100l
rename BL hhrp100t149
rename BM hhrp150t199
rename BN hhrp200t249
rename BO hhrp250t299
rename BP hhrp300t349
rename BQ hhrp350p
rename BR hhrpts
rename BS hhrpnf
rename BT hhrptt
rename BU hhrpmed
rename Householdnumberofmotorvehicle hhmv0
rename BW hhmv1
rename BX hhmv2
rename BY hhmv3p
rename BZ hhmvts
rename CA hhmvnf
rename CB hhmvtt
rename Householdaccesstotelecommunica hhtlna
rename CD hhtlmp
rename CE hhtlll
rename CF hhtlfm
rename hhtlfm hhtlfx
rename CG hhtlin
rename CH hhtlts
rename CI hhtlnf
rename CJ hhtltt
save "/Users/stirkwang/Desktop/thesis/householdData.dta", replace

