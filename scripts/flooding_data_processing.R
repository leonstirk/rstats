source('scripts/das_data_preprocessing.R')

## Remove student area units from analysis (homeowner_rate < 0.46) #
tmp <- 0.45
homeowner_rates<-sapply(levels(das$area_unit_name), function(x) {mean(das[which(das$area_unit_name == x),]$homeowner_rate)})
student_areas <- names(homeowner_rates[homeowner_rates<0.46])
student_areas <- student_areas[which(!student_areas %in% c("South Dunedin"))]
flood_sub <- das[which(!(das$area_unit_name %in% student_areas)),]
rm(tmp)
rm(student_areas)

## Remove harbour areas from st leonards to port chalmers
harbour_areas <- c("St Leonards-Blanket Bay","Sawyers Bay","Port Chalmers")
flood_sub <- flood_sub[which(!flood_sub$area_unit_name %in% harbour_areas),]
rm(harbour_areas)

################################
## Define area unit mb vectors #
################################

forbury_mbs       <- c(levels(as.factor(as.character(das$meshblock_id[which(das$area_unit_name == "Forbury")]))))
south_dunedin_mbs <- c(levels(as.factor(as.character(das$meshblock_id[which(das$area_unit_name == "South Dunedin")]))))
kilda_west_mbs    <- c(levels(as.factor(as.character(das$meshblock_id[which(das$area_unit_name == "St Kilda West")]))))
kilda_central_mbs <- c(levels(as.factor(as.character(das$meshblock_id[which(das$area_unit_name == "St Kilda Central")]))))
kilda_east_mbs    <- c(levels(as.factor(as.character(das$meshblock_id[which(das$area_unit_name == "St Kilda East")]))))


## Hand coded meshblocks

## 2927400
mb_2927400_obs       <- das[which(das$meshblock_id == 2927400),]
mb_2927400_exclude   <- levels(as.factor(mb_2927400_obs[which(mb_2927400_obs$physical_address %in% c(
                                                                                                       '49 Norfolk St',
                                                                                                       '51 Norfolk St',
                                                                                                       '41 Sandringham St',
                                                                                                       '3 Albert Street',
                                                                                                       '7 Albert Street',
                                                                                                       '9 Albert Street',
                                                                                                       '11 Albert Street',
                                                                                                       '57 Beach St',
                                                                                                       '59 Beach St',
                                                                                                       '61 Beach St',
                                                                                                       '75 Beach St',
                                                                                                       '77 Beach St'
                                                                                                   )),]$qpid))
mb_2927400_nonflood  <- levels(as.factor(mb_2927400_obs[which(!mb_2927400_obs$qpid %in% mb_2927400_exclude),]$qpid))


## 2926100
mb_2926100_obs       <- das[which(das$meshblock_id == 2926100),]
mb_2926100_exclude   <- levels(as.factor(mb_2926100_obs[which(mb_2926100_obs$physical_address %in% c(
                                                                                                        '2 Albert St',
                                                                                                        '4 Albert St',
                                                                                                        '6 Albert St',
                                                                                                        '8 Albert St',
                                                                                                        '16 Albert St',
                                                                                                        '16A Albert St',
                                                                                                        '31 Beach St',
                                                                                                        '33 Beach St',
                                                                                                        '35 Beach St',
                                                                                                        '39 Beach St',
                                                                                                        '45 Beach St',
                                                                                                        '54 Bedford Street',
                                                                                                        '56 Bedford Street',
                                                                                                        '62 Bedford Street',
                                                                                                        '68 Bedford Street',
                                                                                                        '72 Bedford Street',
                                                                                                        '74 Bedford Street',
                                                                                                        '69 Norflk St'
                                                                                                    )),]$qpid))
mb_2926100_nonflood  <- levels(as.factor(mb_2926100_obs[which(!mb_2926100_obs$qpid %in% mb_2926100_exclude),]$qpid))


## 2947300
mb_2947300_obs       <- das[which(das$meshblock_id == 2947300),]
mb_2947300_nonflood  <- levels(as.factor(mb_2947300_obs[which(mb_2947300_obs$physical_address %in% c(
                                                                                                       '140 Victoria Rd',
                                                                                                       '21 Plunket St',
                                                                                                       '23 Plunket St',
                                                                                                       '25 Plunket St',
                                                                                                       '29 Plunket St',
                                                                                                       '31 Plunket St',
                                                                                                       '33 Plunket St',
                                                                                                       '35 Plunket St',
                                                                                                       '37A Plunket St',
                                                                                                       '37B Plunket St',
                                                                                                       '39 Plunket St',
                                                                                                       '41 Plunket St',
                                                                                                       '41A Plunket St',
                                                                                                       '43 Plunket St',
                                                                                                       '45 Plunket St',
                                                                                                       '45A Plunket St',
                                                                                                       '45B Plunket St',
                                                                                                       '47 Plunket St',
                                                                                                       '49 Plunket St',
                                                                                                       '51 Plunket St',
                                                                                                       '62 Moreau St',
                                                                                                       '64 Moreau St',
                                                                                                       '66 Moreau St',
                                                                                                       '68 Moreau St',
                                                                                                       '70 Moreau St',
                                                                                                       '72 Moreau St',
                                                                                                       '74 Moreau St',
                                                                                                       '76 Moreau St',
                                                                                                       '78 Moreau St',
                                                                                                       '80 Moreau St',
                                                                                                       '82 Moreau St',
                                                                                                       '84 Moreau St',
                                                                                                       '86 Moreau St',
                                                                                                       '86A Moreau St',
                                                                                                       '86B Moreau St'
                                                                                                   )),]$qpid))
mb_2947300_flood     <- levels(as.factor(mb_2947300_obs[which(!mb_2947300_obs$qpid %in% mb_2947300_nonflood),]$qpid))


##########################################
## Define meshblock vectors by area unit #
##########################################

st_clair_flood_mb         <- c('2926900','2927000')
## st_clair_nonflood_mb      <- c('2926100','2927400')

south_dunedin_nonflood_mb <- c('2931000', as.character(seq(2931400,2931800,100)), as.character(seq(2930300,2930900,100)))
south_dunedin_exclude_mb  <- c('2931200','2931300')
south_dunedin_ab_mb       <- c('2932300','2932400')
south_dunedin_flood_mb    <- south_dunedin_mbs[which(!(south_dunedin_mbs %in% c(south_dunedin_nonflood_mb, south_dunedin_exclude_mb, south_dunedin_ab_mb)))]


kilda_east_nonflood_mb    <- c(as.character(seq(2950000,2950900,100)),'2951000','2951100',as.character(seq(2951400,2951700,100)),'2952100','2952200')
kilda_east_ab_mb          <- c('2951801','2951802','2951900','2952001','2952002','2952003','2952300','2952400','2952500','2933202','2933203','2933204')
kilda_east_exclude_mb     <- kilda_east_mbs[which(!(kilda_east_mbs %in% c(kilda_east_nonflood_mb,kilda_east_ab_mb)))]


kilda_central_flood_mb    <- c(as.character(seq(2947500,2947800,100)),as.character(seq(2948000,2948300,100)))
kilda_central_exclude_mb  <- c('2949900')
kilda_central_ab_mb       <- c('2947900')
kilda_central_nonflood_mb <- kilda_central_mbs[which(!(kilda_central_mbs %in% c(kilda_central_flood_mb,kilda_central_exclude_mb,kilda_central_ab_mb)))]


kilda_west_nonflood_mb    <- c('2945700',as.character(seq(2946100,2946600,100)),'2947100')
kilda_west_exclude_mb     <- c('2947400','2947300')
kilda_west_flood_mb       <- kilda_west_mbs[which(!(kilda_west_mbs %in% c(kilda_west_nonflood_mb,kilda_west_exclude_mb)))]

caversham_nonflood_mb     <- c('2913300','2913400','2913500','2913600','2914800','2914900')

##########
## Group #
#########

flood_mbs                 <- c(forbury_mbs,st_clair_flood_mb,south_dunedin_flood_mb,kilda_central_flood_mb,kilda_west_flood_mb)
nonflood_mbs              <- c(south_dunedin_nonflood_mb,kilda_east_nonflood_mb,kilda_central_nonflood_mb,kilda_west_nonflood_mb,caversham_nonflood_mb)

flood_obs                 <- c(mb_2947300_flood)
nonflood_obs              <- c(mb_2947300_nonflood,mb_2926100_nonflood,mb_2927400_nonflood)

forbury_mb_vec            <- c(flood_mbs, south_dunedin_ab_mb, kilda_central_ab_mb)
tainui_mb_vec             <- c(nonflood_mbs)

########################
## Assign area dummies #
########################

das$flooded     <- ifelse(das$meshblock_id %in% forbury_mb_vec | das$qpid %in% flood_obs,1,0)
das$tainui      <- ifelse(das$meshblock_id %in% tainui_mb_vec | das$qpid %in% nonflood_obs,1,0)

das$flood_prone <- ifelse(das$flooded == 1 | das$tainui == 1,1,0)
das$rest_of_dud <- ifelse(das$flood_prone == 1,0,1)

das$flood_analysis_group <- as.factor(das$rest_of_dud + das$tainui*2 + das$flooded*3)
tmp <- levels(das$flood_analysis_group)
das$flood_analysis_group <- mapvalues(das$flood_analysis_group, from = tmp, to = c("rest_of_dud", "tainui", "flooded"))
rm(tmp)

######################				   
## Set time blocking #
######################

flood_date <- as.Date('2015-06-03')
years <- 3
days <- years*365
start_date <- flood_date - days
end_date <- flood_date + days

flood_sub <- subset(flood_sub,sale_date>start_date & sale_date<end_date)
flood_sub$after_flood <- ifelse(flood_sub$sale_date<flood_date,0,1)
flood_sub$after_flood <- as.factor(flood_sub$after_flood)

