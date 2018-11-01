source('scripts/das_data_preprocessing.R')

## Remove student area units from analysis (homeowner_rate < 0.46) #
tmp <- 0.45
homeowner_rates<-sapply(levels(das$area_unit_name), function(x) {mean(das[which(das$area_unit_name == x),]$homeowner_rate)})
student_areas <- names(homeowner_rates[homeowner_rates<0.46])
student_areas <- student_areas[which(!student_areas %in% c("South Dunedin"))]
das <- das[which(!(das$area_unit_name %in% student_areas)),]
rm(tmp)

## Remove harbour areas from ravensbourne to port chalmers
harbour_areas <- c("St Leonards-Blanket Bay","Sawyers Bay","Port Chalmers")
das <- das[which(!das$area_unit_name %in% harbour_areas),]

######################################
## Define rough area unit boundaries #
######################################

flood_analysis_forbury <- levels(as.factor(as.character(das$meshblock_id[which(das$area_unit_name %in% c("Forbury","South Dunedin","St Kilda West","St Kilda Central"))])))
flood_analysis_tainui <- levels(as.factor(as.character(das$meshblock_id[which(das$area_unit_name %in% c("St Kilda East"))])))

##########################################
## Tidy up boundaries at meshblock level #
##########################################

## St Clair flooded vector
st_clair_flood_mb <- c('2926900','2927000')

## St Clair not flooded vector
st_clair_nonflood_mb <- c('2926200','2926100','2927400')

## South Dunedin vector
south_dunedin_mb <- c('2931000', as.character(seq(2931200,2931800,100)),as.character(seq(2930300,2930900,100)))

## St Kilda West Vector
kilda_west_mb <- c('2946100','2946500','2946600','2946700','2945700','2947100','2947200','2947400')

## St Kilda Central Vector
kilda_central_mb <- c(as.character(seq(2948700,2949900,100)))

## St Kilda East Vector
kilda_east_mb <- c('2952600')

## Musselburgh Vector
musselburgh_mb <- c(as.character(seq(2934000,2934700,100),'2935400'))

## HAND CODE HOUSES IN MB 2947300 #

flood_analysis_forbury <- c(flood_analysis_forbury[which(!(flood_analysis_forbury %in% c(kilda_west_mb, kilda_central_mb, south_dunedin_mb)))],st_clair_flood_mb)
flood_analysis_tainui <- c(flood_analysis_tainui[which(!(flood_analysis_tainui %in% c(kilda_east_mb)))],musselburgh_mb,south_dunedin_mb,kilda_west_mb,st_clair_nonflood_mb,kilda_central_mb)

##################################
## Assign treatment dummy to das #
##################################

das$flooded <- ifelse(das$meshblock_id %in% flood_analysis_forbury,1,0)
das$flood_prone <- ifelse(das$meshblock_id %in% c(flood_analysis_forbury, flood_analysis_tainui),1,0)
das$tainui <- ifelse(das$meshblock_id %in% flood_analysis_tainui,1,0)

######################				   
## Set time blocking #
######################

flood_date <- as.Date('2015-06-03')
years <- 3
days <- years*365
start_date <- flood_date - days
end_date <- flood_date + days

flood_sub <- subset(das,sale_date>start_date & sale_date<end_date)
flood_sub$after_flood <- ifelse(flood_sub$sale_date<flood_date,0,1)
flood_sub$after_flood <- as.factor(flood_sub$after_flood)
