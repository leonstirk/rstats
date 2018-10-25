source('scripts/das_data_preprocessing.R')

######################################
## Define rough area unit boundaries #
######################################

flood_analysis_treatment <- levels(as.factor(as.character(das$meshblock_id[which(das$area_unit_name %in% c("Forbury","South Dunedin","St Kilda West","St Kilda Central"))])))
flood_analysis_control <- levels(as.factor(as.character(das$meshblock_id[which(das$area_unit_name %in% c("St Kilda East"))])))

##########################################
## Tidy up boundaries at meshblock level #
##########################################

## St Clair vector
st_clair_mb <- c('2926900','2927000')

## South Dunedin vector
south_dunedin_mb <- c('2931000', as.character(seq(2931200,2931800,100)),as.character(seq(2930300,2930900,100)))

## St Kilda West Vector
kilda_west_mb <- c('2946100','2946500','2946700','2945700','2947100','2947200','2947400')

## St Kilda Central Vector
kilda_central_mb <- c(as.character(seq(2948700,2949900,100)))

## St Kilda East Vector
kilda_east_mb <- c('2952600')

## Musselburgh Vector
musselburgh_mb <- c(as.character(seq(2934000,2934700,100),'2935400'))

flood_analysis_treatment <- c(flood_analysis_treatment[which(!(flood_analysis_treatment %in% c(kilda_west_mb, kilda_central_mb, south_dunedin_mb)))],st_clair_mb)
flood_analysis_control <- c(flood_analysis_control[which(!(flood_analysis_control %in% c(kilda_east_mb)))],musselburgh_mb,south_dunedin_mb)

##################################
## Assign treatment dummy to das #
##################################

das$flooded <- ifelse(das$meshblock_id %in% flood_analysis_treatment,1,0)
das$flood_prone <- ifelse(das$meshblock_id %in% flood_analysis_control,1,0)

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

############################
## Add arterial road dummy #
############################

arterial_road_vec <- c("Andersons Bay Road","Bay View Road","Forbury Road","Hillside Road","King Edward Street","Macandrew Road","Musselburgh Rise","Prince Albert Road","Queens Drive","Victoria Road")
flood_sub$arterial_street <- ifelse(flood_sub$full_roa %in% arterial_road_vec,1,0)

