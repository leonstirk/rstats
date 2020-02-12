##############################################################################################################################

source('scripts/das_data_preprocessing.R')

##############################################################################################################################

homeowner_rates <- sapply(levels(das$area_unit_name), function(x) {mean(das[which(das$area_unit_name == x),]$homeowner_rate)})
dist_cbds <- sapply(levels(das$area_unit_name), function(x) {mean(das[which(das$area_unit_name == x),]$dist_cbd)})

returnAboveThreshold <- function(vec, threshold, exclude) {
  names <- names(vec[vec < threshold])
  return(names[!names %in% exclude])
}

returnBelowThreshold <- function(vec, threshold, exclude) {
  names <- names(vec[vec > threshold])
  return(names[!names %in% exclude])
}

ho_threshold <- 0.46
dist_threshold <- 5

## Get a vector of student areas by au_name (homeowner_rate < 0.46) (adding "South Dunedin" back in to analysis) #
student_areas <- returnAboveThreshold(homeowner_rates, ho_threshold, c('South Dunedin'))

## Get a vector of harbour areas from st leonards to port chalmers (dist_cbd > 5km) #
harbour_areas <- returnBelowThreshold(dist_cbds, dist_threshold, c())

## Remove student_areas and harbour_areas from analysis #
das <- das[which(!(das$area_unit_name %in% c(student_areas, harbour_areas))),]

## Remove that one map outlier way out north of Forrester Park
das <- das[which(!das$qpid == 1392904),]

## plot the excuded area_units
tmp <- as.data.frame(cbind(area_unit_name = names(homeowner_rates), homeowner_rates, dist_cbds, excluded = ifelse(names(homeowner_rates) %in% c(harbour_areas, student_areas), 1, 0)))
tmp[2:3] <- lapply(tmp[2:3], function(x){as.numeric(as.character(x))})
tmp$excluded <- relevel(tmp$excluded, "1")

tmp <- transform(tmp, area_unit_name=reorder(area_unit_name, -homeowner_rates))
splot <- ggplot(tmp, aes(area_unit_name, homeowner_rates)) + geom_bar(stat = "identity", aes(fill = excluded)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_hline(yintercept = ho_threshold) + scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.46, 0.6, 0.8))

tmp <- transform(tmp, area_unit_name=reorder(area_unit_name, -dist_cbds))
hplot <- ggplot(tmp, aes(area_unit_name, dist_cbds)) + geom_bar(stat = "identity", aes(fill = excluded)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_hline(yintercept = dist_threshold) + scale_y_continuous(breaks = c(0, 2, 4, 5, 6, 8, 10))

## multiplot(
##     splot,
##     hplot,
##     cols = 2
##     )

rm(tmp, ho_threshold, dist_threshold)

################################################################################
## Map the area units and do something with it


## library(rgdal) # for readOGR
## library(sp)    # for spplot

## # Reading shapefile
## cau = readOGR(dsn='/Users/lap44/Dropbox/research/2015/census/2014 Digital Boundaries Generlised Clipped', layer='AU2014_GV_Clipped')

## # Joining with school ethnicity data (notice we refer to @data, as cau contains spatial info as well)
## cau@data = data.frame(cau@data, hhi[match(cau@data[,"AU2014_NAM"], hhi[,"Census.Area.Unit"]),])

## # Limiting map to the area around Christchurch
## spplot(cau, zcol = "hn", xlim = c(1540000, 1590000), ylim= c(5163000, 5198000))

################################################################################


rm(student_areas, harbour_areas, homeowner_rates, dist_cbds)

################################
## Define area unit mb vectors #
################################

getMBsByAU <- function(data, au_name) {
  mb_vec <- c(levels(as.factor(as.character(data$meshblock_id[which(data$area_unit_name == au_name)]))))
  return(mb_vec)
}

getObsByMB <- function(data, mb_id) {
  obs <- data[which(data$meshblock_id == mb_id),]
  return(obs)
}

getQPIDsFromAddresses <- function(mb_obs, address_vec) {
  QPIDs <- levels(as.factor(mb_obs[which(mb_obs$physical_address %in% address_vec),]$qpid))
  return(QPIDs)
}

exclude <- function(data, mb_id, address_names) {
  mb_obs <- getObsByMB(data, mb_id)
  address_qpids <- getQPIDsFromAddresses(mb_obs, address_names)
  return(levels(as.factor(mb_obs[which(!mb_obs$qpid %in% address_qpids),]$qpid)))
}

forbury_mbs       <- getMBsByAU(das, "Forbury")
south_dunedin_mbs <- getMBsByAU(das, "South Dunedin")
kilda_west_mbs    <- getMBsByAU(das, "St Kilda West")
kilda_central_mbs <- getMBsByAU(das, "St Kilda Central")
kilda_east_mbs    <- getMBsByAU(das, "St Kilda East")

##########################
## Hand coded meshblocks #
##########################

## c(2927400, 2926100, 2947300, 2927000, 2948300) #

###########################################################################################
## 2927400 - Partial meshblock in St Clair that I have included in the flood_prone subset #
###########################################################################################

## Vector of non-flood-prone (NFP) addresses to be excluded from the vector of QPIDs being added to the flood_prone subset #
address_names <- c('49 Norfolk St','51 Norfolk St','41 Sandringham St','3 Albert St','7 Albert St','9 Albert St','11 Albert St','57 Beach St','59 Beach St','61 Beach St','75 Beach St','77 Beach St')

mb_2927400_nonflood <- exclude(das, 2927400, address_names)

###########################################################################################
## 2926100 - Partial meshblock in St Clair that I have included in the flood_prone subset #
###########################################################################################

address_names <- c('2 Albert St', '4 Albert St', '6 Albert St', '8 Albert St', '16 Albert St', '16A Albert St', '31 Beach St', '33 Beach St', '35 Beach St', '39 Beach St', '45 Beach St', '54 Bedford St', '56 Bedford St', '62 Bedford St', '68 Bedford St', '72 Bedford St', '74 Bedford St', '69 Norflk St')

mb_2926100_nonflood  <- exclude(das, 2926100, address_names)

################################################################################################
## 2947300 - St Kilda West meshblock that contains both 'flooded' and 'flood_prone' properties #
################################################################################################

## This address_names vector contains properties that fall into the 'flood_prone' but not 'flooded' category (i.e. 'non-flood') #
address_names <- c('140 Victoria Rd','21 Plunket St','23 Plunket St','25 Plunket St','27 Plunket St','29 Plunket St','31 Plunket St','33 Plunket St','35 Plunket St','37A Plunket St','37B Plunket St','39 Plunket St','41 Plunket St','41A Plunket St','43 Plunket St','45 Plunket St','45A Plunket St','45B Plunket St','47 Plunket St','49 Plunket St','51 Plunket St','62 Moreau St','64 Moreau St','66 Moreau St','68 Moreau St','70 Moreau St','72 Moreau St','74 Moreau St','76 Moreau St','78 Moreau St','80 Moreau St','82 Moreau St','84 Moreau St','86 Moreau St','86A Moreau St','86B Moreau St')

mb_2947300_flood <- exclude(das, 2947300, address_names)
obs <- getObsByMB(das, 2947300)
mb_2947300_nonflood <- levels(as.factor(obs[which(!obs$qpid %in% mb_2947300_flood),]$qpid))

###########################################################################################
## 2927000 - St Clair meshblock that contains both 'flooded' and 'flood_prone' properties #
###########################################################################################

## This address_names vector contains properties that fall into the 'flood_prone' but not 'flooded' category (i.e. 'non-flood') #
address_names <- c('2 Wilson Ave','117 Forbury Rd','119 Forbury Rd','119A Forbury Rd','119B Forbury Rd','119C Forbury Rd','121 Forbury Rd','121A Forbury Rd','121B Forbury Rd','121C Forbury Rd','123 Forbury Rd','125 Forbury Rd','139 Forbury Rd','141 Forbury Rd','145 Forbury Rd','147 Forbury Rd','149 Forbury Rd','392 Bayview Rd','394 Bayview Rd','394A Bayview Rd','394B Bayview Rd','394C Bayview Rd','396 Bayview Rd','402 Bayview Rd','404 Bayview Rd','408 Bayview Rd')

mb_2927000_flood <- exclude(das, 2927000, address_names)
obs <- getObsByMB(das, 2927000)
mb_2927000_nonflood <- levels(as.factor(obs[which(!obs$qpid %in% mb_2927000_flood),]$qpid))

##################################################################################################
## 2948300 - St Kild central meshblock that contains both 'flooded' and 'flood_prone' properties #
##################################################################################################

## This address_names vector contains properties that fall into the 'flood_prone' but not 'flooded' category (i.e. 'non-flood') #
address_names <- c('5 Richardson St','7 Richardson St','9 Richardson St','9A Richardson St','9B Richardson St','11 Richardson St','13 Richardson St','15 Richardson St','17 Richardson St','19 Richardson St','21 Richardson St','68 Prince Albert Rd','74 Prince Albert Rd','80 Prince Albert Rd','82 Prince Albert Rd','2 Scott St','6 Scott St','8 Scott St','10 Scott St','12 Scott St','14 Scott St','16 Scott St','18 Scott St','20 Scott St','22 Scott St','26 Ajax St','28 Ajax St')

mb_2948300_flood <- exclude(das, 2948300, address_names)
obs <- getObsByMB(das, 2948300)
mb_2948300_nonflood <- levels(as.factor(obs[which(!obs$qpid %in% mb_2948300_flood),]$qpid))

##################################################################################################

# cleanup temporary variables
rm(address_names, obs)

# cleanup temporary functions
rm(getMBsByAU, getObsByMB, getQPIDsFromAddresses, exclude)

##########################################
## Define meshblock vectors by area unit #
##########################################

st_clair_flood_mb         <- c('2926900')

south_dunedin_nonflood_mb <- c('2931000', as.character(seq(2931400,2931800,100)), as.character(seq(2930300,2930900,100)))
south_dunedin_exclude_mb  <- c('2931200','2931300')
south_dunedin_flood_mb    <- south_dunedin_mbs[which(!(south_dunedin_mbs %in% c(south_dunedin_nonflood_mb, south_dunedin_exclude_mb)))]

kilda_east_exclude_mb     <- c('2933201')
kilda_east_nonflood_mb    <- kilda_east_mbs[which(!kilda_east_mbs %in% c(kilda_east_exclude_mb))]

musselburgh_nonflood_mb  <- c(as.character(seq(2934100,2934700,100)),'2935400')

kilda_central_flood_mb    <- c(as.character(seq(2947500,2947900,100)),as.character(seq(2948000,2948200,100)))
kilda_central_exclude_mb  <- c('2949900', '2948300')
kilda_central_nonflood_mb <- kilda_central_mbs[which(!(kilda_central_mbs %in% c(kilda_central_flood_mb,kilda_central_exclude_mb)))]

kilda_west_nonflood_mb    <- c('2945700','2946100','2946500','2946600','2947100')
kilda_west_exclude_mb     <- c('2947400','2947300')
kilda_west_flood_mb       <- kilda_west_mbs[which(!(kilda_west_mbs %in% c(kilda_west_nonflood_mb,kilda_west_exclude_mb)))]

caversham_nonflood_mb     <- c('2913400','2913500','2913600','2914800','2914900','2914500','2914400')

##########
## Group #
#########

flood_mbs                 <- c(forbury_mbs,st_clair_flood_mb,south_dunedin_flood_mb,kilda_central_flood_mb,kilda_west_flood_mb)
nonflood_mbs              <- c(south_dunedin_nonflood_mb,kilda_east_nonflood_mb,kilda_central_nonflood_mb,kilda_west_nonflood_mb,caversham_nonflood_mb, musselburgh_nonflood_mb)

flood_obs                 <- c(mb_2947300_flood, mb_2948300_flood, mb_2927000_flood)
nonflood_obs              <- c(mb_2947300_nonflood, mb_2926100_nonflood, mb_2927400_nonflood, mb_2948300_nonflood, mb_2927000_nonflood)

## cleanup temporary variables
rm(forbury_mbs, south_dunedin_mbs, kilda_west_mbs, kilda_central_mbs, kilda_east_mbs)

rm(st_clair_flood_mb,south_dunedin_nonflood_mb,south_dunedin_exclude_mb,south_dunedin_flood_mb,kilda_east_exclude_mb,kilda_east_nonflood_mb,musselburgh_nonflood_mb,kilda_central_flood_mb,kilda_central_exclude_mb,kilda_central_nonflood_mb,kilda_west_nonflood_mb,kilda_west_exclude_mb,kilda_west_flood_mb,caversham_nonflood_mb)

rm(mb_2927400_nonflood, mb_2926100_nonflood, mb_2947300_flood, mb_2947300_nonflood, mb_2927000_flood, mb_2927000_nonflood, mb_2948300_flood, mb_2948300_nonflood)

########################
## Assign area dummies #
########################

das$flood           <- ifelse(das$meshblock_id %in% flood_mbs | das$qpid %in% flood_obs,1,0)
das$non_flood       <- ifelse(das$meshblock_id %in% nonflood_mbs | das$qpid %in% nonflood_obs,1,0)

das$flood_prone     <- ifelse(das$flood == 1 | das$non_flood == 1,1,0)
das$non_flood_prone <- ifelse(das$flood_prone == 1,0,1)

das$flood_analysis_group <- as.factor(das$non_flood_prone + das$non_flood*2 + das$flood*3)
das$flood_analysis_group <- mapvalues(das$flood_analysis_group, from = levels(das$flood_analysis_group), to = c("non_flood_prone", "non_flood", "flood"))

flood_vars <- c("flood", "non_flood", "flood_prone", "non_flood_prone")
das[flood_vars] <- lapply(das[flood_vars], as.factor)

rm(flood_mbs, nonflood_mbs, flood_obs, nonflood_obs)


## Describe generated variables
ht <- hux(
    "Variable name" = c('flood', 'non_flood', 'flood_prone', 'non_flood_prone', 'after_flood'),
    Description = c('Properties located in areas with measurable above ground ponding during the 4th June floods',
                    'Properties located on the South Dunedin plain in areas that did not experience measurable surface ponding',
                    'The sum of the properties in the previous two groups i.e. properties in the "flood" group or "non_flood" group',
                    'Properties not located on the South Dunedin plain',
                    'Transactions occuring after the 4th June 2015'),
    add_colnames = TRUE
)

bottom_border(ht)[1,]  <- 2
right_padding(ht)      <- 10
left_padding(ht)       <- 10
width(ht)              <- 1
col_width(ht)          <- c(0.3, 0.7)
valign(ht)             <- 'bottom'
flood_variables        <- map_wrap(ht, by_cols(FALSE, TRUE))
rm(ht)

## Crosstab, frequencies by generated variables
floodVariablesSummary <- function(das) {
    ht <- hux(
        "Variable names" = c('flood', 'non_flood', 'flood_prone', 'non_flood_prone', 'Total'),
        before_flood = c(length(which(das[which(das$after_flood == 0), "flood"] == 1)),
                         length(which(das[which(das$after_flood == 0), "non_flood"] == 1)),
                         length(which(das[which(das$after_flood == 0), "flood_prone"] == 1)),
                         length(which(das[which(das$after_flood == 0), "non_flood_prone"] == 1)),
                         length(which(das$after_flood == 0))
                         ),
        after_flood = c(length(which(das[which(das$after_flood == 1), "flood"] == 1)),
                        length(which(das[which(das$after_flood == 1), "non_flood"] == 1)),
                        length(which(das[which(das$after_flood == 1), "flood_prone"] == 1)),
                        length(which(das[which(das$after_flood == 1), "non_flood_prone"] == 1)),
                        length(which(das$after_flood == 1))
                        ),
        Total = c(length(which(das$flood == 1)),
                  length(which(das$non_flood == 1)),
                  length(which(das$flood_prone == 1)),
                  length(which(das$non_flood_prone == 1)),
                  nrow(das)
                  ),
        add_colnames = TRUE
    )

    bottom_border(ht)[1,]  <- 2
    bottom_border(ht)[c(3,5),2:4] <- 1
    align(ht)[,2:3]        <- 'right'
    right_padding(ht)      <- 10
    left_padding(ht)       <- 10
    number_format(ht)      <- 0
    width(ht)              <- 1
    col_width(ht)          <- rep(1/4,4)
    ht[1,1]                <- ""
    return(ht)
}

flood_variables_summary <- floodVariablesSummary(das)

tc_descriptives <- tcDescriptives(das, "flood_prone", des_vars)
