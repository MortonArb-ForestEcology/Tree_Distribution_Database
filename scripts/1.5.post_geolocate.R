############ 1.4
########### 5.20.18 Elizabeth Tokarz & Emily Beckman
#############

## Be sure to run "set_workingdirectory.R" before running this script

############### INPUT:
############### OUTPUT:


###################
## 1. Read in Data
###################

# records put through GeoLocate (gbif & consortium)
post_geo <- read.csv(file='gbif_consortium_DC_georef_hasLocalANDnoCoord_geolocated.csv', as.is=T, na.strings=c("","NA"))
nrow(post_geo) #3990
# rename columns to Darwin Core
setnames(post_geo,
         old=c("latitude","longitude","country","state"),
         new=c("decimalLatitude","decimalLongitude","countryCode","stateProvince"))

# records before input to GeoLocate (gbif & consortium)
pre_geo <- read.csv(file=paste0(compiled, '/occurrence_compiled_refined_for_geolocate.csv'), as.is=T, na.strings=c("","NA"))
pre_geo$gps_determ <- "NA"
pre_geo$precision <- "NA"
nrow(pre_geo) #17263

# records not put through GeoLocate
no_geo <- read.csv(file=paste0(compiled, '/occurrence_compiled_refined_no_geolocate.csv'), as.is=T, na.strings=c("","NA"))
nrow(no_geo) #50974
table(no_geo$gps_determ)
no_geo$gps_determ <- ifelse(is.na(no_geo$gps_determ),"NA",no_geo$gps_determ)
unique(no_geo$gps_determ)
no_geo$precision <- "NA"

###################################################
## 2. Assign GPS Determination and Compile All Data
###################################################

# fill pre_geo dataset with coordinates found through geolocate
post_geo_all <- pre_geo
test <- pre_geo[which(is.na(pre_geo$decimalLatitude)),]
nrow(test) #8613
post_geo_succ <- post_geo[which(!is.na(post_geo$decimalLatitude)),]
obs <- post_geo_succ$obs_no
lat <- post_geo_succ$decimalLatitude
long <- post_geo_succ$decimalLongitude
prec <- post_geo_succ$precision
for (i in 1:length(obs)){
  row <- grep(pattern = obs[i], x = post_geo_all$obs_no)
  post_geo_all$decimalLatitude[row] <- lat[i]
  post_geo_all$decimalLongitude[row] <- long[i]
  post_geo_all$gps_determ[row] <- "L"
  post_geo_all$precision[row] <- prec[i]
}
test2 <- post_geo_all[which(is.na(post_geo_all$decimalLatitude)),]
nrow(test2) #6248
table(post_geo_all$gps_determ)

# join occurrences for geolocate and those not run through it
all_occ <- join(post_geo_all,no_geo,type="full")
nrow(all_occ) #68237 -- yay!

# reorder the occurrences here according to their year and precision (given by geolocate)
all_occ <- all_occ[order(all_occ$year, decreasing = T), ]
# order by precision
high <- as.numeric(grep(all_occ$precision, pattern = "High"))
medium1 <- as.numeric(grep(all_occ$precision, pattern = "Medium"))
medium2 <- as.numeric(grep(all_occ$precision, pattern = "medium"))
low <- as.numeric(grep(all_occ$precision, pattern = "Low"))
blank <- as.numeric(which(all_occ$precision == "NA"))
# we can concatenate these row numbers to create the order in which we want the occurrences
precise_order <- c(high, medium1, medium2, low, blank)
all_occ <- all_occ[precise_order, ]
nrow(all_occ)

# write a new dataset
write.csv(all_occ, file=paste0(compiled, '/occurrence_compiled_post_geolocate.csv'), row.names = F)

###########################
## 3. Fill County Centroids
###########################

# fill in county centroid coordinates where necessary and possible
# read in county and state vectors from the FIA county reference file from the FIA datamart.
fia_cou <- read.csv(file=paste0(translate_fia, '/fia_county_raw.csv'), as.is=T)
# read in csv with county centroid coordinates
counties <- read.csv(file='counties_wgs.csv', as.is=T)
setnames(fia_cou,
         old=c("STATECD","COUNTYCD"),
         new=c("STATEFP","COUNTYFP"))
centroids <- join(counties, fia_cou, by = c('STATEFP','COUNTYFP'), type = 'full')
state_names <- centroids$STATENM
county_names <- centroids$COUNTYNM
lat <- centroids$CENTROID_Y
long <- centroids$CENTROID_X

# create dataframe of only records without coordinates
no_coord <- all_occ[which(is.na(all_occ$decimalLatitude)),]
nrow(no_coord) #12294
no_coord$gps_determ <- "NA"
# and one of only records with cordinates
have_coord <- all_occ[which(!is.na(all_occ$decimalLatitude)),]
nrow(have_coord) #55943

# place county centroid coordinates in missing lat/long where possible
extract_county_centroid <- function(df, states, counties, lat, long){
  s_look <- grep(pattern = states, x = df$state_new, ignore.case=T)
  c_look <- grep(pattern = counties, x = df$county_new, ignore.case = T)
  overlap <- intersect(s_look, c_look)
  df$decimalLatitude[overlap] <- lat
  df$decimalLongitude[overlap] <- long
  df$gps_determ[overlap] <- "C"
  return(df)
}
for (i in 1:length(county_names)){
  no_coord <- extract_county_centroid(no_coord, state_names[i], county_names[i], lat[i], long[i])
}
# see how many rows were filled with county centroid
filled <- no_coord[which(!is.na(no_coord$decimalLatitude)),]
nrow(filled) #8826

# join data back together
occur_all <- join(no_coord,have_coord,type="full")
write.csv(occur_all, file=paste0(compiled, '/occurrence_compiled_post_geolocate_filled.csv'), row.names = F)

#####################
## 5. Finishing Touches
#####################

# remove rows with no lat/long
occur_all <- occur_all[which(!is.na(occur_all$decimalLatitude)),]
nrow(occur_all) #64769
unique(occur_all$decimalLatitude)

# remove points with fewer than 2 digits after the decimal for lat and/or long
occur_dec2 <- occur_all[grep("\\.[0-9][1-9]",occur_all$decimalLatitude),]
nrow(occur_dec2) #56492
occur_dec2 <- occur_dec2[grep("\\.[0-9][1-9]",occur_dec2$decimalLongitude),]
nrow(occur_dec2) #50102

# reorder dataset before subsetting in next script to place higher quality datasets and most recent records first
occur_dec2 <- occur_dec2[order(factor(occur_dec2$dataset,levels=c("other","redlist","fia","exsitu","consortium","gbif",
                                                                  "hipp","natureserve","bonap","usdaplants"))),]
head(occur_dec2)
occur_dec2 <- occur_dec2[order(occur_dec2$year, na.last = TRUE, decreasing = T),]
unique(occur_dec2$year)

write.csv(occur_dec2, file=paste0(compiled, '/occurrence_compiled_post_geolocate_filled_dec2.csv'), row.names = F)
