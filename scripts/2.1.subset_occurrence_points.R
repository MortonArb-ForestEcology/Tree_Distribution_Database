############ 2.2
########### 4.2.18 Emily Beckman
############# subset occurrence point data to remove duplicates, etc.

############### INPUT: ____________FILL IN______________
#                       /cb_2016_us_county_5m/cb_2016_us_county_5m.shp
############### OUTPUT: occurrence_compiled_dec2_unique.csv
#                       occurrence_compiled_dec2_unique_countyDupMarked.csv
#                       occurrence_compiled_dec2_unique_countyDupRemoved.csv
#                       occurrence_compiled_dec2_unique_countyDupRemoved_acceptedDistMarked.csv

##############
### Functions
##############

## Subset data and (optionally) write a CSV
gen_subset <- function(orig_data, action, export_name){
  selected_rows <- (action)
  new <- orig_data[selected_rows,]
  if (missing(export_name)){
    return(data.frame(new))
  } else {
    write.csv(new, file = export_name)
    return(data.frame(new))
  }
}

###################
## 1. Read in Data
###################

occur_all <- read.csv(file=paste0(compiled, '/occurrence_compiled_post_geolocate_filled_dec2.csv'), as.is=T)
nrow(occur_all) #68237

###########################################
### 2. Remove Spatial Duplicates by Species
###########################################

# make sure coordinates have correct +/- sign
for(row in 1:nrow(occur_all)){
  if(occur_all$decimalLongitude[row] > 0){
    occur_all$decimalLongitude[row] <- occur_all$decimalLongitude[row]*(-1)
    print("pos")
  }
  if(occur_all$decimalLatitude[row] < 0){
    occur_all$decimalLatitude[row] <- abs(occur_all$decimalLatitude[row])
    print("neg")
  }
}
# round lat and long to 3 digits after decimal
occur_all$lat_round <- round(occur_all$decimalLatitude, 3)
occur_all$long_round <- round(occur_all$decimalLongitude, 3)
# before removing duplicates, let's number the occurrences so we know which ones will be saved
occur_all$obs_no <- seq(1, length(occur_all$X), 1)

# remove spatial duplicates based on species key and lat/long rounded to 3 digits after the decimal
occur_dec2_unq <- occur_all %>% distinct(speciesKey,lat_round,long_round,.keep_all=TRUE)
# make a new vector with these unique observations
first_match <- occur_dec2_unq$obs_no
# now we can return to our occur_all dataset and label the occurrences as duplicates or not
occur_all$duplicate <- "Duplicate" #41433 rows
occur_all$duplicate[first_match] <- "Unique" #8669 rows
table(occur_all$duplicate)
# count number of duplicates per unique occurrence
freq <- count(occur_all,vars = c("speciesKey","lat_round","long_round"))
occur_all <- join(freq,occur_all,by=c("speciesKey","lat_round","long_round"),type="right")
# and we can easily subset the duplicates and write a new file with the unique occurrences only
occur_dec2_unq <- occur_all[which(occur_all$duplicate == "Unique"),]
write.csv(occur_dec2_unq, file=paste0(compiled, "/occurrence_compiled_dec2_unique.csv"), row.names = F)

###########################################
### 3. Remove Spatial Duplicates by County
###########################################

# remove unnecessary county centroids (i.e. when higher quality occurrence point is already located within that county)
# load shapefile of US county boundaries
counties_map <- readOGR(paste0(one_up, '/cb_2016_us_county_5m/cb_2016_us_county_5m.shp'))
# project to WGS84
wgs84 <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
counties_wgs <- spTransform(counties_map, wgs84)
# turn occ data into SpatialPointsDataFrame
coordinates(occur_dec2_unq) <- c("decimalLongitude", "decimalLatitude")
proj4string(occur_dec2_unq) <- wgs84
# spatial join of occurrence points to counties shapefile
pts.poly.a <- point.in.poly(occur_dec2_unq, counties_wgs)
# back to a dataframe
occur_counties <- as.data.frame(pts.poly.a)
#test <- split(occur_counties,c(speciesKey,))

occur_counties <- occur_counties[order(occur_counties$gps_determ, decreasing = T),]
duplicates <- occur_counties[duplicated(occur_counties[c("speciesKey", "COUNTYNS")]),]
nrow(duplicates) #6235
to_remove <- duplicates[which(duplicates$gps_determ == "C" | duplicates$gps_determ == "SC"),]
nrow(to_remove) #188
to_remove$county_centroid_dup <- rep("x")
occur_dup_marked <- full_join(occur_counties, to_remove)
# write a file with these county duplicates marked
write.csv(occur_dup_marked, file=paste0(compiled, "/occurrence_compiled_dec2_unique_countyDupMarked.csv"), row.names = F)
occur_clean <- anti_join(occur_counties, to_remove, by = "obs_no")
nrow(occur_clean) #7616
# And write another file without the county duplicates
write.csv(occur_clean, file=paste0(compiled, "/occurrence_compiled_dec2_unique_countyDupRemoved.csv"), row.names = F)
# make smaller dataset for testing
#lobata <- occur_clean[which(occur_clean$species == "Quercus lobata"),]
#write.csv(lobata, file=paste0(compiled, "/occurrence_compiled_lobata_test.csv"), row.names = F)

# make dataframe of county centroids (lat and long)
centroids <- as.data.frame(centroid(counties_wgs))
colnames(centroids) <- c("centroid_long", "centroid_lat")
# round centroid lat and long to 3 digits after decimal
centroids$long_round <- round(centroids$centroid_long, 3)
centroids$lat_round <- round(centroids$centroid_lat, 3)
  str(centroids)
# join occurrence points to centroid dataframe based on rounded lat and long
occur_centroid_join <- join(occur_clean, centroids, by = c("long_round", "lat_round"), type="left", match = "first")
str(occur_centroid_join)
  unique(occur_centroid_join$gps_determ)
# mark records that may be centroids based on matching coordinates to centroid
occur_centroid_join$gps_determ <- ifelse(is.na(occur_centroid_join$centroid_long), occur_centroid_join$gps_determ, "SC")
  unique(occur_centroid_join$gps_determ)
# turn occurrence point data into a SpatialPointsDataFrame
coordinates(occur_centroid_join) <- c("decimalLongitude", "decimalLatitude")
proj4string(occur_centroid_join) <- wgs84
# spatial join of occurrence points to counties shapefile
pts.poly <- point.in.poly(occur_centroid_join, counties_wgs)


table(occur_clean$species)

############################
### Remove Unwanted Years
############################

## not using right now
# remove points recorded before 1950 (or whatever year cutoff you want)
#occur_dec2_1950 <- subset(occur_dec2,year>=1950)
#nrow(occur_dec2_1950) #6265
#unique(occur_dec2_1950$year)

############################################
### 4. Mark Records Outside 'Accepted' Range
############################################

# The accepted range refers to the maps created by the USDA and similar organizations.
# They list the counties in which each species has been recorded, so they offer a broad range.
accepted_dist <- subset(occur_clean, dataset == "usdaplants" | dataset == "bonap" | dataset == "natureserve")
  nrow(accepted_dist) #2643
# turn into SpatialPointsDataFrame
coordinates(accepted_dist) <- c("decimalLongitude", "decimalLatitude")
proj4string(accepted_dist) <- wgs84
# spatial join of occurrence points to counties shapefile
pts.poly.a <- point.in.poly(accepted_dist, counties_wgs)
# back to a dataframe
accepted_dist <- as.data.frame(pts.poly.a)
# keep only distinct counties and mark
accepted_dist_cty <- accepted_dist %>% distinct(COUNTYNS)
accepted_dist_cty$inside_accepted_dist <- "x"
  nrow(accepted_dist_cty) #414
# join marked counties to whole list (with dups removed)
accepted_dist_marked <- join(occur_clean, accepted_dist_cty, type="left", match = "first")
  str(accepted_dist_marked)
write.csv(accepted_dist_marked, file=paste0(compiled, "/occurrence_compiled_dec2_unique_countyDupRemoved_acceptedDistMarked.csv"), row.names = F)

# make smaller dataset for testing
lobata <- accepted_dist_marked[which(accepted_dist_marked$species == "Quercus lobata"),]
write.csv(lobata, file=paste0(compiled, "/occurrence_compiled_lobata_test.csv"), row.names = F)
