############ 2.2
########### 4.2.18 Emily Beckman
############# subset occurrence point data to remove duplicates, etc.

############### INPUT: Google Drive/Distributions_TreeSpecies/in-use_occurrence_compiled/occurence_raw_compiled.csv
############### OUTPUT:

################
### LIBRARIES and FUNCTIONS
################

library(sp)
library(rgdal)
library(spatialEco)
library(geosphere)
library(mapview)
library(data.table)
library(dplyr)

## Counts number of duplicates removed for each record
count.dups <- function(DF) { ### i dont think this is working, just says "1" for every record
  DT <- data.table(DF)
  DT[,.N,by=names(DT)]
}

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

################
### 1. Remove Spatial Duplicates by Species
################

# read in occurrence point file
occur_all <- read.csv(file=paste0(one_up, '/in-use_occurrence_compiled/occurrence_compiled_dec2.csv'), as.is=T)
# round lat and long to 3 digits after decimal
occur_all$lat_round <- round(occur_all$decimalLatitude, 3)
occur_all$long_round <- round(occur_all$decimalLongitude, 3)
# remove spatial duplicates based on species name and lat/long rounded to 2 digits after the decimal
occur_dec2_unq <- count.dups(occur_all)%>%distinct(species,lat_round,long_round,.keep_all=TRUE)
  nrow(occur_dec2_unq) #6579 with 2 dec places, 7923 with 3 dec places
  str(occur_dec2_unq)
write.csv(occur_dec2_unq, file=paste0(one_up, "/in-use_occurrence_compiled/occurrence_compiled_dec2_unique.csv"))

################
### 2. Remove Spatial Duplicates by County #I'm sure there is a way to compress this code, just did it stream of consciousness
################

# remove unnecessary county centroids (i.e. when higher quality occurrence point is already located within that county)

# load shapefile of US county boundaries
counties_map <- readOGR(paste0(one_up, '/cb_2016_us_county_5m/cb_2016_us_county_5m.shp'))
# project to WGS84
wgs84 <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
counties_wgs <- spTransform(counties_map, wgs84)
  #mapview(counties_wgs) # take a look if you want
# make dataframe of county centroids (lat and long)
centroids <- as.data.frame(centroid(counties_wgs))
colnames(centroids) <- c("centroid_long", "centroid_lat")
# round centroid lat and long to 3 digits after decimal
centroids$long_round <- round(centroids$centroid_long, 3)
centroids$lat_round <- round(centroids$centroid_lat, 3)
  str(centroids)
# join occurrence points to centroid dataframe based on rounded lat and long
occur_centroid_join <- join(occur_dec2_unq, centroids, by = c("long_round", "lat_round"), type="left", match = "first")
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
# mark occurrence points that are county centroids within counties that are already represented by geolocated points
occur_counties <- as.data.frame(pts.poly)
  nrow(occur_counties) #7123
duplicates <- occur_counties[duplicated(occur_counties[c(4,15)]),]
  nrow(duplicates) #6468
to_remove <- subset(duplicates, gps_determ == "C" | gps_determ == "SC")
  nrow(to_remove) #644
to_remove$county_centroid_dup <- rep("x")
occur_dup_marked <- full_join(occur_counties, to_remove)
write.csv(occur_dup_marked, file=paste0(one_up, "/in-use_occurrence_compiled/occurrence_compiled_dec2_unique_countyDupMarked.csv"))
# remove county centroid duplicate records
occur_clean <- anti_join(occur_counties, to_remove, by = "X")
  nrow(occur_clean) #6479
write.csv(occur_clean, file=paste0(one_up, "/in-use_occurrence_compiled/occurrence_compiled_dec2_unique_countyDupRemoved.csv"))

################
### 3. Remove Unwanted Years
################

# not using right now:
# remove points recorded before 1950 (or whatever year cutoff you want)
#occur_dec2_1950 <- subset(occur_dec2,year>=1950)
#nrow(occur_dec2_1950) #6265
#unique(occur_dec2_1950$year)

################
### 4. Mark Records Outside 'Accepted' Range
################

accepted_dist <- subset(occur_all, dataset == "usdaplants" | dataset == "bonap" | dataset == "natureserve")
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
write.csv(accepted_dist_marked, file=paste0(one_up, "/in-use_occurrence_compiled/occurrence_compiled_dec2_unique_countyDupRemoved_acceptedDistMarked.csv"))
