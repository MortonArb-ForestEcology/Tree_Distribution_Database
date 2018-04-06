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

# counts number of duplicates removed for each record
count.dups <- function(DF) {
  DT <- data.table(DF)
  DT[,.N,by=names(DT)]
}

################
### 1. Remove Duplicates by Species
################

# read in occurrence point file
occur_all <- read.csv(file=paste0(one_up, '/in-use_occurrence_compiled/occurrence_compiled_dec2.csv'), as.is=T)

# remove spatial duplicates based on species name and lat/long rounded to 2 digits after the decimal
occur_dec2_unq <- count.dups(occur_all)%>%distinct(species,decimalLatitude,decimalLongitude,.keep_all=TRUE)
  nrow(occur_dec2_unq) #11189
  str(occur_dec2_unq)
write.csv(occur_dec2_unq, file=paste0(one_up, "/in-use_occurrence_compiled/occurrence_compiled_dec2_unique.csv"))

################
### 2. Remove Duplicates by County
################

# remove unnecessary county centroids (i.e. when higher quality occurrence point is already located within that county)
  # turn occurrence point data into a SpatialPointsDataFrame
coordinates(occur_dec2_unq) <- c("decimalLongitude", "decimalLatitude")
wgs84 <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
proj4string(occur_dec2_unq) <- wgs84
  # load shapefile of US county boundaries
counties_map <- readOGR(paste0(one_up, '/cb_2016_us_county_5m/cb_2016_us_county_5m.shp'))
  # project to WGS84
counties_wgs <- spTransform(counties_map, wgs84)
#centroids <- getSpPPolygonsLabptSlots(counties_wgs)
centroids <- as.data.frame(centroid(counties_wgs))
colnames(centroids) <- c("centroid_lon", "centroid_lat")
centroids <- data.frame("ID" = 1:nrow(centroids), centroids)
coordinates(centroids) <- c("centroid_lon", "centroid_lat")
proj4string(centroids) <- proj4string(counties_wgs) # assign projection
  # spatial join of occurrence points to counties shapefile
pts.poly <- point.in.poly(occur_dec2_unq, counties_wgs)
write.csv(pts.poly,file=paste0(one_up, "/in-use_occurrence_compiled/occurrence_compiled_dec2_unique_counties.csv"))
  # mark occurrence points that are county centroids within counties that are already represented by geolocated points
occur_counties <- as.data.frame(pts.poly)
  nrow(occur_counties) #10297
duplicates <- occur_counties[duplicated(occur_counties[c(4,15)]),]
  nrow(duplicates) #8834
to_remove <- duplicates[duplicates$gps_determ %in% "C",]
  nrow(to_remove) #437
  to_remove$county_centroid_dup <- rep("x")
occur_dup_marked <- full_join(occur_counties, to_remove)
write.csv(occur_dup_marked, file=paste0(one_up, "/in-use_occurrence_compiled/occurrence_compiled_dec2_unique_countyDupMarked.csv"))
  # remove county centroid duplicate records
occur_clean <- anti_join(occur_counties, to_remove, by = "X")
  nrow(occur_clean) #9860
write.csv(occur_clean, file=paste0(one_up, "/in-use_occurrence_compiled/occurrence_compiled_dec2_unique_countyDupRemoved.csv"))

################
### 3. Remove Unwanted Years
################

# not using right now...
# remove points recorded before 1950 (or whatever year cutoff you want)
#occur_dec2_1950 <- subset(occur_dec2,year>=1950)
#nrow(occur_dec2_1950) #6265
#unique(occur_dec2_1950$year)

################
### 4. Mark Records Outside 'Accepted' Range
################

### TO DO: spatially test to see if points are within counties of occurrence recorded by USDA PLANTS, BONAP, and NatureServe, and
### mark points which fall outside these county-level distributions --> check these points to make sure they are not locations of
### non-natural ex situ collections (remove), then the rest of the 'outliers' can be used as points with less confidence
