############ 2.2
########### 4.2.18 Emily Beckman
############# subset occurrence point data to remove duplicates, etc.

############### INPUT: Google Drive/Distributions_TreeSpecies/in-use_occurrence_compiled/occurence_raw_compiled.csv
############### OUTPUT:

################
### Libraries and Functions
################

#library(dplyr)
#library(plyr)
#library(rgbif)
#library(ridigbio)
#library(data.table)
#library(tidyr)
#library(stringr)

################
### 1. Remove Duplicates by Species
################

# read in occurrence point file, if needed
occur_all <- read.csv(file=paste0(one_up, '/in-use_occurrence_compiled/occurrence_compiled.csv'), as.is=T)

# remove spatial duplicates based on species name and lat/long rounded to 2 digits after the decimal
occur_dec2_unq<-count.dups(occur_dec2)%>%distinct(SPECIES,lat_round,long_round,.keep_all=TRUE)
  nrow(occur_dec2_unq) #4798
  str(occur_dec2_unq)
write.csv(occur_dec2_unq, file=paste0(one_up, "/in-use_occurrence_compiled/occurrence_compiled_dec2.csv"))

################
### 2. Remove Duplicates by County
################

# remove unnecessary county centroids (i.e. when higher quality occurrence point is already located within that county)
# turn occurrence point data into a SpatialPointsDataFrame
coordinates(occur_dec2_unq) <- c("LONG", "LAT")
wgs84 <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
proj4string(occur_dec2_unq) <- wgs84
# load shapefile of US county boundaries
counties_map <- readOGR('./sp_occ/cb_2016_us_county_5m/cb_2016_us_county_5m.shp')
# project to WGS84
counties_wgs <- spTransform(counties_map, wgs84)
# spatial join of occurrence points to counties shapefile
pts.poly <- point.in.poly(occur_dec2_unq, counties_wgs)
write.csv(pts.poly,file="./sp_occ/merged_data/occur_counties.csv")
# mark occurrence points that are county centroids within counties that are already represented by geolocated points
occur_counties <- as.data.frame(pts.poly)
  nrow(occur_counties) #4512
  occur_counties <- occur_counties[order(occur_counties$CENTROID, na.last = FALSE),]
duplicates <- occur_counties[duplicated(occur_counties[c(2,15)]),]
  nrow(duplicates) #3825
to_remove <- duplicates[duplicates$CENTROID %in% "x",]
  nrow(to_remove) #468
  to_remove$county_centroid_dup<-rep("x")
occur_remove_marked <- full_join(occur_counties, to_remove)
  str(occur_remove_marked)
  nrow(occur_remove_marked) #4512
occur_clean <- anti_join(occur_counties, to_remove, by = c("LOCALITY","SPECIES"))
  nrow(occur_clean) #4036

## g. Write files
write.csv(occur_remove_marked,file="./sp_occ/merged_data/occur_dup_counties_marked.csv")
write.csv(occur_clean,file="./sp_occ/merged_data/occur_dup_counties_removed.csv")

### TO DO: spatially test to see if points are within counties of occurrence recorded by USDA PLANTS, BONAP, and NatureServe, and
### mark points which fall outside these county-level distributions --> check these points to make sure they are not locations of
### non-natural ex situ collections (remove), then the rest of the 'outliers' can be used as points with less confidence



# remove points recorded before 1950 (or whatever year cutoff you want)
#occur_dec2_1950 <- subset(occur_dec2,year>=1950)
#nrow(occur_dec2_1950) #6265
#unique(occur_dec2_1950$year)
