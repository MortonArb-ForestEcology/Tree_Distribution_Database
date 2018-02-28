
################
### LIBRARIES and FUNCTIONS
################

library(dplyr)
library(rgbif)
library(ridigbio)
library(data.table)
library(tidyr)

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
### 1. CREATE UNIFIED DATAFRAME OF ALL OCCURRENCE RECORDS FOR TARGET SPECIES
################

## a. Read in list of target species
sp_list <- as.character(as.list(read.csv(file='./Google Drive/Distributions_TreeSpecies/target_species_list.csv'))$species)

## b. Read in standardized occurrence point datasets (exactly the same column headers in each file)
file_list <- list.files(path = "./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/standard_col", pattern = ".csv", full.names = T)
file_dfs <- lapply(file_list, read.csv, header = TRUE, fileEncoding="latin1", strip.white = TRUE, colClasses = "character")
  length(file_dfs)

## c. Stack standardized datasets to create one dataframe
df <- data.frame()
for(file in seq_along(file_dfs)){
  df <- rbind(df, file_dfs[[file]])
}
  str(df); nrow(df) #94426
df$species_no <- 0

## d. Read in raw occurrence point datasets, standardize column names, and create one dataframe
gbif <- read.csv(file='./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/gbif_raw.csv', as.is=T)
    nrow(gbif) #90192
  setnames(gbif,
    old=c("decimallatitude","decimallongitude","basisofrecord","institutioncode","coordinateuncertaintyinmeters"),
    new=c("lat","long","basis","source","uncert_m"))
    gbif$dataset <- "gbif"
    gbif$state <- NA
    gbif$county <- NA
consortium <- read.csv(file='./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/consortium_raw.csv', as.is=T)
    nrow(consortium) #98500
  setnames(consortium,
    old=c("scientificName","decimalLatitude","decimalLongitude","basisOfRecord","habitat","associatedTaxa","institutionCode","coordinateUncertaintyInMeters","stateProvince"),
    new=c("species","lat","long","basis","locality1","locality2","source","uncert_m","state")) ### various locality info needs to be concatenated
    consortium$dataset <- "consortium"
    consortium$county <- NA
idigbio <- read.csv(file='./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/idigbio_raw.csv', as.is=T)
    nrow(idigbio) #196485
  idigbio <- setnames(idigbio,
    old=c("dwc.genus","dwc.scientificName","idigbio.geoPoint","dwc.basisOfRecord","idigbio.eventDate","dwc.locality","dwc.institutionCode","dwc.coordinateUncertaintyInMeters","dwc.stateProvince","dwc.county"),
    new=c("genus","species","lat-long","basis","year","locality","source","uncert_m","state","county"))
    idigbio$dataset <- "idigbio"
    # Separate single iDigBio lat/long column into lat and long
  idigbio <- idigbio %>% separate("lat-long", c("lat", "long"), sep=",", fill="right", extra="merge")
    unique(idigbio$lat)
### help ###: Remove everything but the number in lat and long columns in iDigBio
  #idigbio1$lat <- gsub("{\\\"lat\\\":","",idigbio$lat)
### help ###: Edit eventDate column to just be 'year' ( now in form YYYY-MM-DD )
keep_col <- c("dataset","genus","species","locality","lat","long","source","year","basis","uncert_m","state","county")
datasets <- list(gbif, consortium, idigbio)
for (i in datasets) {
  i <- i %>% select(keep_col) 
  df2 <- rbind(i) 
}
  df2$species_no <- 0
  df2$gps_determ <- NA
  df2$status <- NA
  
##  e. Stack datasets to create one large file
occur_all <- rbind(df,df2)

## f. Subset based on target species list and write file
occur_sp <- gen_subset(occur_all, (occur_all$species %in% sp_list),"./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/standard_col/datasets_combined/occurrence_raw_compiled.csv")
  nrow(occur_sp) #11508

################
### 2. CREATE DATA SUBSETS ---- working ----
################

## a. Read in occurrence point file -IF- you've added coordinates for species with locality info but no given lat/long
occur_all <- read.csv(file='./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/standard_col/datasets_combined/occurrence_raw_compiled_edit.csv', as.is=T)

## e. Replace unwanted values in specific columns and set to number type where necessary
occur_all$species<-gsub("welshii","havardii",occur_all$species)
  occur_all$species<-gsub("dunnii","palmeri",occur_all$species)
occur_all$year[is.na(occur_all$year)]<-"1111"
  replace <- c("UNKNOWN","\\<0\\>","N/A","NA","^$")
  for (i in replace){
    occur_all$year <- gsub(i,"1111",occur_all$year)
  }
  unique(occur_all$year)
occur_all$year<-as.numeric(occur_all$year)
  occur_all$lat<-as.numeric(occur_all$lat)
  occur_all$long<-as.numeric(occur_all$long)
occur_all$locality<-gsub(",",".",occur_all$locality)
  str(occur_all)
  
## b. Remove points with ---less than 2 digits after the decimal for lat and/or long---
occur_dec <- occur_sp[grep("\\.[0-9][1-9]",occur_sp$lat),]
  nrow(occur_dec) #16678
occur_dec2 <- occur_sp[grep("\\.[0-9][1-9]",occur_dec$long),]
  nrow(occur_dec2) #14881

## c. Remove points ---recorded before 1950--- (or whatever year cutoff you want)
#occur_dec2_1950 <- subset(occur_dec2,year>=1950)
#  nrow(occur_dec2_1950) #6265
# unique(occur_dec2_1950$year)

## d. Reorder dataset before duplicate deletion to place higher quality datasets and most recent records first
occur_dec2 <- occur_dec2[order(factor(occur_dec2$DATASET,levels=c("other_pts","redlist",
  "consortium","fia","ex_situ","gbif","andrew_hipp","natureserve","bonap","usda"))),]
  head(occur_dec2)
occur_dec2 <- occur_dec2[order(-occur_dec2$year, na.last = FALSE),]
  unique(occur_dec2$year)
  str(occur_dec2)

## e. Remove ---spatial duplicates--- based on species name and lat/long rounded to 2 digits after the decimal
occur_dec2_unq<-count.dups(occur_dec2)%>%distinct(SPECIES,lat_round,long_round,.keep_all=TRUE)
  nrow(occur_dec2_unq) #4798
  str(occur_dec2_unq)
write.csv(occur_dec2_unq,file="./sp_occ/merged_data/occur_dec2_unq.csv")

## f. Remove ---unnecessary county centroids--- i.e. when higher quality occurrence point is already located within that county
  # Turn occurrence point data into a SpatialPointsDataFrame
coordinates(occur_dec2_unq) <- c("LONG", "LAT")
wgs84 <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
proj4string(occur_dec2_unq) <- wgs84
  # Load shapefile of US county boundaries
counties_map <- readOGR('./sp_occ/cb_2016_us_county_5m/cb_2016_us_county_5m.shp')
  # project to WGS84
counties_wgs <- spTransform(counties_map, wgs84)
  # Spatial join of occurrence points to counties shapefile
pts.poly <- point.in.poly(occur_dec2_unq, counties_wgs)
write.csv(pts.poly,file="./sp_occ/merged_data/occur_counties.csv")
  # Mark occurrence points that are county centroids within counties that are
    # already represented by geolocated points ------ this may need another look ------
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
