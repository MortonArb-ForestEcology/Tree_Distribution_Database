############ 2.1
########### 3.22.18 Emily Beckman
############# compile all occurrence point data into one standardized dataframe 

## Be sure to run "set_workingdirectory.R" before running this script

############### INPUT: several csv files of raw oak occurrence data
#
#                     gbif_DC_post-georef_revised.csv
#                     consortium_raw.csv
#                     idigbio_raw.csv
#                     fia_tree_raw.csv
#                     fia_plot_raw.csv*
#                     fia_species_raw.csv*
#                     fia_county_raw.csv*
#
#
# * marks files from fia_translation_data_raw folder
# All other files from in-use_occurrence_raw folder
#
############### OUTPUT: occurence_raw_compiled_test.csv 
#                 (compilation of all occurrence data to be used in the model)
#
################
### LIBRARIES and FUNCTIONS
################

library(dplyr)
library(plyr)
library(rgbif)
library(ridigbio)
library(data.table)
library(tidyr)
library(stringr)

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

## Match up column headers, keeping all columns, not just matching ones [stacking] (fills added columns with NAs)
# SOURCE: https://amywhiteheadresearch.wordpress.com/2013/05/13/combining-dataframes-when-the-columns-dont-match/
rbind.all.columns <- function(x, y) {
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  x[, c(as.character(y.diff))] <- NA
  y[, c(as.character(x.diff))] <- NA
  return(rbind(x, y))
}
## Example use of rbind.all.columns function:
# Create a list of all dataframes you want to stack
# 'Reduce' iterates through list and merges with previous dataframe in the list
#all_data <- Reduce(rbind.all.columns, file_dfs_list)

################
### 1. Target Species List
################

# read in list of target species
sp_list <- read.csv(file='./Google Drive/Distributions_TreeSpecies/target_species_list.csv')

sp_list <- read.csv(file=paste0(one_up, '/target_species_list.csv'), header = T)

################
### 2. Unify Already-Standardized Datasets
################

# read in standardized occurrence point datasets (exactly the same column headers in each file)
file_list <- list.files(path = "./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/standard_col", 
                        pattern = ".csv", full.names = T)

file_list <- list.files(path = "standard_col", pattern = ".csv", full.names = T)
file_dfs <- lapply(file_list, read.csv, header = TRUE, fileEncoding="latin1", strip.white = TRUE, colClasses = "character")
length(file_dfs) #7

# stack standardized datasets to create one dataframe
df <- data.frame()
for(file in seq_along(file_dfs)){
  df <- rbind(df, file_dfs[[file]])
}
str(df); nrow(df) #93192
# rename columns to match Darwin Core Archive format
setnames(df,
         old=c("source","basis","lat", "long", "uncert_m", "state","status"),
         new=c("institutionCode","basisOfRecord","decimalLatitude", "decimalLongitude", 
               "coordinateUncertaintyInMeters", "stateProvince","occurrenceRemarks"))
# add standard species ID columns
df <- join(df, sp_list, by = "species", type="full"); str(df)
# remove rows with no species name match (i.e. keep records for target species only)
df <- df[!(is.na(df$speciesKey)),]
nrow(df) #30819

################
### 3. Standardize GBIF Data
################

# read in raw occurrence points
gbif <- read.csv(file='./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/gbif_DC_post-georef_revised.csv', as.is=T)

gbif <- read.csv(file='gbif_DC_post-georef_revised.csv', as.is=T)
nrow(gbif) #11089
# Do anything to the 

# add an observation number to easily restore this order of points after merging
gbif$obs_no <- seq(1, length(gbif$state), by = 1)
# make sure species is a factor
gbif$species <- as.factor(gbif$species)
# recognize that scientificName refers to something different in sp_list

# The rows will duplicate if their species key duplicates. ex. five lobata lines 
# in sp-list, so each lobata occurrence will spring four duplicates here, unless we add the first match argument.
# add FIA ID column
gbif <- join(gbif, sp_list, by = c("species", "speciesKey"), type = "full", match = "first") 
gbif <- gbif[order(gbif$obs_no), ]

# remove extraneous columns
gbif <- subset(gbif, select = c(order,family,genus,specificEpithet,infraspecificEpithet,scientificName,
                                institutionCode,collectionCode,datasetName,basisOfRecord,catalogNumber,
                                recordNumber,decimalLatitude,decimalLongitude,gps_determ,coordinateUncertaintyInMeters,
                                georeferenceSources,year,individualCount,countryCode,stateProvince,county,
                                municipality,locality,locationRemarks,occurrenceRemarks,habitat,fieldNotes,
                                issue,species,speciesKey,fia_codes))
# add and fill dataset name column
gbif$dataset <- "gbif"
gbif$fia_codes <- as.factor(gbif$fia_codes)
gbif$gps_determ <- as.factor(gbif$gps_determ)
str(gbif)

################
### 4. Standardize Herbaria Consortium Data (SERNEC, SEINet, etc.)
################

# read in raw occurrence points  
consortium <- read.csv(file='./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/consortium_raw.csv', as.is=T)

consortium <- read.csv(file='consortium_raw.csv', as.is=T)
nrow(consortium) #98500
# remove extraneous columns
consortium <- subset(consortium, select = c(order,family,genus,specificEpithet,infraspecificEpithet,
                                            scientificName,scientificNameAuthorship,institutionCode,collectionCode,
                                            basisOfRecord,catalogNumber,recordNumber,decimalLatitude,decimalLongitude,
                                            geodeticDatum,coordinateUncertaintyInMeters,georeferenceSources,year,
                                            individualCount,country,stateProvince,county,municipality,locality,
                                            locationRemarks,occurrenceRemarks,habitat))
# add and fill dataset name column
consortium$dataset <- "herbaria" 
consortium$scientificName <- as.factor(consortium$scientificName)
# add standard species ID columns
consortium <- join(consortium, sp_list, by = "scientificName", type="full", match = "first"); str(consortium)
# remove rows with no species name match (i.e. keep records for target species only)
consortium <- consortium[!(is.na(consortium$species)),]
nrow(consortium) #632
################
### 5. Standardize iDigBio Data
################

# read in raw occurrence points  
idigbio <- read.csv(file='./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/idigbio_raw.csv', as.is=T)

idigbio <- read.csv(file='idigbio_raw.csv', as.is=T)
nrow(idigbio) #196485
# remove duplicate column
idigbio <- subset(idigbio, select = -(dwc.eventDate))
# remove the "dwc." and .idigbio preceeding each column name
names(idigbio) = gsub(pattern = "dwc.", replacement = "", x = names(idigbio))
names(idigbio) = gsub(pattern = "idigbio.", replacement = "", x = names(idigbio))
names(idigbio)[names(idigbio) == 'isoCountryCode'] <- 'countryCode'
str(idigbio)
# separate single iDigBio lat/long column into lat and long
idigbio <- idigbio %>% separate("geoPoint", c("decimalLatitude", "decimalLongitude"), sep=",", fill="right", extra="merge")
# reassign the empty latitude values to NA to avoid confusion
idigbio$decimalLatitude[which(idigbio$decimalLatitude==unique(idigbio$decimalLatitude)[1] )] <- NA
# remove the extra symbols and change the column to a numeric variable
# when using gsub, be sure to include fixed=T to avoid confusion of symbols like "
idigbio$decimalLatitude <- as.numeric(gsub("{\"lat\": ","",idigbio$decimalLatitude, fixed = T))
# repeat for longitude ("long" column)    
# first remove the bracket at the end
idigbio$decimalLongitude <- gsub("}", "", idigbio$decimalLongitude)
# then remove the extra symbols and change to numeric
idigbio$decimalLongitude <- as.numeric(gsub(" \"lon\": ","",idigbio$decimalLongitude, fixed = T))
# standardize the eventDate column
# first we have to remove the characters that are not the year, month or day
idigbio <- idigbio %>% separate("eventDate", c("year", "delete"), sep="-", fill="right", extra="merge")
# remove unwanted "delete" column
idigbio <- subset(idigbio, select = -(delete))
# remove extraneous columns
idigbio <- subset(idigbio, select = c(order,family,genus,specificEpithet,infraspecificEpithet,
                                      scientificName,institutionCode,collectionCode,basisOfRecord,
                                      catalogNumber,recordNumber,decimalLatitude,decimalLongitude,
                                      coordinateUncertaintyInMeters,year,individualCount,countryCode,
                                      stateProvince,county,municipality,locality,dataQualityScore)) 
# add and fill dataset name column
idigbio$dataset <- "idigbio" 
# capitalize first letter of genus
idigbio$genus <- str_to_title(idigbio$genus, locale = "en")
# create species column
idigbio$species <- as.factor(paste(idigbio$genus,idigbio$specificEpithet))
# add standard species ID columns
idigbio <- join(idigbio, sp_list, by = "species", type="full"); str(idigbio)
# remove rows with no species name match (i.e. keep records for target species only)
idigbio <- idigbio[!(is.na(idigbio$speciesKey)),]
nrow(idigbio) # 29655

################
### 6. Standardize FIA Data
################

fia <- read.csv(file='./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/fia_tree_raw.csv', as.is=T)   # where species information is stored
plot <- read.csv(file='./Google Drive/Distributions_TreeSpecies/fia_translation_data_raw/fia_plot_raw.csv', as.is=T)   # where coordinates are stored

fia <- read.csv(file='fia_tree_raw.csv', as.is=T)   # where species information is stored
plot <- read.csv(file=paste0(translate_fia, '/fia_plot_raw.csv'), as.is=T)   # where coordinates are stored
# remove unnecessary columns from plot
plot2 <- plot[, c("INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT", "LAT", "LON")]
# Match the location IDs and merge the species and plot data frames 
fia_coord <- merge(fia, plot2, by.y = c("INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT"), all = F)
# Add in density here. First make a dataframe with all unique plots and number them.
u <- unique(fia_coord[,c('SPCD', 'INVYR','STATECD','UNITCD', 'COUNTYCD', 'PLOT', 'LAT', 'LON')])
ID <- seq(from = 1, to = length(u$INVYR), by = 1)
u_plot <- data.frame(u, ID)
# using ID as a label that marks unique plots, see how many individual trees of a species are found in each.
density_test <- merge(u_plot, fia_coord, by = c("SPCD", "INVYR", "UNITCD", "COUNTYCD", "PLOT", "STATECD"), all = T)
t <- as.numeric(table(density_test$ID))
# The results of the table show the number of individuals per plot
u_plot$density <- t
# manipulate u_plot further to add onto raw data block; rename as fia
fia <- u_plot
rm(plot, plot2, fia_coord, density_test, u, ID)
# Match up SPCD using 
fia_sp <- read.csv(file='./Google Drive/Distributions_TreeSpecies/fia_translation_data_raw/fia_species_raw.csv', as.is=T)    

fia_sp <- read.csv(file=paste0(translate_fia, '/fia_species_raw.csv'), as.is=T)    
fia <- merge(fia, fia_sp, by = "SPCD", all = F)
fia <- fia[, 1:16]
# combine columns into single species name
fia$scientificName <- paste(fia$GENUS, fia$SPECIES, fia$VARIETY, fia$SUBSPECIES)
fia$species <- paste(fia$GENUS, fia$SPECIES)
fia$order <- "Fagales"
fia$family <- "Fagaceae"
fia$institutionCode <- "USFS"
fia$country <- "US"
# Match up STATECD and COUNTYCD using
fia_cou <- read.csv(file='./Google Drive/Distributions_TreeSpecies/fia_translation_data_raw/fia_county_raw.csv', as.is=T)    

fia_cou <- read.csv(file=paste0(translate_fia, '/fia_county_raw.csv'), as.is=T)    
fia <- merge(fia, fia_cou, by = c("STATECD", "COUNTYCD"), all = F)
# remove unnecessary columns
fia <- subset(fia, select = c(order,family,GENUS,SPECIES,scientificName,institutionCode,
                              LAT,LON,INVYR,density,country,STATENM,COUNTYNM,species,SPCD))
# rename remaining columns to match other data sets
setnames(fia,
         old=c("LAT","LON", "INVYR", "STATENM", "COUNTYNM", "SPCD", "GENUS","SPECIES", "density", "country"),
         new=c("decimalLatitude","decimalLongitude", "year", "stateProvince", "county", "fia_codes", "genus","specificEpithet", "individualCount", "countryCode"))
fia$dataset <- "fia"
fia$basisOfRecord <- "WILD_PROVENANCE"
# add standard species ID columns
fia <- join(fia, sp_list, by = c("fia_codes", "species"), type="left", match = "first"); str(fia)
fia <- fia[, 1:18]

################
### 7. Stack All Datasets
################

# Create a list of all dataframes you want to stack                            
datasets <- list(df,gbif,consortium,idigbio,fia)
# 'Reduce' iterates through list and merges with previous dataframe in the list
all_data <- Reduce(rbind.all.columns, datasets)
# make some changes across this dataset to prevent future errors
replace <- c("UNKNOWN","\\<0\\>","N/A","NA","^$")

# remove rows with no lat and long
occur_all <- all_data[!(is.na(all_data$decimalLatitude)),]
occur_all <- all_data[!(is.na(all_data$decimalLongitude)),]
# make some changes across this dataset to prevent future errors
occur_all$year[is.na(occur_all$year)]<-"1111"
replace <- c("UNKNOWN","\\<0\\>","N/A","NA","^$",is.na(i))
for (i in replace){
  occur_all$year <- gsub(i,"1111",occur_all$year)
}
occur_all$year<-as.numeric(occur_all$year)
occur_all$decimalLatitude<-as.numeric(occur_all$decimalLatitude)
occur_all$decimalLongitude<-as.numeric(occur_all$decimalLongitude)
occur_all$locality<-gsub(",",".",occur_all$locality)

# IDK about this one...
# remove points with ---less than 2 digits after the decimal for lat and/or long---
occur_dec <- occur_all[grep("\\.[0-9][1-9]",occur_all$decimalLatitude),]
nrow(occur_dec) #75696
occur_dec2 <- occur_all[grep("\\.[0-9][1-9]",occur_dec$decimalLongitude),]
nrow(occur_dec2) #67379

# reorder dataset before subsetting in next script to place higher quality datasets and most recent records first
occur_all <- occur_all[order(factor(occur_all$dataset,levels=c("other","redlist","consortium","fia","ex_situ",
                                                               "gbif","andrew_hipp","natureserve","bonap","usda"))),]
head(occur_all)
occur_all <- occur_all[order(occur_all$year, na.last = TRUE, decreasing = T),]
unique(occur_all$year)

# not used currently
# all oaks? compile each occurrence set first better way?
## i. Subset based on target species list and write file
#occur_sp <- gen_subset(occur_all, (occur_all$species %in% sp_list),"./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/standard_col/datasets_combined/occurrence_raw_compiled.csv")
#  nrow(occur_sp) #11508

# write file
write.csv(occur_all, file="./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/occurence_raw_compiled_test.csv")

write.csv(occur_all, file="occurence_raw_compiled_test.csv")

##REMOVE/MOVE TO NEXT SCRIPT

################
### CREATE DATA SUBSETS ---- working ----
################

# read in occurrence point file -IF- you've added coordinates for species with locality info but no given lat/long
occur_all <- read.csv(file='./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/standard_col/datasets_combined/occurrence_raw_compiled_edit.csv', as.is=T)
# remove points ---recorded before 1950--- (or whatever year cutoff you want)
#occur_dec2_1950 <- subset(occur_dec2,year>=1950)
#nrow(occur_dec2_1950) #6265
#unique(occur_dec2_1950$year)

# we already did this above
# reorder dataset before duplicate deletion to place higher quality datasets and most recent records first
#occur_dec2 <- occur_dec2[order(factor(occur_dec2$DATASET,levels=c("other","redlist",
#  "consortium","fia","ex_situ","gbif","andrew_hipp","natureserve","bonap","usda"))),]
#  head(occur_dec2)
#occur_dec2 <- occur_dec2[order(-occur_dec2$year, na.last = FALSE),]
#  unique(occur_dec2$year)
#  str(occur_dec2)

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

### TO DO: spatially test to see if points are within counties of occurrence recorded by USDA PLANTS, BONAP, and NatureServe, and
### mark points which fall outside these county-level distributions --> check these points to make sure they are not locations of
### non-natural ex situ collections (remove), then the rest of the 'outliers' can be used as points with less confidence 


