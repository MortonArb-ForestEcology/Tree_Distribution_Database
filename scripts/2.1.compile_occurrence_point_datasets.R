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
############### OUTPUT: occurence_compiled_dec2.csv
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
library(lubridate)
library(rgdal)
library(geosphere)

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
sp_list <- read.csv(file=paste0(one_up, '/target_species_list.csv'), header = T)

################
### 2. Unify Already-Standardized Datasets
################

# read in standardized occurrence point datasets (exactly the same column headers in each file)
file_list <- list.files(path = "standard_col", pattern = ".csv", full.names = T)
file_dfs <- lapply(file_list, read.csv, header = TRUE, fileEncoding="latin1", strip.white = TRUE, colClasses = "character")
length(file_dfs) #7

# stack standardized datasets to create one dataframe
df <- data.frame()
for(file in seq_along(file_dfs)){
  df <- rbind(df, file_dfs[[file]])
}
str(df); nrow(df) #93192 (ELT), 94426
df$gps_determ <- ifelse(df$gps_determ=="", "NA", df$gps_determ)
# rename columns to match Darwin Core Archive format
setnames(df,
         old=c("source","basis","lat", "long", "uncert_m", "state","status"),
         new=c("institutionCode","basisOfRecord","decimalLatitude", "decimalLongitude",
               "coordinateUncertaintyInMeters", "stateProvince","occurrenceRemarks"))
# add standard species ID columns
df <- join(df, sp_list, by = "species", type="left"); str(df)
# remove rows with no species name match (i.e. keep records for target species only)
df <- df[!(is.na(df$speciesKey)),]
nrow(df) #37326 (ELT), 30900

write.csv(df, file=paste0(one_up, "/in-use_occurrence_compiled/standardized_col_compiled.csv"))

################
### 3. Standardize GBIF Data
################

# read in raw occurrence points
gbif <- read.csv(file='gbif_DC_post-georef_revised.csv', as.is=T)
nrow(gbif) #11089

# add an observation number to easily restore this order of points after merging
gbif$obs_no <- seq(1, length(gbif$state), by = 1)
# make sure species is a factor
gbif$species <- as.factor(gbif$species)
# recognize that scientificName refers to something different in sp_list

# The rows will duplicate if their species key duplicates. ex. five lobata lines
# in sp-list, so each lobata occurrence will spring four duplicates here, unless we add the first match argument.
# add FIA ID column
gbif <- join(gbif, sp_list, by = c("speciesKey"), type = "full", match = "first")
gbif <- gbif[order(gbif$obs_no), ]

# remove extraneous columns
gbif <- subset(gbif, select = c(order,family,genus,specificEpithet,infraspecificEpithet,scientificName,
                                institutionCode,collectionCode,datasetName,basisOfRecord,catalogNumber,
                                recordNumber,decimalLatitude,decimalLongitude,precision,gps_determ,coordinateUncertaintyInMeters,
                                georeferenceSources,year,individualCount,countryCode,stateProvince,county,
                                municipality,locality,locationRemarks,occurrenceRemarks,habitat,fieldNotes,
                                issue,species,speciesKey,fia_codes))
# add and fill dataset name column
gbif$dataset <- "gbif"
gbif$fia_codes <- as.factor(gbif$fia_codes)
gbif$gps_determ <- as.factor(gbif$gps_determ)
str(gbif)
nrow(gbif) #11089 still (ELT)

write.csv(gbif, file=paste0(one_up, "/in-use_occurrence_compiled/gbif_compiled.csv"))

################
### 4. Standardize Herbaria Consortium Data (SERNEC, SEINet, etc.)
################

# read in raw occurrence points
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
consortium$dataset <- "consortium"
consortium$synonyms <- consortium$scientificName
# add standard species ID columns
consortium <- join(consortium, sp_list, by = "synonyms", type="left", match = "first"); str(consortium)
# remove rows with no species name match (i.e. keep records for target species only)
consortium <- consortium[!(is.na(consortium$species)),]
nrow(consortium) #5068 (ELT)

write.csv(consortium, file=paste0(one_up, "/in-use_occurrence_compiled/consortium_compiled.csv"))

################
### 5. Standardize iDigBio Data
################

# read in raw occurrence points
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
# create synonyms column to match with sp_list
idigbio$synonyms <- as.factor(paste(idigbio$genus,idigbio$specificEpithet))
# add standard species ID columns
idigbio <- join(idigbio, sp_list, by = "synonyms", type="left"); str(idigbio)
# remove rows with no species name match (i.e. keep records for target species only)
idigbio <- idigbio[!(is.na(idigbio$speciesKey)),]
nrow(idigbio) # 11733 (ELT)

write.csv(idigbio, file=paste0(one_up, "/in-use_occurrence_compiled/idigbio_compiled.csv"))

################
### 6. Standardize FIA Data
################

# read in FIA files
fia <- read.csv(file='fia_tree_raw.csv', as.is=T)   # where species information is stored
plot <- read.csv(file=paste0(translate_fia, '/fia_plot_raw.csv'), as.is=T)   # where coordinates are stored
# remove unnecessary columns from plot
plot <- plot[, c("INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT", "LAT", "LON")]
# Match the location IDs and merge the species and plot data frames
fia_coord <- merge(fia, plot, by.y = c("INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT"), all = F)
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
rm(density_test, u, ID)
# Match up SPCD using
fia_sp <- read.csv(file=paste0(translate_fia, '/fia_species_raw.csv'), as.is=T)
fia <- merge(fia, fia_sp, by = "SPCD", all = F)
fia <- fia[, 1:16]
# count individuals per species
#unique(fia$SPECIES) # see order of species for below counts
#sum(fia[fia$SPECIES==unique(fia$SPECIES)[1], "density"]) # count number of individual trees reported per species
#table(fia$SPECIES) # count how many plots with unique coordinates contain the above individual trees
# combine columns into single species name
fia$scientificName <- paste(fia$GENUS, fia$SPECIES, fia$VARIETY, fia$SUBSPECIES)
fia$species <- paste(fia$GENUS, fia$SPECIES)
fia$order <- "Fagales"
fia$family <- "Fagaceae"
fia$institutionCode <- "USFS"
fia$country <- "US"
# Match up STATECD and COUNTYCD using
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

write.csv(fia, file=paste0(one_up, "/in-use_occurrence_compiled/fia_compiled.csv"))

### Make FIA absence occurence file ###################################################
# Note that this absence data will only be for the 13 species that FIA included in its search
rare_oak <- c(6768, 8429, 811, 6782, 851, 6785, 8514, 821, 844, 8492, 836, 8455, 8457)

# make a new function to minimize adding too many rows to this data frame
# function will compare presence coordinates to the coordinates of each plot 
# and determine which rows can be called absences for each species
# Then concatenate 4 letter code onto species column
fia_absence_joint <- plot

# rare_sp will be the fia SPCD
# be sure plot and fia_sp are loaded before running this.
find_absence <- function(rare_sp){
  presence <- fia[fia$fia_codes==rare_sp, c("decimalLatitude", "decimalLongitude")]
  presence <- paste(presence$decimalLatitude, presence$decimalLongitude, sep = "_")
# combine the coordinates of plot into single values for easy comparison
  all <- paste(plot$LAT, plot$LON, sep = "_")
# find which plot coordinates did not have the species present
  absence <- setdiff(all, presence)
  abs_list <- strsplit(absence, "_")
  #Transform the list into a data frame and set appropriate column names:
  absence <- ldply(abs_list)
  colnames(absence) <- c("LAT", "LON")
  # label the absent rows now
  absence$absent <- 1
  absence2 <- join(plot, absence, by = c("LAT", "LON"), type  ="full")
  # Even though this resulting data frame is slightly longer, its only the last row
  # that differs from plot, so we can use it to find out which row numbers represent absences
abs_count <- which(absence2$absent == 1)
  print(fia_sp[fia_sp$SPCD==rare_sp, "SPECIES"])
  print(abs_count)
}

test <- find_absence(6768)


###sapply(find_absence(rare_oak)) how does this work?






# we will make a new dataset for absence occurrences
 fia_absence <- data.frame()

 # be sure that fia_cou, fia_sp and plot have already been loaded before running loop

 # run the loop
 for (i in 1:length(rare_oak)){
   # find the unique coordinates where each species is present
   presence <- fia[fia$fia_codes==rare_oak[i], c("decimalLatitude", "decimalLongitude")]
   presence <- paste(presence$decimalLatitude, presence$decimalLongitude, sep = "_")
   # combine the coordinates of plot into single values for easy comparison
   all <- paste(plot$LAT, plot$LON, sep = "_")
   # find which plot coordinates did not have the species present
   absence <- setdiff(all, presence)
   # split the absence coordinates and make a list
   abs_list <- strsplit(absence, "_")
   #Transform the list into a data frame and set appropriate column names:
   absence <- ldply(abs_list)
   colnames(absence) <- c("LAT", "LON")
   # Now merge the above coordinates with the rest of the plot information
   absence <- join(absence, plot, by = c("LAT", "LON"), type="left", match = "first")
   # Now merge the data frame with state and county data
   absence <- merge(absence, fia_cou, by = c("STATECD", "COUNTYCD", "UNITCD"), all = F)
   # add the fia_code column
   absence$SPCD <- rare_oak[i]
   # add the rest of the species information
   absence <- merge(absence, fia_sp, by = "SPCD", all = F)
   absence <- absence[, 1:23]
   absence$scientificName <- paste(absence$GENUS, absence$SPECIES, absence$VARIETY, absence$SUBSPECIES)
   absence$species <- paste(absence$GENUS, absence$SPECIES)
   absence$order <- "Fagales"
   absence$family <- "Fagaceae"
   absence$institutionCode <- "USFS"
   absence$country <- "US"
   # remove unnecessary columns
   absence <- subset(absence, select = c(order,family,GENUS,SPECIES,scientificName,institutionCode,
                                         LAT,LON,CREATED_DATE.x,country,STATENM,COUNTYNM,species,SPCD))
   # rename remaining columns to match other data sets
   setnames(absence,
            old=c("LAT","LON", "CREATED_DATE.x", "STATENM", "COUNTYNM", "SPCD", "GENUS","SPECIES", "country"),
            new=c("decimalLatitude","decimalLongitude", "year", "stateProvince", "county", "fia_codes", "genus","specificEpithet", "countryCode"))
   # fix year column
   # first we have to remove the characters that are not the year, month or day
   absence$year <- mdy(absence$year)
   absence <- absence %>% separate("year", c("year", "delete"), sep="-", fill="right", extra="merge")
   # remove unwanted "delete" column
   absence <- subset(absence, select = -(delete))
   # I'm not sure how helpful this year column is.
   absence$dataset <- "fia"
   absence$basisOfRecord <- "WILD_PROVENANCE"
   # add standard species ID columns
   absence <- join(absence, sp_list, by = c("fia_codes", "species"), type="left", match = "first"); str(fia)
   fia_absence <- rbind(fia_absence, absence)
 }
 # takes about 90 minutes to run--my computer can only fit absences for the first 12
 # So I uploaded the compiled list of the first 12 species and absences for the 13th were uploaded separately.
 # result includes all absences besides for spcd == 8457
 write.csv(fia_absence, file=paste0(one_up, "/in-use_occurrence_compiled/fia_absence_compiled.csv"))
################
### 7. Stack All Datasets
################

# Create a list of all dataframes you want to stack
datasets <- list(df,gbif,consortium,idigbio,fia)
# 'Reduce' iterates through list and merges with previous dataframe in the list
all_data <- Reduce(rbind.all.columns, datasets)
  nrow(all_data) #65609

# Some occurrences do not have coordinate data, but they do have state and county information
# Write in county centroid coordinates and label them with a C
  # FIRST make dataframe of county centroids
  # load shapefile of US county boundaries
  counties_map <- readOGR(paste0(one_up, '/cb_2016_us_county_5m/cb_2016_us_county_5m.shp'))
  # project to WGS84
  wgs84 <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  counties_wgs <- spTransform(counties_map, wgs84)
  # make dataframe of county centroids (lat and long)
  centroids <- as.data.frame(centroid(counties_wgs))
  colnames(centroids) <- c("centroid_long", "centroid_lat")
  # round centroid lat and long to 3 digits after decimal
  centroids$long_round <- round(centroids$centroid_long, 3)
  centroids$lat_round <- round(centroids$centroid_lat, 3)
  # add county names to data frame
    # remove the 0s from the county FP codes by making the factors characters, then numeric, then factors again.
    centroids$STATECD <- as.factor(as.numeric(as.character(counties_wgs$STATEFP)))
    centroids$COUNTYCD <- as.factor(as.numeric(as.character(counties_wgs$COUNTYFP)))
  fia_cou$STATECD <- as.factor(fia_cou$STATECD)
  fia_cou$COUNTYCD <- as.factor(fia_cou$COUNTYCD)
  # Luckily these state and county codes align with the fia_county file from above
  # So let's tack on the names to these coordinates with fia_cou
  centroids <- join(centroids, fia_cou, by = c("STATECD", "COUNTYCD"), type = "left")
  # It looks like some numbers are still not quite aligning, there may be errors in states with a lot of counties--VA and AK and FL (Dade)
  
  # Second we can make a subset of the occurrences that lack coordinates, but have state and county.
  fill_in_county_coord <- which(is.na(all_data$decimalLatitude)) # this is the rows to subset of all_data
  match_these_counties <-  all_data[(is.na(all_data$decimalLatitude)),c("stateProvince", "county")] # this is a dataframe of the state and county pairs we have to match
  colnames(match_these_counties) <- c("STATENM", "COUNTYNM")
  # clean counties
  match_these_counties$COUNTYNM <- gsub(" County", "", match_these_counties$COUNTYNM, fixed = T)
  match_these_counties$COUNTYNM <- gsub(" Co.", "", match_these_counties$COUNTYNM, fixed = T)
  match_these_counties <- join(match_these_counties, centroids, by = c("STATENM", "COUNTYNM"), type ="left")
  # Note that any misspelled counties will not be found. Some rows were still unable to match data.
  
  # now fill in these coordinate gaps
  all_data[fill_in_county_coord, "decimalLatitude"] <- match_these_counties$lat_round
  all_data[fill_in_county_coord, "decimalLongitude"] <- match_these_counties$long_round
  # and label the points as gps_determ = "C"
  all_data[fill_in_county_coord, "gps_determ"] <- "C"
  
# remove rows with no lat and long still
occur_all <- all_data[!(is.na(all_data$decimalLatitude)),]
  nrow(occur_all) #59357
occur_all <- all_data[!(is.na(all_data$decimalLongitude)),]
  nrow(occur_all) #59357
# make some changes across this dataset to prevent future errors
occur_all$year[is.na(occur_all$year)]<-"1111"
replace <- c("UNKNOWN","\\<0\\>","N/A","NA","^$","is.na(i)")
for (i in replace){
  occur_all$year <- gsub(i,"1111",occur_all$year)
}
occur_all$year<-as.numeric(occur_all$year)
occur_all$decimalLatitude<-as.numeric(occur_all$decimalLatitude)
occur_all$decimalLongitude<-as.numeric(occur_all$decimalLongitude)
occur_all$locality<-gsub(",",".",occur_all$locality)

# remove points with fewer than 2 digits after the decimal for lat and/or long
occur_dec2 <- occur_all[grep("\\.[0-9][1-9]",occur_all$decimalLatitude),]
nrow(occur_dec2) #51215 (ELT)
occur_dec2 <- occur_dec2[grep("\\.[0-9][1-9]",occur_dec2$decimalLongitude),]
nrow(occur_dec2) #45390

# reorder dataset before subsetting in next script to place higher quality datasets and most recent records first
occur_dec2 <- occur_dec2[order(factor(occur_dec2$dataset,levels=c("other","redlist","consortium","fia","ex_situ",
                                                               "gbif","andrew_hipp","natureserve","bonap","usda"))),]
head(occur_dec2)
occur_dec2 <- occur_dec2[order(occur_dec2$year, na.last = TRUE, decreasing = T),]
unique(occur_dec2$year)

# write file
write.csv(occur_dec2, file=paste0(compiled, "/occurrence_compiled_dec2.csv"))

## ON TO THE NEXT SCRIPT TO REMOVE DUPLICATE POINTS
