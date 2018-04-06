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
                                recordNumber,decimalLatitude,decimalLongitude,gps_determ,coordinateUncertaintyInMeters,
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
rm(plot, fia_coord, density_test, u, ID)
# Match up SPCD using
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

################
### 7. Stack All Datasets
################

# Create a list of all dataframes you want to stack
datasets <- list(df,gbif,consortium,idigbio,fia)
# 'Reduce' iterates through list and merges with previous dataframe in the list
all_data <- Reduce(rbind.all.columns, datasets)
  nrow(all_data) #65699
# remove rows with no lat and long
occur_all <- all_data[!(is.na(all_data$decimalLatitude)),]
  nrow(occur_all) #55985
occur_all <- all_data[!(is.na(all_data$decimalLongitude)),]
  nrow(occur_all) #55985
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

# remove points with less than 2 digits after the decimal for lat and/or long
occur_dec2 <- occur_all[grep("\\.[0-9][1-9]",occur_all$decimalLatitude),]
nrow(occur_all) #48065 (ELT)
occur_dec2 <- occur_dec2[grep("\\.[0-9][1-9]",occur_dec2$decimalLongitude),]
nrow(occur_dec2) #42581

# reorder dataset before subsetting in next script to place higher quality datasets and most recent records first
occur_dec2 <- occur_dec2[order(factor(occur_dec2$dataset,levels=c("other","redlist","consortium","fia","ex_situ",
                                                               "gbif","andrew_hipp","natureserve","bonap","usda"))),]
head(occur_dec2)
occur_dec2 <- occur_dec2[order(occur_dec2$year, na.last = TRUE, decreasing = T),]
unique(occur_dec2$year)

# write file
write.csv(occur_dec2, file=paste0(one_up, "/in-use_occurrence_compiled/occurrence_compiled_dec2.csv"))

## ON TO THE NEXT SCRIPT TO REMOVE DUPLICATE POINTS
