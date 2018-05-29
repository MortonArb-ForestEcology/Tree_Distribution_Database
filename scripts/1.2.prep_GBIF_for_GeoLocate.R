############ 1.2
########### 3.15.18 Elizabeth Tokarz & Emily Beckman
############# prepare GBIF for loading into GeoLocate Application
###### Find app here:  http://www.museum.tulane.edu/geolocate/web/WebGeoref.aspx
## Load data like this: http://www.museum.tulane.edu/geolocate/standalone/tutorial.html

## Be sure to run "set_workingdirectory.R" before running this script

# Round 1 Pre-GeoLocate
############### INPUT: gbif_DarwinCore_edit.csv from Google Drive
############### OUTPUT: gbif_DC_georef.csv (to be uploaded into GEOLocate application)
# Round 2 Post-GeoLocate
############### INPUT: gbif_DC_post-georef.csv from Google Drive
############### OUTPUT: gbif_DC_post-georef_revised.csv (to be loaded into script
###############         2.1 compile_occurrence_point_datasets.R)

library(dplyr)
library(plyr)
library(rgbif)
library(tidyr)
library(data.table)
library(rgeos)

######################################################################################
################# ROUND ONE [pre GeoLocate] ##########################################################
######################################################################################

# read in raw data file from GBIF
gbif_full <- read.csv(file='gbif_raw_DarwinCore_edit.csv', as.is=T, na.strings=c("","NA")) # replace any empty or "NA" cells with NA so the rest of the script works smoothly
nrow(gbif_full) #12195
# rename and compress for ease of manipulation
gbif <- gbif_full
# select desired columns and drop the rest
gbif <- subset(gbif, select = c(order,family,genus,specificEpithet,infraspecificEpithet,scientificName,
                                institutionCode,collectionCode,datasetName,basisOfRecord,catalogNumber,
                                recordNumber,decimalLatitude,decimalLongitude,coordinateUncertaintyInMeters,
                                georeferenceSources,year,individualCount,countryCode,stateProvince,county,
                                municipality,locality,locationRemarks,verbatimLocality,occurrenceRemarks,
                                habitat,fieldNotes,issue,species,speciesKey))
# change column names from DarwinCore standard to GeoLocate column names
setnames(gbif,
         old=c("decimalLatitude","decimalLongitude","countryCode","stateProvince"),
         new=c("lat","long","country","state"))

########################
## Fill Locality Column
########################

# see how many rows are missing locality data
sum(is.na(gbif$locality)) #3044
# when there is no locality information other than that of the other locality columns, copy that information to locality
gbif$locality[is.na(gbif$locality)]  <- gbif$verbatimLocality[is.na(gbif$locality)]
sum(is.na(gbif$locality)) #1835, better
gbif$locality[is.na(gbif$locality)]  <- gbif$occurrenceRemarks[is.na(gbif$locality)]
sum(is.na(gbif$locality)) #1820
gbif$locality[is.na(gbif$locality)]  <- gbif$locationRemarks[is.na(gbif$locality)]
sum(is.na(gbif$locality)) #1267
gbif$locality[is.na(gbif$locality)]  <- gbif$habitat[is.na(gbif$locality)]
sum(is.na(gbif$locality)) #1259
# Next, concatenate any municipality entries to the locality column.
gbif$locality[!is.na(gbif$municipality)] <- paste(gbif$locality[!is.na(gbif$municipality)], gbif$municipality[!is.na(gbif$municipality)], sep = "; ")
sum(is.na(gbif$locality)) #1243

# to prevent some errors, let's write out basic abbreviations in locality
# remove commas first
gbif$locality <- gsub(pattern = ",", x = gbif$locality, replacement = " ")
gbif$locality <- gsub(pattern = "\\'", x = gbif$locality, fixed = T, replacement = "")
# now write out common abbreviations
change_it <- c("mtn", "Mtn", "Mts.", "cyn", "Cyn", "jct", "junc.",
               "Rte", "hwy", "Hwy", " N ", " N. ", " S ", " S. ", " s ",
               " E ", " E. ", " W ", " W. ", " SE ", " SW ", " NE ",
               " NW ", " ca. ", " ca ", " Ca. ", " mi. ", " mi ", " km ", " Rd. ",
               " rd. ",  " Ave. ", " Fk. ", " fk. ", " Mt. ", " Pk. ")
to_this <- c("mountain", "mountain", "mountains", "canyon", "canyon", "junction", "junction",
             "route", "highway", "highway", " north ", " north ", " south ", " south ", " south ",
             " east ", " east ", " west ", " west ", " southeast ", " southwest ", " northeast ",
             " northwest ", " ", " ", " ", " miles ", " miles ", " kilometers ", " road ",
             " road ", " avenue ", " fork ", " fork ", " Mount ", " Peak ")
# this loop finds all cases of the characters in the change_it vector and replaces
# them with the characters in the to_this vector
for (i in 1: length(to_this)){
  gbif$locality <- gsub(pattern = change_it[i], x = gbif$locality, replacement = to_this[i])
}

#####################
## Fill State Column
#####################

# make a new column to preserve original state column
gbif$state_new <- NA

# function to write in the appropriate state for each row in a new state column, by searching within locality column
extract_state_any_case <- function(d.f, loc, repl){
  # First check the state column for a direct match to one of the lower 48 states
    rows <- grep(pattern = loc, x = d.f$state, ignore.case = T)
    # Fill in the rows with matches with the state name in a new column.
    d.f$state_new[rows] <- repl
  gbif_s_na <- which(is.na(d.f$state_new))
  # For remaining NAs in the new state column, check the locality of those rows for state names or abbreviations
  rows <- grep(pattern = loc, x = d.f$locality, ignore.case = T)
  overlap <- intersect(gbif_s_na, rows)
  d.f$state_new[overlap] <- repl
  gbif_s_na <- which(is.na(d.f$state_new))
  # Finally, for any remaining NAs in the new state column, check the verbatim locality.
    rows <- grep(pattern = loc, x = d.f$verbatimLocality, ignore.case = T)
  overlap <- intersect(gbif_s_na, rows)
  d.f$state_new[overlap] <- repl
  return(d.f$state_new)
}
# note that case cannot be ignored for abbreviations
extract_state <- function(d.f, loc, repl){
  # First check the state column for a direct match to one of the lower 48 states
    rows <- grep(pattern = loc, x = d.f$state)
    # Fill in the rows with matches with the state name in a new column.
    d.f$state_new[rows] <- repl
  gbif_s_na <- which(is.na(d.f$state_new))
  # For remaining NAs in the new state column, check the locality of those rows for state names or abbreviations
  rows <- grep(pattern = loc, x = d.f$locality)
  overlap <- intersect(gbif_s_na, rows)
  d.f$state_new[overlap] <- repl
  gbif_s_na <- which(is.na(d.f$state_new))
  # Finally, for any remaining NAs in the new state column, check the verbatim locality.
    rows <- grep(pattern = loc, x = d.f$verbatimLocality)
  overlap <- intersect(gbif_s_na, rows)
  d.f$state_new[overlap] <- repl
  return(d.f$state_new)
}

# create lists of all states and abbreviations
# switch Arkansas and Kansas b/c otherwise Kansas will always replace Arkansas
state_names <- c("Alabama","Alaska","Arizona","Kansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Arkansas",
                "Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico",
                "New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah",
                "Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming", "District Of Columbia")
state_abb <- c("AL","AK","AZ","KS","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","AR","KY","LA","ME","MD","MA","MI","MN","MS",
              "MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA",
              "WV","WI","WY", "DC")

# see how many rows are missing state data
sum(is.na(gbif$state_new)) # 12195
# first find and fill in the state names --> this function DOES ignore.case
for (i in 1:length(state_names)){
    gbif$state_new <- extract_state_any_case(gbif, state_names[i], state_names[i])
  }
sum(is.na(gbif$state_new)) #2083 rows are still blank
# then look for state abbreviations --> this function does NOT ignore.case
for (i in 1:length(state_names)){
  gbif$state_new <- extract_state(gbif, state_abb[i], state_names[i])
}
sum(is.na(gbif$state_new)) #1358 rows are still blank
sum(is.na(gbif$locality[which(is.na(gbif$state))])) #366 of rows w/out state have NA locality

# take a look at the remaining rows with NAs in the new state column, if they say they are in the US
gbif_us <- gbif[which(gbif$country=="US" | is.na(gbif$country)),]
unique(gbif_us$locality[!is.na(gbif_us$locality) & is.na(gbif_us$state_new)])
# try to fill in the state manually, based on locality info
# change the below vectors based on your dataset and the above output
change_it <- c("Santa Barbara", "San Francisco", "OSP", "Floradida", "Californica",
              "Calif.", "Seqouia", "Fla.", "Oglethorp", "Starkesville", "Stone Mt.",
              "Fort Lauderdale","Californie","Stone Mountain")
to_this <- c( "California", "California", "Oregon", "Florida", "California",
              "California", "California", "Florida", "Georgia", "Georgia", "Georgia",
              "Florida","California","Georgia")
# make a loop using the same search and replace "extract _state_new" function
for (i in 1:length(to_this)){
  gbif$state_new <- extract_state(gbif, change_it[i], to_this[i])
}
sum(is.na(gbif$state_new)) #1330 rows are still blank
# check for typos/errors in non-NA original state entries
unique(gbif[is.na(gbif$state_new), "state"])
# these states mostly represent non-US locations
# we can now move on to filling in the county columns

######################
## Fill County Column
######################

# make a new column to preserve original county column
gbif$county_new <- NA
# read in county and state vectors from the FIA county reference file from the FIA datamart
fia_cou <- read.csv(file=paste0(translate_fia, '/fia_county_raw.csv'), as.is=T)
# these two vectors contain all the counties in the US
cou_state_names <- fia_cou$STATENM
cou_county_names <- fia_cou$COUNTYNM

# make a function to search through the state, county and locality data to look for matches
# in this function, we consider the state and county as a pair
# the county and correlating state will always be entered as two of the arguments
extract_county <- function(d.f, state_loc, loc){
  # first to make sure that the county matches the state, we will only consider
  # occurrences in the state half of the pair
  gbif_c_look <- which(d.f$state_new == state_loc)
  # often the county was already mentioned in the county column, so first we
  # check the county column for the county name in the current pair
  rows <- grep(pattern = loc, x = d.f$county, ignore.case = T)
  # next we see which row numbers are also in the state listed in the argument
  overlap <- intersect(gbif_c_look, rows)
  # next the county name is written into the new county column for all occurrences with
  # the state in the pair, and the county name in the original county column
  d.f$county_new[overlap]  <- loc
  # we will also look at all localities containing the name of the county
  # note that this might misrepresent an occurrence if a street or town is
  # mentioned in the locality and matches a county name elsewhere in the state
  # make sure we do not overwrite the new county entries written above
  gbif_c_look <- which(gbif$state_new[is.na(gbif$county_new)] == state_loc)
  rows <- grep(pattern = loc, x = d.f$locality, ignore.case = T)
  # check to make sure the row has both the right state and the right county
  # we look at the intersection of the row numbers
  overlap <- intersect(gbif_c_look, rows)
  # and then only for the overlapping rows do we write the county name into the new county column
  d.f$county_new[overlap]  <- loc
  return(d.f$county_new)
}

# fill in counties with this loop
# it might take 5-10 minutes to run
for (i in 1:length(cou_state_names)){
gbif$county_new <- extract_county(gbif, cou_state_names[i], cou_county_names[i])
}
# compare how many counties were NA within the original column and the new column
sum(is.na(gbif$county)) #2587
sum(is.na(gbif$county_new)) #3364

# take a look at the remaining rows with NAs in the new county column
unique(gbif[which(is.na(gbif$county_new) & !is.na(gbif$county)), c("state", "county")])
# because of typos in the FIA document or the GBIF entries, we can fill in some blanks:
# the below vectors can be adjusted based on the above results
county_counties <- c("DeSoto", "De Kalb", "Saint Clair", "DE BACA", "De Baca", "De Soto", "Cockran", "Oglethorp Co.", "Saint Johns", "Saint Lucie", "Saint Louis", "San Bernadino","Oglethorp County")
county_states <- c("Louisiana", "Georgia", "Alabama", "New Mexico", "New Mexico", "Florida", "Texas", "Georgia", "Florida", "Florida", "Missouri", "California","Georgia")
county_replacements <- c("DeSoto", "DeKalb", "St. Clair", "De Baca", "De Baca", "DeSoto", "Cochran", "Oglethorpe", "St. Johns", "St. Lucie", "St. Louis", "San Bernardino","Oglethorpe")
# this loop simply writes the county_replacement data to the new county column
# for rows matching both the county_counties and county_states data in the original state and county columns
for (i in 1:length(county_counties)){
gbif$county_new[which(gbif$county==county_counties[i] & gbif$state==county_states[i])] <- county_replacements[i]
}
sum(is.na(gbif$county_new)) #3325
# for unique county names lacking states
county_counties <- c("Charlton", "Alcorn", "Orangeburg", "Castro", "Okaloosa")
state_replacements <- c("Georgia", "Mississippi", "South Carolina", "Texas", "Florida")
for(i in 1:length(county_counties)){
gbif$county_new[which(gbif$county==county_counties[i])] <- county_counties[i]
gbif$state_new[which(gbif$county==county_counties[i])] <- state_replacements[i]
}
sum(is.na(gbif$county_new)) #3322

##########################
## Rearrange for GeoLocate
##########################

# add observation number so we can match GeoLocate file to original dataset
gbif$obs_no <- seq(1, length(gbif$basis), 1)
# save this file before we rearrange it for GeoLocate
write.csv(gbif, file='gbif_DC_cleaned.csv', row.names = F)

# make a new data frame for use in geolocate
# note: it is very important that the first ten columns are exactly as follows
# this format is how GeoLocate knows to read the data
geo_loc <- data.frame(locality_string = gbif$locality)
geo_loc$country <- gbif$country
geo_loc$state <- gbif$state_new
geo_loc$county <- gbif$county_new
geo_loc$latitude <- gbif$lat
geo_loc$longitude <- gbif$long
geo_loc$correction_status <- NA
geo_loc$precision <- NA
geo_loc$error_polygon <- NA
geo_loc$multiple_results <- NA
# we will add back in the other columns later since it may be faster to run the GeoLocate app
# without all of them, but here are a few to make sure we can match after GeoLocate
geo_loc$year <- gbif$year
geo_loc$speciesKey <- gbif$speciesKey
geo_loc$obs_no <- gbif$obs_no
nrow(geo_loc) #12195
str(geo_loc)

# version 1: records WITH COUNTY
geo_loc1 <- geo_loc[which(!is.na(geo_loc$county)),] # | !is.na(geo_loc$locality)),]
nrow(geo_loc1) #8873
# version 2: records WITH LOCALITY
geo_loc2 <- geo_loc[which(!is.na(geo_loc$locality)),]
nrow(geo_loc2) #10952
# version 3: records MISSING COORDINATES
geo_loc3 <- geo_loc[which(is.na(geo_loc$latitude)),]
nrow(geo_loc3) #5168
# version 4: records WITH LOCALITY & MISSING COORDINATES
geo_loc4 <- geo_loc3[which(!is.na(geo_loc3$locality)),]
nrow(geo_loc4) #4041

# write a csv to upload into geoLocate
write.csv(geo_loc4, file='gbif_DC_georef_hasLocalANDnoCoord.csv', row.names = F)
  #write.csv(geo_loc, file='gbif_DC_georef.csv', row.names = F)
  #write.csv(geo_loc1, file='gbif_DC_georef_hasCounty.csv', row.names = F)
  #write.csv(geo_loc2, file='gbif_DC_georef_hasLocal.csv', row.names = F)
  #write.csv(geo_loc3, file='gbif_DC_georef_noCoord.csv', row.names = F)

#############################
## Instructions for GeoLocate
#############################

# After the page is loaded and before beginning to GeoLocate, check "options" and unmark "do error polygon".
# The application can only process 128 entries at a time, so you will have to go
# through and change the page, select "Page Georeference" and let it run.
# It should take about two to fifteen minutes per page of 128 entries, depending on the computer.
# After, on the bottom there should be an export option. The file should be
# exported as a csv and it can be renamed then as "gbif_DC_post-georef.csv".

# proceed here to run the file:
# http://www.museum.tulane.edu/geolocate/web/WebFileGeoref.aspx
# use the default options when loading the file.


######################################################################################
################# ROUND TWO [post GeoLocate] ##########################################################
######################################################################################

# all records put through GeoLocate
post_geo <- read.csv(file='gbif_DC_post-georef4.csv', as.is=T)
nrow(post_geo) #3379
# all records from step one
pre_geo <- read.csv(file='gbif_DC_cleaned.csv', as.is=T)
pre_geo$gps_determ <- NA
nrow(pre_geo) #12195
# records which had lat/long before geolocate
pre_geo_coord <- pre_geo[which(!is.na(pre_geo$lat)),]
pre_geo_coord$gps_determ <- "G"
nrow(pre_geo_coord) #7027
# records with coords filled in by GeoLocate
post_geo_succ <- post_geo[which(!is.na(post_geo$latitude)),]
post_geo_succ$gps_determ <- "L"
nrow(post_geo_succ) #3045
# all records with coordinates now
have_coord <- join(post_geo_succ,pre_geo_coord,type='full') #,by='obs_no')
nrow(have_coord) #10072
# all records, with post-geo coords included
diff <- setdiff(pre_geo$obs_no,have_coord$obs_no)
no_coord <- pre_geo[diff,]
nrow(no_coord) #2123

### THIS PART DOES NOT APPLY CURRENTLY BECAUSE ALL POINTS ARE NOT RUN THROUGH GEOLOCATE ###
# Now let's see how many of the coordinates we had before have changed.
# sum(geo_loc$latitude==post_geo$latitude) #NA  Wow--everything has changed...This
# means that some of our pre-existing coordinates that were uploaded into GeoLocate were changed.
# We may need to change some back, but we also should be aware that some of those were
# associated with certain issues that could have been coordinate-related.

# fill in county centroid coordinates where necessary
# read in county and state vectors from the FIA county reference file from the FIA datamart.
fia_cou <- read.csv(file=paste0(translate_fia, '/fia_county_raw.csv'), as.is=T)
# read in csv with county centroid coordinates
counties <- read.csv(file='counties_wgs.csv', as.is=T)
setnames(fia_cou,
         old=c("STATECD","COUNTYCD"),
         new=c("STATEFP","COUNTYFP"))
centroids <- join(counties, fia_cou, by = c('STATEFP','COUNTYFP'), type = 'left')
state_names <- centroids$STATENM
county_names <- centroids$COUNTYNM
lat <- centroids$CENTROID_Y
long <- centroids$CENTROID_X

extract_county_centroid <- function(d.f, states, counties, lat, long){
  # First to make sure that the county matches the state, we will only consider
  # occurrences in the state half of the pair.
  gbif_s_look <- which(d.f$state == states)
  gbif_l_look <- which(is.na(d.f$lat))
  # Often the county was already mentioned in the county column, so first we
  # check the county column for the county name in the current pair.
  rows <- grep(pattern = counties, x = d.f$county, ignore.case = T)
  # Then we see which row numbers are also in the state listed in the argument.
  overlap <- intersect(gbif_s_look, rows)
  overlap2 <- intersect(gbif_l_look, rows)
  overlap_all <- intersect(overlap, overlap2)
  # Then the coordinate is written into the new county column for all occurrences with
  # the state in the pair, and the county name in the original county column.
  d.f$lat[overlap_all] <- lat
  d.f$long[overlap_all] <- long
  d.f$gps_determ[overlap_all] <- "C"
  return(d.f)
}
for (i in 1:nrow(no_coord)){
  no_coord <- extract_county_centroid(no_coord, state_names[i], county_names[i], lat[i], long[i])
}
filled_succ <- no_coord[which(!is.na(no_coord$lat)),]
nrow(filled_succ) #740
no_coord[which(is.na(no_coord$lat)),]$gps_determ <- "NA"

all_occ <- join(have_coord,no_coord,type='full') #,by='obs_no')
nrow(all_occ) #12195

sum(all_occ$gps_determ == "L") #3045
sum(all_occ$gps_determ == "C") #740
sum(all_occ$gps_determ == "G") #7027
sum(all_occ$gps_determ == "NA") #1383

# find which coordinates existed in the input document
# pre_filled <- which(!is.na(geo_loc$latitude))
# Recognize that pre_existing coordinates were probably more precise than the
# geolocated coordinates, unless an issue was designated to it. We will want to
# retain the pre-existing coordinates that had no issues
# Remove from this above vector the coordinates that had issues.
# problematic issues here would be "COORDINATE_ROUNDED" and "COORDINATE_MISMATCH")
#### for some reason there are no issues, even in the original gbif file imported at very beginning of script
#rounded <- grep(pattern = "COORDINATE_ROUNDED", pre_geo$issue)
#mismatch <- grep(pattern = "COORDINATE_MISMATCH", post_geo_succ$issue)
#problems <- union(rounded, mismatch)
# We can make a vector of the so-called issueless rows, in which we trust the
# pre-filled coordinates more than the GeoLocate output. From our vector of the
# numbers of the pre-filled rows, remove all row numbers that had the issues described above.
#issueless <- post_geo_succ[-problems]
#nrow(issueless) # 3660
# Now replace the "issueless rows with their pre-existing coordinates
# post_geo$latitude[issueless] <- geo_loc$latitude[issueless]
# post_geo$longitude[issueless] <- geo_loc$longitude[issueless]
# Now label these replaced values with a G for given in "gps_determ"
# post_geo$gps_determ[issueless] <- "G"

# join post geolocate and occurrences with a given lat-long
#post_geo_all <- rbind(pre_geo,post_geo_succ)
#nrow(post_geo_all) #16295

# At this point, some of the NA coordinates may correspond to occurrences with only state and county locality.
# Label these points as C for county in "gps_determ"
# check_sc <- post_geo[which(is.na(post_geo$gps_determ)), c("state", "county")]
#length(check_sc$state)
#sum(!is.na(check_sc))
# a <- which(!is.na(check_sc$state))
# b <- which(!is.na(check_sc$county))
# keep_these <- intersect(a,b)
## now rewrite check_sc to feature numbers
# check_sc <- which(is.na(post_geo$gps_determ))
# and write in the C for county
# post_geo[check_sc[keep_these], "gps_determ"] <- "C"

# now remove all occurrences that still have NA in gps_determ
# keep_these <- which(!is.na(post_geo$gps_determ))
# post_geo <- post_geo[keep_these, ]

# tack on an observation number here
#post_geo_all$obs_no <- seq(1, length(post_geo_all), 1)
# rename columns to match gbif
# post_geo <- setnames(post_geo, old=c("locality_string","latitude","longitude"), new=c("locality","lat","long"))
# Now using the updated revised_post_geo dataset and the observation numbers that
# correlate with the gbif rows, tack on the other necessary DarwinCore columns

#gbif <- read.csv(file='gbif_DC_cleaned.csv', as.is=T)

#post_geo_full <- merge(all_occ, gbif, by = "obs_no")
#nrow(post_geo_full) #12195
#str(post_geo_full)
#post_geo_full <- post_geo_full[,c(1:18,20,25,27:29,32:36)]
#str(post_geo_full)

#post_geo$county <- post_geo$county.remove
#post_geo$locality <- post_geo$locality_string
#post_geo$stateProvince <- post_geo$state
#post_geo$decimalLatitude <- post_geo$latitude
#post_geo$decimalLongitude <- post_geo$longitude

setnames(all_occ,
         old=c("lat","long","country","state"),
         new=c("decimalLatitude","decimalLongitude","countryCode","stateProvince"))

# Lastly we will reorder the occurrences here according to their year and precision, as determined by GeoLocate
# By year
all_occ <- all_occ[order(all_occ$year, decreasing = T), ]

# Then we will sort "high" precision first, and then "medium" then "low" and lastly "NA" or "(blank)"
high <- as.numeric(grep(all_occ$precision, pattern = "High"))
medium1 <- as.numeric(grep(all_occ$precision, pattern = "Medium"))
medium2 <- as.numeric(grep(all_occ$precision, pattern = "medium"))
low <- as.numeric(grep(all_occ$precision, pattern = "Low"))
blank <- as.numeric(which(is.na(all_occ$precision)))

# We can concatenate these row numbers to create the order in which we want the occurrences
precise_order <- c(high, medium1, medium2, low, blank)
all_occ <- all_occ[precise_order, ]

# and write a new dataset
write.csv(all_occ, file='gbif_DC_post-georef_revised4.csv', row.names = F)
