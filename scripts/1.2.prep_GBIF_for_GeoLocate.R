############ 1.2
########### 3.15.18 Elizabeth Tokarz
############# prepare GBIF for loading into GeoLocate Application
###### Find app here:  http://www.museum.tulane.edu/geolocate/web/WebGeoref.aspx
## Load data like this: http://www.museum.tulane.edu/geolocate/standalone/tutorial.html

## Be sure to run "set_workingdirectory.R" before running this script

# Round 1 Pre-GeoLocate
############### INPUT: gbif_DarwinCore_edit.csv from Google Drive
############### OUTPUT: gbif_DC_georef.csv (to be uploaded into GEOLocate application)
########                (from GEOLocate application) gbif_DC_post-georef.csv
# Round 2 Post-GeoLocate
############### INPUT: gbif_DC_post-georef.csv from Google Drive
############### OUTPUT: gbif_DC_post-georef_revised.csv (to be loaded into script
#                       2.compile_occurrence_point_datasets.R)

library(dplyr)
library(rgbif)
library(tidyr)
library(data.table)

######################################################################################
################# ROUND ONE ##########################################################
######################################################################################
gbif_full <- read.csv(file='gbif_raw_DarwinCore_edit.csv', as.is=T)
nrow(gbif_full) #12195
# rename and compress for ease of manipulation
gbif <- gbif_full
# remove extraneous columns
gbif <- subset(gbif, select = c(basisOfRecord, institutionCode, genus,
                                scientificName, speciesKey, species, year, countryCode,
                                stateProvince, county, municipality,
                                locality, verbatimLocality, occurrenceRemarks,
                                associatedTaxa, decimalLatitude, decimalLongitude,
                                coordinateUncertaintyInMeters,
                                georeferenceSources, issue))

setnames(gbif,
         old=c("decimalLatitude","decimalLongitude","basisOfRecord","institutionCode","coordinateUncertaintyInMeters", "countryCode", "stateProvince", "scientificName"),
         new=c("lat","long","basis","source","uncert_m", "country", "state", "synonym"))

# to be able to use GeoLocate, we will need to refine the state, county and locality columns.
# FIX LOCALITY DATA as much as possible
sum(is.na(gbif$locality)) #3044
# when there is no locality information other than that of the verbatim locality column, copy that information to locality
gbif$locality[is.na(gbif$locality)]  <- gbif$verbatimLocality[is.na(gbif$locality)]
sum(is.na(gbif$locality)) #1835, better

# STATE
# First check the state column for a direct match to one of the lower 48 states
# Fill in matches in a new column.
# For remaining NAs in the new state column, check the locality of those rows for state names or abbreviations
# Finally, for any remaining NAs in the new state column, check the verbatim locality again.

# make a new column before running this function
gbif$state_new <- NA

# Note that case cannot be ignored, otherwise, kansas will replace arkansas.
extract_state_new <- function(d.f, loc, repl){
  rows <- grep(pattern = loc, x = d.f$state) 
  d.f$state_new[rows] <- repl
  gbif_s_na <- which(is.na(d.f$state_new))
  rows <- grep(pattern = loc, x = d.f$locality) 
  overlap <- intersect(gbif_s_na, rows)
  d.f$state_new[overlap] <- repl
  gbif_s_na <- which(is.na(d.f$state_new))
  rows <- grep(pattern = loc, x = d.f$verbatimLocality) 
  overlap <- intersect(gbif_s_na, rows)
  d.f$state_new[overlap] <- repl
  return(d.f$state_new)
}

# all states and abbreviations
state_names <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas",
                "Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico",
                "New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah",
                "Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming", "District Of Columbia")
state_abb <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS",
              "MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA",
              "WV","WI","WY", "DC")

sum(is.na(gbif$state_new)) # 12195
# first for the state names
for (i in 1:length(state_names)){
    gbif$state_new <- extract_state_new(gbif, state_names[i], state_names[i])
  }
sum(is.na(gbif$state_new)) # 2250

# then for the state abbreviations
for (i in 1:length(state_names)){
  gbif$state_new <- extract_state_new(gbif, state_abb[i], state_names[i])
}
sum(is.na(gbif$state_new)) # 1527
sum(is.na(gbif$locality[which(is.na(gbif$state))])) # 928 localities are NAs

#unique(gbif$locality[!is.na(gbif$locality) & is.na(gbif$state_new)]) # a lot of these localities are outside the US
# There are a couple of funny outliers--you may change these below vectors based on your dataset and the above output
# make a loop for the rest:
change_it <- c("Santa Barbara", "San Francisco", "OSP", "Floradida", "Californica", 
              "(Calif.)", "Seqouia", "Fla.", "Oglethorp", "Starkesville", "Stone Mt.", 
              "Fort Lauderdale", "New mexico")
to_this <- c( "California", "California", "Oregon", "Florida", "California", 
              "California", "California", "Florida", "Georgia", "Georgia", "Georgia", 
              "Florida", "New Mexico")

for (i in 1:length(to_this)){
  gbif$state_new <- extract_state_new(gbif, change_it[i], to_this[i])
}
sum(is.na(gbif$state_new)) # 1494

# checking for typos/errors in non-NAs
unique(gbif[is.na(gbif$state_new), "state"] )
# these states mostly represent non-US locations
# We can now move on to filling in the county columns.

# COUNTY
gbif$county_new <- NA

# read in county and state vectors
fia_cou <- read.csv(file=paste0(translate_fia, '/fia_county_raw.csv'), as.is=T)
# These two vectors contain all the counties in the US
cou_state_names <- fia_cou$STATENM
cou_county_names <- fia_cou$COUNTYNM

# make a function to search through the state, county and locality data to look for matches
extract_county_new_v2 <- function(d.f, state_loc, loc){
  gbif_c_look <- which(d.f$state_new == state_loc)
  rows <- grep(pattern = loc, x = d.f$locality, ignore.case = T)
  overlap <- intersect(gbif_c_look, rows)
  d.f$county_new[overlap]  <- loc
  # Sometimes the county was already mentioned in the county column
  # Note that the above might misrepresent an occurrence if a street or town is mentioned in the locality that matches a county name elsewhere in the state
  rows <- grep(pattern = loc, x = d.f$county, ignore.case = T)
  overlap <- intersect(gbif_c_look, rows)
  d.f$county_new[overlap]  <- loc
  return(d.f$county_new)
}

# fill in counties with this loop. It should take 5-10 minutes to run.
for (i in 1:length(cou_state_names)){
gbif$county_new <- extract_county_new_v2(gbif, cou_state_names[i], cou_county_names[i])
}

sum(is.na(gbif$county)) # 2587
sum(is.na(gbif$county_new)) # 2966

unique(gbif[which(is.na(gbif$county_new)), c("state", "county")])
# because of typos in the FIA document or the GBIF entries, we need to fill in some blanks:
# The below vectors can be adjusted based on the above results for your dataset

# For misspelled counties that do not match FIA
county_counties <- c("DeSoto", "De Kalb", "Saint Clair", "DE BACA", "De Baca", "De Soto", "Cockran", "Oglethorp Co.", "Saint Johns")
county_states <- c("Louisiana", "Georgia", "Alabama", "New Mexico", "New Mexico", "Florida", "Texas", "Georgia", "Florida")
county_replacements <- c("DeSoto", "DeKalb", "St. Clair", "De Baca", "De Baca", "DeSoto", "Cochran", "Oglethorpe", "St. Johns")  

for (i in 1:length(county_counties)){
gbif$county_new[which(gbif$county==county_counties[i] & gbif$state==county_states[i])] <- county_replacements[i]
}
sum(is.na(gbif$county_new)) # 2929

# For unique county names lacking states
county_counties <- c("Charlton", "Alcorn", "Orangeburg")
state_replacements <- c("Georgia", "Mississippi", "South Carolina")

for(i in 1:length(county_counties)){
gbif$county_new[which(gbif$county==county_counties[i])] <- county_counties[i]
gbif$state_new[which(gbif$county==county_counties[i])] <- state_replacements[i]
}
sum(is.na(gbif$county_new)) # 2926

# check new county names now
unique(gbif$county_new)

# Next, if the county is NA, but the municipality is not, then we can rewrite municipality into the county column
gbif_c_na <- which(is.na(gbif$county_new))
mun <- which(!is.na(gbif$municipality))
overlap <- intersect(gbif_c_na, mun)
gbif$county_new[overlap] <- gbif$municipality[overlap]
sum(is.na(gbif$county_new)) # 2921

# Back to LOCALITY
# and if the locality is NA, but the county is not, then we can rewrite the county into the locality column
sum(is.na(gbif$locality))  #1835
gbif_c_na <- which(is.na(gbif$locality))
mun <- which(!is.na(gbif$county_new))
overlap <- intersect(gbif_c_na, mun)
gbif$locality[overlap] <- gbif$county_new[overlap]
sum(is.na(gbif$locality))  #995

# Finally, to prevent some errors, let's write out some basic abbreviations in locality
# remove commas first
gbif$locality <- gsub(pattern = ",", x = gbif$locality, replacement = " ")
gbif$locality <- gsub(pattern = "\\'", x = gbif$locality, fixed = T, replacement = "")
# now make a loop
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

for (i in 1: length(to_this)){
  gbif$locality <- gsub(pattern = change_it[i], x = gbif$locality, replacement = to_this[i])
}

# Save this file before we rearrange it for GeoLocate
write.csv(gbif, file='gbif_DC_cleaned.csv', row.names = F)

# make a new data frame for use in geolocate
# Note: it is very important that the first ten columns are as follows.
# This format is how GeoLocate knows to read the data.
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
# We can add other columns later and it may be faster to run the GeoLocate app
# without all of them. Just keep a few so we can identify the columns.
geo_loc$year <- gbif$year
geo_loc$speciesKey <- gbif$speciesKey
geo_loc$obs_no <- seq(1, length(gbif$basis), 1)

# write a csv to upload into georeference
write.csv(geo_loc, file='gbif_DC_georef.csv', row.names = F)

# note how many coordinates are missing.
sum(is.na(geo_loc$latitude)) # 5168
# We will compare this to how many have been filled in after using geoLocate.
# We don't expect it to catch everything.

# Instructions for GeoLocate
# After the page is loaded and before beginning to GeoLocate, check "options" and unmark "do error polygon".
# The application can only process 128 entries at a time, so you will have to go
# through and change the page, select "Page Georeference" and let it run.
# It should take about two to fifteen minutes per page of 128 entries, depending on the computer.
# After, on the bottom there should be an export option. The file should be
# exported as a csv and it can be renamed then as "gbif_DC_post-georef.csv".

# proceed here to run the file:
# http://www.museum.tulane.edu/geolocate/web/WebFileGeoref.aspx
# use the default options when loading the file.

# Note: this above script has been updated since last running the file through GeoLocate on 3/23/18
######################################################################################
################# ROUND TWO ##########################################################
######################################################################################

# load this file from memory 7DBC9921 (created 3.21.18)

#do a quick comparison with the output file
post_geo <- read.csv(file='gbif_DC_post-georef.csv', as.is=T)

sum(is.na(post_geo$latitude)) # 2052 Whoa! This is a big difference, using geolocate gave us over 3000 more occurrences

# Now let's see how many of the coordinates we had before have changed.
sum(geo_loc$latitude==post_geo$latitude) #NA  Wow--everything has changed...This
# means that some of our pre-existing coordinates that were uploaded into GeoLocate were changed.
# We may need to change some back, but we also should be aware that some of those were
# associated with certain issues that could have been coordinate-related.

# let's give all of these non-NA coordinates an L for localized in new column "gps_determ"
post_geo$gps_determ <- NA
post_geo$gps_determ[!is.na(post_geo$latitude)] <- "L"

# find which coordinates existed in the input document
pre_filled <- which(!is.na(geo_loc$latitude))
# Recognize that pre_existing coordinates were probably more precise than the
# geolocated coordinates, unless an issue was designated to it. We will want to
# retain the pre-existing coordinates that had no issues
# Remove from this above vector the coordinates that had issues.
# problematic issues here would be "COORDINATE_ROUNDED" and "COORDINATE_MISMATCH")
rounded <- grep(pattern = "COORDINATE_ROUNDED", x = gbif[pre_filled, "issue"])
mismatch <- grep(pattern = "COORDINATE_MISMATCH", x = gbif[pre_filled, "issue"])
problems <- union(rounded, mismatch)
issueless <- pre_filled[-problems]
# Now replace the "issueless rows with their pre-existing coordinates
post_geo$latitude[issueless] <- geo_loc$latitude[issueless]
post_geo$longitude[issueless] <- geo_loc$longitude[issueless]
# Now label these replaced values with a G for given in "gps_determ"
post_geo$gps_determ[issueless] <- "G"

# At this point, some of the NA coordinates may correspond to occurrences with only state and county locality.
# Label these points as C for county in "gps_determ"
check_sc <- post_geo[which(is.na(post_geo$gps_determ)), c("state", "county")]
#length(check_sc$state)
#sum(!is.na(check_sc))
a <- which(!is.na(check_sc$state))
b <- which(!is.na(check_sc$county))
keep_these <- intersect(a,b)
## now rewrite check_sc to feature numbers
check_sc <- which(is.na(post_geo$gps_determ))
# and write in the c for county
post_geo[check_sc[keep_these], "gps_determ"] <- "C"

# now remove all occurrences that still have NA in gps_determ
keep_these <- which(!is.na(post_geo$gps_determ))
post_geo <- post_geo[keep_these, ]

# tack on an observation number here
gbif_full$obs_no <- seq(1, length(gbif$basis), 1)
# Now using the updated revised_post_geo dataset and the observation numbers that
# correlate with the gbif rows, tack on the other necessary DarwinCore columns
post_geo <- merge(post_geo, gbif_full, by = "obs_no", suffixes = c(".remove", ""))
post_geo$county <- post_geo$county.remove
post_geo$locality <- post_geo$locality_string
post_geo$stateProvince <- post_geo$state
post_geo$decimalLatitude <- post_geo$latitude
post_geo$decimalLongitude <- post_geo$longitude

# Lastly we will reorder the occurrences here according to their year and precision, as determined by GeoLocate
# By year
post_geo <- post_geo[order(post_geo$year, decreasing = T), ]

# Then we will sort "high" precision first, and then "medium" then "low" and lastly "NA" or "(blank)"
high <- as.numeric(grep(post_geo$precision, pattern = "High"))
medium1 <- as.numeric(grep(post_geo$precision, pattern = "Medium"))
medium2 <- as.numeric(grep(post_geo$precision, pattern = "medium"))
low <- as.numeric(grep(post_geo$precision, pattern = "Low"))
blank <- as.numeric(which(post_geo$precision==""))
precise_order <- c(high, medium1, medium2, low, blank)
post_geo <- post_geo[precise_order, ]

# and write a new dataset
write.csv(post_geo, file='gbif_DC_post-georef_revised.csv', row.names = F)
