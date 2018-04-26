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
nrow(gbif) #12195
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
# FIX LOCALITY DATA as much as possible  
sum(is.na(gbif$locality)) #3044
# when there is no locality information other than that of the verbatim locality column, copy that information to locality
gbif$locality[is.na(gbif$locality)]  <- gbif$verbatimLocality[is.na(gbif$locality)]
sum(is.na(gbif$locality)) #1835, better

# STATE
# for columns with NA in state, look in locality data for a state abbreviation or name and copy it into the empty column
# write a function to take the state from the locality
# note that characters cannot be read directly into functions, 
# so some adjustments must be made.
extract_state <- function(d.f, loc, repl){
  gbif_s_na <- which(is.na(d.f$state))
  rows <- grep(pattern = loc, x = d.f$locality) 
  overlap <- intersect(gbif_s_na, rows)
  d.f$state[overlap] <- repl
  return(d.f$state)
}

# Run the function several times
sum(is.na(gbif$state)) # 2030
gbif$state <- extract_state(gbif, "CA", "California")
sum(is.na(gbif$state))
gbif$state <- extract_state(gbif, "California", "California")
gbif$state <- extract_state(gbif, "Texas", "Texas")
gbif$state <- extract_state(gbif, "TX", "Texas")
gbif$state <- extract_state(gbif, "FL", "Florida")
gbif$state <- extract_state(gbif, "Florida", "Florida")
gbif$state <- extract_state(gbif, "AZ", "Arizona")
gbif$state <- extract_state(gbif, "Arizona", "Arizona")
gbif$state <- extract_state(gbif, "UT", "Utah")
gbif$state <- extract_state(gbif, "Utah", "Utah")
gbif$state <- extract_state(gbif, "AL", "Alabama")
gbif$state <- extract_state(gbif, "Alabama", "Alabama")
gbif$state <- extract_state(gbif, "Arkansas", "Arkansas")
gbif$state <- extract_state(gbif, "Georgia", "Georgia")
gbif$state <- extract_state(gbif, "LA", "Louisiana")
gbif$state <- extract_state(gbif, "Louisiana", "Louisiana")
gbif$state <- extract_state(gbif, "NM", "New Mexico")
gbif$state <- extract_state(gbif, "New Mexico", "New Mexico")
gbif$state <- extract_state(gbif, "South Carolina", "South Carolina")
gbif$state <- extract_state(gbif, "Santa Barbara", "California")
gbif$state <- extract_state(gbif, "San Francisco", "California")
gbif$state <- extract_state(gbif, "Oklahoma", "Oklahoma")
gbif$state <- extract_state(gbif, "Oregon", "Oregon")
gbif$state <- extract_state(gbif, "OSP", "Oregon")
gbif$state <- extract_state(gbif, "North Carolina", "North Carolina")
sum(is.na(gbif$state)) # 1108 We filled in almost 1000 blanks
which(is.na(gbif$state))
sum(is.na(gbif$locality[which(is.na(gbif$state))])) # 928 localities are NAs anyway, but what about the 180 others?
gbif$locality[!is.na(gbif$locality) & is.na(gbif$state)]
# some remaining localities are foreign countries, some are typos.

# COUNTY
sum(is.na(gbif$county)) # 2587

extract_county <- function(d.f, loc){
  gbif_c_na <- which(is.na(d.f$county))
  rows <- grep(pattern = loc, x = d.f$locality) 
  overlap <- intersect(gbif_c_na, rows)
  # the difference between this function and extract_state is that the entire locality
  # string before the key word is written into the county column here.
  find_cou <- separate(d.f[overlap, ], locality, into = "loca", sep = "County", remove = F)
  d.f$county[overlap]  <- find_cou$loca
  return(d.f$county)
}

gbif$county <- extract_county(gbif, "County")
sum(is.na(gbif$county)) # 2256
gbif$county <- extract_county(gbif, "county")
sum(is.na(gbif$county)) # 2252
gbif$county <- extract_county(gbif, "Co.")
sum(is.na(gbif$county)) # 2135

# Next, if the county is NA, but the municipality is not, then we can rewrite municipality into the county column
gbif_c_na <- which(is.na(gbif$county))
mun <- which(!is.na(gbif$municipality))
overlap <- intersect(gbif_c_na, mun)
gbif$county[overlap] <- gbif$municipality[overlap]
sum(is.na(gbif$county)) # 2130

# and if the locality is NA, but the county is not, then we can rewrite the county into the locality column
sum(is.na(gbif$locality))  #1835
gbif_c_na <- which(is.na(gbif$locality))
mun <- which(!is.na(gbif$county))
overlap <- intersect(gbif_c_na, mun)
gbif$locality[overlap] <- gbif$county[overlap]
sum(is.na(gbif$locality))  #959
# although the county name is often accompanied by other words in the updated
# county column, the information should be sufficient to use geolocate.

# Finally, to prevent some errors, let's write out some basic abbreviations in locality
# remove commas!
gbif$locality <- gsub(pattern = ",", x = gbif$locality, replacement = " ")
gbif$locality <- gsub(pattern = "mtn", x = gbif$locality, replacement = "mountain")
gbif$locality <- gsub(pattern = "Mtn", x = gbif$locality, replacement = "mountain")
gbif$locality <- gsub(pattern = "Mts.", x = gbif$locality, replacement = "mountains")
gbif$locality <- gsub(pattern = "cyn", x = gbif$locality, replacement = "canyon")
gbif$locality <- gsub(pattern = "Cyn", x = gbif$locality, replacement = "canyon")
gbif$locality <- gsub(pattern = "jct", x = gbif$locality, replacement = "junction")
gbif$locality <- gsub(pattern = "junc. ", x = gbif$locality, replacement = "junction ")
gbif$locality <- gsub(pattern = "Rte ", x = gbif$locality, replacement = "route ")
gbif$locality <- gsub(pattern = "hwy", x = gbif$locality, replacement = "highway")
gbif$locality <- gsub(pattern = "Hwy", x = gbif$locality, replacement = "highway")
gbif$locality <- gsub(pattern = "\\'", x = gbif$locality, fixed = T, replacement = "")
gbif$locality <- gsub(pattern = " N ", x = gbif$locality, replacement = " north ")
gbif$locality <- gsub(pattern = " N. ", x = gbif$locality, replacement = " north ")
gbif$locality <- gsub(pattern = " S ", x = gbif$locality, replacement = " south ")
gbif$locality <- gsub(pattern = " S. ", x = gbif$locality, replacement = " south ")
gbif$locality <- gsub(pattern = " s ", x = gbif$locality, replacement = " south ")
gbif$locality <- gsub(pattern = " E ", x = gbif$locality, replacement = " east ")
gbif$locality <- gsub(pattern = " E. ", x = gbif$locality, replacement = " east ")
gbif$locality <- gsub(pattern = " W ", x = gbif$locality, replacement = " west ")
gbif$locality <- gsub(pattern = " W. ", x = gbif$locality, replacement = " west ")
gbif$locality <- gsub(pattern = " SE ", x = gbif$locality, replacement = " southeast ")
gbif$locality <- gsub(pattern = " SW ", x = gbif$locality, replacement = " southwest ")
gbif$locality <- gsub(pattern = " NE ", x = gbif$locality, replacement = " northeast ")
gbif$locality <- gsub(pattern = " NW ", x = gbif$locality, replacement = " northwest ")
gbif$locality <- gsub(pattern = " ca. ", x = gbif$locality, replacement = " ")
gbif$locality <- gsub(pattern = " ca ", x = gbif$locality, replacement = " ")
gbif$locality <- gsub(pattern = " Ca. ", x = gbif$locality, replacement = " ")
gbif$locality <- gsub(pattern = " mi. ", x = gbif$locality, replacement = " miles ")
gbif$locality <- gsub(pattern = " mi ", x = gbif$locality, replacement = " miles ")
gbif$locality <- gsub(pattern = " km ", x = gbif$locality, replacement = " kilometers ")
gbif$locality <- gsub(pattern = " Rd. ", x = gbif$locality, replacement = " road ")
gbif$locality <- gsub(pattern = " rd. ", x = gbif$locality, replacement = " road ")
gbif$locality <- gsub(pattern = " St. ", x = gbif$locality, replacement = " street ")
gbif$locality <- gsub(pattern = " Ave. ", x = gbif$locality, replacement = " avenue ")
gbif$locality <- gsub(pattern = " Fk. ", x = gbif$locality, replacement = " fork ")
gbif$locality <- gsub(pattern = " fk. ", x = gbif$locality, replacement = " fork ")
gbif$locality <- gsub(pattern = " Mt. ", x = gbif$locality, replacement = " Mount ")
gbif$locality <- gsub(pattern = " Pk. ", x = gbif$locality, replacement = " Peak ")


# make a new data frame for use in geolocate
# Note: it is very important that the first ten columns are as follows.
# This format is how GeoLocate knows to read the data.
geo_loc <- data.frame(locality_string = gbif$locality)
geo_loc$country <- gbif$country
geo_loc$state <- gbif$state
geo_loc$county <- gbif$county
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