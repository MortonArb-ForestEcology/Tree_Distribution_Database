############ 1.2
########### 3.15.18 Elizabeth Tokarz
############# prepare GBIF for loading into GeoLocate Application
###### Find app here:  http://www.museum.tulane.edu/geolocate/web/WebGeoref.aspx 
## Load data like this: http://www.museum.tulane.edu/geolocate/standalone/tutorial.html 

############### INPUT: gbif_DarwinCore_edit.csv from Google Drive
############### OUTPUT: gbif_DC_georef.csv (to be uploaded into GEOLocate application)
########                (from GEOLocate application) gbif_DC_post-georef.csv

library(dplyr)
library(rgbif)
library(tidyr)
library(data.table)

gbif <- read.csv(file='./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/gbif_raw.csv', as.is=T)
# for windows? but too big to not use server # more columns than column names
# gbif <- read.csv(file='G:/My Drive/Distributions_TreeSpecies/in-use_occurrence_raw/gbif_raw_DarwinCore_edit.csv', as.is=T)
nrow(gbif) #12195
# remove extraneous columns
gbif <- subset(gbif, select = c(basisOfRecord, institutionCode, genus, 
                                scientificName, species, year, countryCode, 
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
sum(is.na(gbif$state)) # 2030
# example
#gbif2 <- gbif
# make a vector of the rows for which state is NA
#gbif_s_na <- which(is.na(gbif2$state))
# find which rows have a locality clue of CA
#rows <- grep(pattern = "CA", x = gbif2$locality) 
# find out which row numbers overlap, meaning that both the state entry is 
# NA and the locality indicates that the NA can be changed to California
#overlap <- intersect(gbif_s_na, rows)
# now change the state entries in rows "overlap" to California
#gbif2$state[overlap] <- "California"
#sum(is.na(gbif2$state)) # 1363 Hurrah ! It worked!

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
gbif$state <- extract_state(gbif, "CA", "California")
sum(is.na(gbif$state))
gbif$state <- extract_state(gbif, "California", "California")
gbif$state <- extract_state(gbif, "Texas", "Texas")
gbif$state <- extract_state(gbif, "TX", "Texas")
gbif$state <- extract_state(gbif, "FL", "Florida")
gbif$state <- extract_state(gbif, "Florida", "Florida")
gbif$state <- extract_state(gbif, "AZ", "Arizona")
gbif$state <- extract_state(gbif, "UT", "Utah")
gbif$state <- extract_state(gbif, "Utah", "Utah")
gbif$state <- extract_state(gbif, "AL", "Alabama")
gbif$state <- extract_state(gbif, "Alabama", "Alabama")
gbif$state <- extract_state(gbif, "Arkansas", "Arkansas")
gbif$state <- extract_state(gbif, "Georgia", "Georgia")
gbif$state <- extract_state(gbif, "LA", "Louisiana")
sum(is.na(gbif$state)) # 1137 We filled in almost 1000 blanks
sum(is.na(gbif$locality[is.na(gbif$state)])) # 928 localities are NAs anyway, but what about the 209 others?
gbif$locality[!is.na(gbif$locality[is.na(gbif$state)])]
# The states aren't listed, but I can still tell that many of them are in CA.##### DO SOMETHING WITH THIS?

# COUNTY
sum(is.na(gbif$county)) # 2587
# make a vector of the rows for which counties are NA
gbif_c_na <- which(is.na(gbif$county))
# find which rows have a locality clue of "County or county"
# considered also using, "Co. or co.", but it also picked up words beginning with co...
rows <- grep(pattern = "County", x = gbif$locality) 
# find out which row numbers overlap, meaning that both the county entry is 
# NA and the locality indicates a particular county
overlap <- intersect(gbif_c_na, rows)
# now to extract the county
find_cou <- separate(gbif[overlap, ], locality, into = "loca", sep = "County", remove = F)
gbif$county[overlap]  <- find_cou$loca
sum(is.na(gbif$county)) # 2256
# do the same for "county" lowercase
gbif_c_na <- which(is.na(gbif$county))
rows <- grep(pattern = "county", x = gbif$locality) 
overlap <- intersect(gbif_c_na, rows)
find_cou <- separate(gbif[overlap, ], locality, into = "loca", sep = "county", remove = F)
gbif$county[overlap]  <- find_cou$loca
sum(is.na(gbif$county)) # 2252
# and repeat for "Co." capitalized
gbif_c_na <- which(is.na(gbif$county))
rows <- grep(pattern = "Co.", x = gbif$locality) 
overlap <- intersect(gbif_c_na, rows)
find_cou <- separate(gbif[overlap, ], locality, into = "loca", sep = "Co.", remove = F)
gbif$county[overlap]  <- find_cou$loca
sum(is.na(gbif$county)) # 2135
# Next, if the county is NA, but the municipality is not, then we can rewrite municipality into the county column
gbif_c_na <- which(is.na(gbif$county))
mun <- which(!is.na(gbif$municipality))
overlap <- intersect(gbif_c_na, mun)
gbif$county[overlap] <- gbif$municipality[overlap]
# and if the locality is NA, but the county is not, then we can rewrite the county into the locality column
gbif_c_na <- which(is.na(gbif$locality))
mun <- which(!is.na(gbif$county))
overlap <- intersect(gbif_c_na, mun)
gbif$locality[overlap] <- gbif$county[overlap]
# although the county name is often accompanied by other words in the updated
# county column, the information should be sufficient to use geolocate.

# Finally, to prevent some errors, let's write out some basic abbreviations in locality
gsub(pattern = "mtn", x = gbif$locality, replacement = "mountain")
gsub(pattern = "Mtn", x = gbif$locality, replacement = "mountain")
gsub(pattern = "cyn", x = gbif$locality, replacement = "canyon")
gsub(pattern = "Cyn", x = gbif$locality, replacement = "canyon")
gsub(pattern = "jct", x = gbif$locality, replacement = "junction")
gsub(pattern = "hwy", x = gbif$locality, replacement = "highway")
gsub(pattern = "Hwy", x = gbif$locality, replacement = "highway")
gsub(pattern = "\"", x = gbif$locality, fixed = T, replacement = "") # does this work?
gsub(pattern = " N ", x = gbif$locality, replacement = "North") # does this account for the spaces?
gsub(pattern = " s ", x = gbif$locality, replacement = "South")
gsub(pattern = " E ", x = gbif$locality, replacement = "East")
gsub(pattern = " W ", x = gbif$locality, replacement = "West")
gsub(pattern = " SE ", x = gbif$locality, replacement = "southeast")
gsub(pattern = " NE ", x = gbif$locality, replacement = "northeast")
gsub(pattern = " ca. ", x = gbif$locality, replacement = "")
gsub(pattern = " Ca. ", x = gbif$locality, replacement = "")

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
# the above are the necessary columns, but we can add more to preserve our data structure
geo_loc$basis <- gbif$basis
geo_loc$source <- gbif$source
geo_loc$genus <- gbif$genus
geo_loc$synonym <- gbif$synonym
geo_loc$species <- gbif$species
geo_loc$year <- gbif$year
geo_loc$occurrenceRemarks <- gbif$occurrenceRemarks
geo_loc$associatedTaxa <- gbif$associatedTaxa
geo_loc$uncert_m <- gbif$uncert_m
geo_loc$georeferencedSources <- gbif$georeferenceSources
geo_loc$issue <- gbif$issue
geo_loc$obs_no <- seq(1, length(gbif$basis), 1)


# write a csv to upload into georeference
write.csv(geo_loc, file='G:/My Drive/Distributions_TreeSpecies/in-use_occurrence_raw/gbif_DC_georef.csv',
          row.names = F)

# note how many coordinates are missing.
sum(is.na(geo_loc$latitude)) # 5168
# We will compare this to how many have been filled in after using geoLocate.
# We don't expect it to catch everything.

# Instructions for GeoLocate
# After the page is loaded and before beginning to GeoLocate, check "options" and unmark "do error polygon".
# The application can only process 128 entries at a time, so you will have to go 
# through and change the page, select "Page Georeference" and let it run. 
# It should take about five to ten minutes per page of 128 entries.
# After, on the bottom there should be an export option. The file should be 
# exported as a csv and it can be renamed then as "gbif_DC_post-georef.csv".

# proceed here to run the file:
# http://www.museum.tulane.edu/geolocate/web/WebFileGeoref.aspx
# use the default options when loading the file.

# load this file from memory CB576794 (created 3.15.18)