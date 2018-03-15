############ 1.2
########### 3.15.18 Elizabeth Tokarz
############# prepare GBIF for loading into GeoLocate Application
###### Find app here:  http://www.museum.tulane.edu/geolocate/web/WebGeoref.aspx 
## Load data like this: http://www.museum.tulane.edu/geolocate/standalone/tutorial.html 

############### INPUT: gbif_DarwinCore_edit.csv from Google Drive
############### OUTPUT: smaller csv file (removing all dead trees and containing
############### only our rare oaks)
###### lower48_Quercus.csv 



library(dplyr)
library(rgbif)
library(tidyr)


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
#extract_state <- function(df, loc, rep){
#  gbif_s_na <- which(is.na(df$state))
#  rows <- grep(pattern = loc, x = df$locality) 
#  overlap <- intersect(gbif_s_na, rows)
#  df$state[overlap] <- rep
#}

# Until function works, we can do the alternative long way...
#extract_state(gbif2, "CA", "California")
gbif_s_na <- which(is.na(gbif$state))
rows <- grep(pattern = "CA", x = gbif$locality) 
overlap <- intersect(gbif_s_na, rows)
gbif$state[overlap] <- "California"
#extract_state(gbif2, "California", "California")
gbif_s_na <- which(is.na(gbif$state))
rows <- grep(pattern = "California", x = gbif$locality) 
overlap <- intersect(gbif_s_na, rows)
gbif$state[overlap] <- "California"
#extract_state(gbif2, "TX", "Texas")
gbif_s_na <- which(is.na(gbif$state))
rows <- grep(pattern = "TX", x = gbif$locality) 
overlap <- intersect(gbif_s_na, rows)
gbif$state[overlap] <- "Texas"
#extract_state(gbif2, "Texas", "Texas")
gbif_s_na <- which(is.na(gbif$state))
rows <- grep(pattern = "Texas", x = gbif$locality) 
overlap <- intersect(gbif_s_na, rows)
gbif$state[overlap] <- "Texas"
#extract_state(gbif2, "FL", "Florida")
gbif_s_na <- which(is.na(gbif$state))
rows <- grep(pattern = "FL", x = gbif$locality) 
overlap <- intersect(gbif_s_na, rows)
gbif$state[overlap] <- "Florida"
#extract_state(gbif2, "Florida", "Florida")
gbif_s_na <- which(is.na(gbif$state))
rows <- grep(pattern = "Florida", x = gbif$locality) 
overlap <- intersect(gbif_s_na, rows)
gbif$state[overlap] <- "Florida"
#extract_state(gbif2, "AZ", "Arizona")
gbif_s_na <- which(is.na(gbif$state))
rows <- grep(pattern = "AZ", x = gbif$locality) 
overlap <- intersect(gbif_s_na, rows)
gbif$state[overlap] <- "Arizona"
#extract_state(gbif2, "UT", "Utah")
gbif_s_na <- which(is.na(gbif$state))
rows <- grep(pattern = "UT", x = gbif$locality) 
overlap <- intersect(gbif_s_na, rows)
gbif$state[overlap] <- "Utah"
#extract_state(gbif2, "Utah", "Utah")
gbif_s_na <- which(is.na(gbif$state))
rows <- grep(pattern = "Utah", x = gbif$locality) 
overlap <- intersect(gbif_s_na, rows)
gbif$state[overlap] <- "Utah"
#extract_state(gbif2, "AL", "Alabama")
gbif_s_na <- which(is.na(gbif$state))
rows <- grep(pattern = "AL", x = gbif$locality) 
overlap <- intersect(gbif_s_na, rows)
gbif$state[overlap] <- "Alabama"
#extract_state(gbif2, "Alamaba", "Alabama")
gbif_s_na <- which(is.na(gbif$state))
rows <- grep(pattern = "Alabama", x = gbif$locality) 
overlap <- intersect(gbif_s_na, rows)
gbif$state[overlap] <- "Alabama"
#extract_state(gbif2, "Arkansas", "Arkansas")
gbif_s_na <- which(is.na(gbif$state))
rows <- grep(pattern = "Arkansas", x = gbif$locality) 
overlap <- intersect(gbif_s_na, rows)
gbif$state[overlap] <- "Arkansas"
#extract_state(gbif2, "Georgia", "Georgia")
gbif_s_na <- which(is.na(gbif$state))
rows <- grep(pattern = "Georgia", x = gbif$locality) 
overlap <- intersect(gbif_s_na, rows)
gbif$state[overlap] <- "Georgia"
#extract_state(gbif2, "LA", "Louisiana")
gbif_s_na <- which(is.na(gbif$state))
rows <- grep(pattern = "LA", x = gbif$locality) 
overlap <- intersect(gbif_s_na, rows)
gbif$state[overlap] <- "Louisiana"
sum(is.na(gbif$state)) # 1137 We filled in almost 1000 blanks
sum(is.na(gbif$locality[is.na(gbif$state)])) # 928 localities are NAs anyway, but what about the 209 others?
gbif$locality[!is.na(gbif$locality[is.na(gbif$state)])]
# The states aren't listed, but I can still tell that many of them are in CA.
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
# lastly, if the county is NA, but the municipality is not, then we can rewrite municipality into the county column
gbif_c_na <- which(is.na(gbif$county))
mun <- which(!is.na(gbif$municipality))
overlap <- intersect(gbif_c_na, mun)
gbif$county[overlap] <- gbif$municipality[overlap]
# although the county name is often accompanied by other words in the updated
# county column, the information should be sufficient to use geolocate.

# make a new data frame for use in geolocate
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


# write a csv to upload into georeference
write.csv(geo_loc, file='G:/My Drive/Distributions_TreeSpecies/in-use_occurrence_raw/gbif_DC_georef.csv')

# note how many coordinates are missing.
sum(is.na(geo_loc$latitude)) # 5168
# We will compare this to how many have been filled in after using geoLocate











