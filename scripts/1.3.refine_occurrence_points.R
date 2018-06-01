############ 1.3
########### 5.30.18 Elizabeth Tokarz & Emily Beckman
############# refine locality information provided for each occurrence

## Be sure to run "set_workingdirectory.R" before running this script

############### INPUT: all_occurrence_compiled.csv
############### OUTPUT:


###################
## 1. Read in Data
###################

# read in raw data file from df
all_data <- read.csv(file="raw_occurrence_compiled.csv", as.is=T, na.strings=c("","NA")) # replace any empty or "NA" cells with NA so the rest of the script works smoothly
nrow(all_data)
table(all_data$gps_determ)
# rename and compress for ease of manipulation
df <- all_data
# select desired columns and drop the rest
df <- subset(df, select = c(order,family,genus,specificEpithet,infraspecificEpithet,scientificName,
                                institutionCode,collectionCode,datasetName,basisOfRecord,catalogNumber,
                                recordNumber,decimalLatitude,decimalLongitude,coordinateUncertaintyInMeters,
                                georeferenceSources,year,individualCount,countryCode,stateProvince,county,
                                municipality,locality,locationRemarks,verbatimLocality,occurrenceRemarks,
                                habitat,fieldNotes,issue,species,speciesKey,dataset,gps_determ,synonyms,
                                fia_codes))
# change column names from DarwinCore standard to GeoLocate column names
setnames(df,
         old=c("decimalLatitude","decimalLongitude","countryCode","stateProvince"),
         new=c("lat","long","country","state"))

###########################
## 2. Fill Locality Column
###########################

# see how many rows are missing locality data
sum(is.na(df$locality)) #25879
# concatenate other columns potentially containing locality information to a new locality column
df$locality_orig <- df$locality
df$locality[!is.na(df$verbatimLocality)] <- paste(df$locality[!is.na(df$verbatimLocality)], df$verbatimLocality[!is.na(df$verbatimLocality)], sep = "; ")
df$locality[!is.na(df$occurrenceRemarks)] <- paste(df$locality[!is.na(df$occurrenceRemarks)], df$occurrenceRemarks[!is.na(df$occurrenceRemarks)], sep = "; ")
df$locality[!is.na(df$locationRemarks)] <- paste(df$locality[!is.na(df$locationRemarks)], df$locationRemarks[!is.na(df$locationRemarks)], sep = "; ")
df$locality[!is.na(df$habitat)] <- paste(df$locality[!is.na(df$habitat)], df$habitat[!is.na(df$habitat)], sep = "; ")
df$locality[!is.na(df$municipality)] <- paste(df$locality[!is.na(df$municipality)], df$municipality[!is.na(df$municipality)], sep = "; ")

# to prevent some errors, let's write out basic abbreviations in locality
# remove commas first
df$locality <- gsub(pattern = ",", x = df$locality, replacement = " ")
#df$locality <- gsub(pattern = "\\'", x = df$locality, fixed = T, replacement = "")
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
  df$locality <- gsub(pattern = change_it[i], x = df$locality, replacement = to_this[i])
}

#######################
## 3. Fill State Column
#######################

# make a new column to preserve original state column
df$state_new <- NA

# function to write in the appropriate state for each row in a new state column, by searching within locality column
extract_state_any_case <- function(d.f, loc, repl){
  # First check the state column for a direct match to one of the lower 48 states
    rows <- grep(pattern = loc, x = d.f$state, ignore.case = T)
    # Fill in the rows with matches with the state name in a new column.
    d.f$state_new[rows] <- repl
  df_s_na <- which(is.na(d.f$state_new))
  # For remaining NAs in the new state column, check the locality of those rows for state names or abbreviations
  rows <- grep(pattern = loc, x = d.f$locality, ignore.case = T)
  overlap <- intersect(df_s_na, rows)
  d.f$state_new[overlap] <- repl
  #df_s_na <- which(is.na(d.f$state_new))
  # Finally, for any remaining NAs in the new state column, check the verbatim locality.
  #  rows <- grep(pattern = loc, x = d.f$verbatimLocality, ignore.case = T)
  #overlap <- intersect(df_s_na, rows)
  #d.f$state_new[overlap] <- repl
  return(d.f$state_new)
}
# note that case cannot be ignored for abbreviations
extract_state <- function(d.f, loc, repl){
  # First check the state column for a direct match to one of the lower 48 states
    rows <- grep(pattern = loc, x = d.f$state)
    # Fill in the rows with matches with the state name in a new column.
    d.f$state_new[rows] <- repl
  df_s_na <- which(is.na(d.f$state_new))
  # For remaining NAs in the new state column, check the locality of those rows for state names or abbreviations
  rows <- grep(pattern = loc, x = d.f$locality)
  overlap <- intersect(df_s_na, rows)
  d.f$state_new[overlap] <- repl
  #df_s_na <- which(is.na(d.f$state_new))
  # Finally, for any remaining NAs in the new state column, check the verbatim locality.
  #  rows <- grep(pattern = loc, x = d.f$verbatimLocality)
  #overlap <- intersect(df_s_na, rows)
  #d.f$state_new[overlap] <- repl
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
sum(is.na(df$state_new)) #68237
# first find and fill in the state names --> this function DOES ignore.case
for (i in 1:length(state_names)){
    df$state_new <- extract_state_any_case(df, state_names[i], state_names[i])
  }
sum(is.na(df$state_new)) #38812 rows are still blank
# then look for state abbreviations --> this function does NOT ignore.case
for (i in 1:length(state_names)){
  df$state_new <- extract_state(df, state_abb[i], state_names[i])
}
sum(is.na(df$state_new)) #34802 rows are still blank
sum(is.na(df$locality[which(is.na(df$state))])) #15971 of rows w/out state have NA locality

# take a look at the remaining rows with NAs in the new state column, if they say they are in the US
df_us <- df[which(df$country=="US" | is.na(df$country)),]
#unique(df_us$locality[!is.na(df_us$locality) & is.na(df_us$state_new)])
# try to fill in the state manually, based on locality info
# change the below vectors based on your dataset and the above output
change_it <- c("Santa Barbara", "San Francisco", "OSP", "Floradida", "Californica",
              "Calif.", "Seqouia", "Fla.", "Oglethorp", "Starkesville", "Stone Mt.",
              "Fort Lauderdale","Californie","Stone Mountain","Porter Mountain",
              "Sugarloaf Mountain","Magazine Mountain")
to_this <- c( "California", "California", "Oregon", "Florida", "California",
              "California", "California", "Florida", "Georgia", "Georgia", "Georgia",
              "Florida","California","Georgia","Arkansas","Arkansas","Arkansas")
# make a loop using the same search and replace "extract _state_new" function
for (i in 1:length(to_this)){
  df$state_new <- extract_state(df, change_it[i], to_this[i])
}
sum(is.na(df$state_new)) #34245 rows are still blank
# check for typos/errors in non-NA original state entries
unique(df[is.na(df$state_new), "state"])
# these states mostly represent non-US locations
# we can now move on to filling in the county columns

########################
## 4. Fill County Column
########################

# make a new column to preserve original county column
df$county_new <- NA
# read in county and state vectors from the FIA county reference file from the FIA datamart
fia_cou <- read.csv(file=paste0(translate_fia, '/fia_county_raw.csv'), as.is=T)
distinct_cou <- read.csv(file=paste0(translate_fia, '/fia_distinct_counties.csv'), as.is=T)
# these two vectors contain all the counties in the US
cou_state_names <- fia_cou$STATENM
cou_county_names <- fia_cou$COUNTYNM
# these two have all the distinct counties
cou_state_distinct <- distinct_cou$state
cou_county_distinct <- distinct_cou$county

# make a function to search through the state, county and locality data to look for matches
# in this function, we consider the state and county as a pair
# the county and correlating state will always be entered as two of the arguments
extract_county <- function(d.f, state_loc, loc){
  # first to make sure that the county matches the state, we will only consider
  # occurrences in the state half of the pair
  df_c_look <- which(d.f$state_new == state_loc)
  # often the county was already mentioned in the county column, so first we
  # check the county column for the county name in the current pair
  rows <- grep(pattern = loc, x = d.f$county, ignore.case = T)
  # next we see which row numbers are also in the state listed in the argument
  overlap <- intersect(df_c_look, rows)
  # next the county name is written into the new county column for all occurrences with
  # the state in the pair, and the county name in the original county column
  d.f$county_new[overlap]  <- loc
  # we will also look at all localities containing the name of the county
  # note that this might misrepresent an occurrence if a street or town is
  # mentioned in the locality and matches a county name elsewhere in the state
  # make sure we do not overwrite the new county entries written above
  df_c_look <- which(df$state_new[is.na(df$county_new)] == state_loc)
  rows <- grep(pattern = loc, x = d.f$locality, ignore.case = T)
  # check to make sure the row has both the right state and the right county
  # we look at the intersection of the row numbers
  overlap <- intersect(df_c_look, rows)
  # and then only for the overlapping rows do we write the county name into the new county column
  d.f$county_new[overlap]  <- loc
  return(d.f$county_new)
}

# fill in counties with this loop
# it might take 5-10 minutes to run
for (i in 1:length(cou_state_names)){
df$county_new <- extract_county(df, cou_state_names[i], cou_county_names[i])
}
# compare how many counties were NA within the original column and the new column
sum(is.na(df$county)) #39466
sum(is.na(df$county_new)) #40268

# make a function for distinct counties (only in one state)
extract_county_distinct <- function(d.f, state_loc, loc, retur){
  # check the county column for the county name
  rows <- grep(pattern = loc, x = d.f$county, ignore.case = T)
  # make sure we do not overwrite the new county entries written above
  df_c_look <- which(is.na(df$county_new))
  rows <- grep(pattern = loc, x = d.f$locality, ignore.case = T)
  # check to make sure the row has both the right state and the right county
  # we look at the intersection of the row numbers
  overlap <- intersect(df_c_look, rows)
  # and then only for the overlapping rows do we write the county name into the new county column
  d.f$county_new[overlap]  <- loc
  d.f$state_new[overlap] <- state_loc
  if(retur == 1){
    return(d.f$state_new)
  }
  else {
    return(d.f$county_new)
  }
}

for (i in 1:length(cou_state_distinct)){
  df$county_new <- extract_county_distinct(df, cou_state_distinct[i], cou_county_distinct[i], 0)
}
write.csv(df, file='df_DC_working.csv', row.names = F)
for (i in 1:length(cou_state_distinct)){
  df$state_new <- extract_county_distinct(df, cou_state_distinct[i], cou_county_distinct[i], 1)
}
write.csv(df, file='df_DC_working.csv', row.names = F)

# compare how many counties were NA within the original column and the new column
sum(is.na(df$county)) #39466
sum(is.na(df$county_new)) #24960

# take a look at the remaining rows with NAs in the new county column
unique(df[which(is.na(df$county_new) & !is.na(df$county)), c("state", "county")])
# because of typos in the FIA document or the df entries, we can fill in some blanks:
# the below vectors can be adjusted based on the above results
county_counties <- c("DeSoto", "De Kalb", "Saint Clair", "DE BACA", "De Baca", "De Soto", "Cockran",
                      "Oglethorp Co.", "Saint Johns", "Saint Lucie", "Saint Louis", "San Bernadino","Oglethorp County","Santa Catarina","Santa Rosa")
county_states <- c("Louisiana", "Georgia", "Alabama", "New Mexico", "New Mexico", "Florida", "Texas",
                    "Georgia", "Florida", "Florida", "Missouri", "California","Georgia","California","California")
county_replacements <- c("DeSoto", "DeKalb", "St. Clair", "De Baca", "De Baca", "DeSoto",
                          "Cochran", "Oglethorpe", "St. Johns", "St. Lucie", "St. Louis", "San Bernardino","Oglethorpe","Los Angeles","Santa Barbara")
# this loop simply writes the county_replacement data to the new county column
# for rows matching both the county_counties and county_states data in the original state and county columns
for (i in 1:length(county_counties)){
df$county_new[which(df$county==county_counties[i] & df$state==county_states[i])] <- county_replacements[i]
}
sum(is.na(df$county_new)) #24925

# add observation number so we can match GeoLocate file to original dataset
df$obs_no <- seq(1, length(df$basis), 1)
# rename to Darwin Core
setnames(df,
         old=c("lat","long","country","state"),
         new=c("decimalLatitude","decimalLongitude","countryCode","stateProvince"))
# save this file before we rearrange it for GeoLocate
write.csv(df, file='df_DC_cleaned.csv', row.names = F)
#write.csv(df, file='all_occ_counties_filled.csv', row.names = F)
table(df$gps_determ)

#####################
## 5. Finishing Touches
#####################

#df <- read.csv(file='df_DC_cleaned.csv', as.is=T)
occur_all <- df

# make some changes across this dataset to prevent future errors
occur_all$year[is.na(occur_all$year)]<-"1111"
replace <- c("UNKNOWN","\\<0\\>","N/A","NA","^$","is.na(i)")
for (i in replace){
  occur_all$year <- gsub(i,"1111",occur_all$year)
}
# change the class of some column values to prevent errors
occur_all$year<-as.numeric(occur_all$year)
occur_all$decimalLatitude<-as.numeric(occur_all$decimalLatitude)
occur_all$decimalLongitude<-as.numeric(occur_all$decimalLongitude)
occur_all$locality<-gsub(",",".",occur_all$locality)

# write file
write.csv(occur_all, file=paste0(compiled, "/occurrence_compiled_refined.csv"))

# create subset to use for GeoLocate
geoloc <- occur_all[which(occur_all$dataset=="gbif" | occur_all$dataset=="consortium"),]
nrow(geoloc) #17263
write.csv(geoloc, file=paste0(compiled, "/occurrence_compiled_refined_for_geolocate.csv"))
gbif_geo <- occur_all[which(occur_all$dataset=="gbif"),]
nrow(gbif_geo) #12195
#write.csv(gbif_geo, file=paste0(compiled, "/occurrence_compiled_for_geolocate.csv"))
diff <- setdiff(occur_all$obs_no,geoloc$obs_no)
the_rest <- occur_all[diff,]
nrow(the_rest) #50974
write.csv(the_rest, file=paste0(compiled, "/occurrence_compiled_refined_no_geolocate.csv"))



### THIS PART DOES NOT APPLY CURRENTLY BECAUSE ALL POINTS ARE NOT RUN THROUGH GEOLOCATE ###
# Now let's see how many of the coordinates we had before have changed.
# sum(geo_loc$latitude==post_geo$latitude) #NA  Wow--everything has changed...This
# means that some of our pre-existing coordinates that were uploaded into GeoLocate were changed.
# We may need to change some back, but we also should be aware that some of those were
# associated with certain issues that could have been coordinate-related.

# find which coordinates existed in the input document
# pre_filled <- which(!is.na(geo_loc$latitude))
# Recognize that pre_existing coordinates were probably more precise than the
# geolocated coordinates, unless an issue was designated to it. We will want to
# retain the pre-existing coordinates that had no issues
# Remove from this above vector the coordinates that had issues.
# problematic issues here would be "COORDINATE_ROUNDED" and "COORDINATE_MISMATCH")
#### for some reason there are no issues, even in the original df file imported at very beginning of script
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
# rename columns to match df
# post_geo <- setnames(post_geo, old=c("locality_string","latitude","longitude"), new=c("locality","lat","long"))
# Now using the updated revised_post_geo dataset and the observation numbers that
# correlate with the df rows, tack on the other necessary DarwinCore columns

#df <- read.csv(file='df_DC_cleaned.csv', as.is=T)

#post_geo_full <- merge(all_occ, df, by = "obs_no")
#nrow(post_geo_full) #12195
#str(post_geo_full)
#post_geo_full <- post_geo_full[,c(1:18,20,25,27:29,32:36)]
#str(post_geo_full)

#post_geo$county <- post_geo$county.remove
#post_geo$locality <- post_geo$locality_string
#post_geo$stateProvince <- post_geo$state
#post_geo$decimalLatitude <- post_geo$latitude
#post_geo$decimalLongitude <- post_geo$longitude
