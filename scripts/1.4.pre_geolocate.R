############ 1.4
########### 3.15.18 Elizabeth Tokarz & Emily Beckman
############# prepare occurrence points for loading into GeoLocate Application
###### Find app here:  http://www.museum.tulane.edu/geolocate/web/WebGeoref.aspx
## Load data like this: http://www.museum.tulane.edu/geolocate/standalone/tutorial.html

## Be sure to run "set_workingdirectory.R" before running this script

############### INPUT:
############### OUTPUT:


###################
## 1. Read in Data
###################

df <- read.csv(file=paste0(compiled, "/occurrence_compiled_refined_for_geolocate.csv"), as.is=T, na.strings=c("","NA")) # replace any empty or "NA" cells with NA so the rest of the script works smoothly
nrow(df) #17263

#####################################
## 2. Rearrange Columns for GeoLocate
#####################################

# make a new data frame for use in geolocate
# note: it is very important that the first ten columns are exactly as follows
# this format is how GeoLocate knows to read the data
geo_loc <- data.frame(locality_string = df$locality)
geo_loc$country <- df$countryCode
geo_loc$state <- df$state_new
geo_loc$county <- df$county_new
geo_loc$latitude <- df$decimalLatitude
geo_loc$longitude <- df$decimalLongitude
geo_loc$correction_status <- NA
geo_loc$precision <- NA
geo_loc$error_polygon <- NA
geo_loc$multiple_results <- NA
# we will add back in the other columns later since it may be faster to run the GeoLocate app
# without all of them, but here are a few to make sure we can match after GeoLocate
geo_loc$year <- df$year
geo_loc$speciesKey <- df$speciesKey
geo_loc$obs_no <- df$obs_no
nrow(geo_loc) #17263
str(geo_loc)

##################################################################
## 3. Subset Occurrences to Put Smallest Number Through GeoLocate
##################################################################

# remove duplicate rows so we geolocate the smallest amount of data possible
geo_loc <- distinct(geo_loc,country,state,county,locality_string,speciesKey,.keep_all=T)
nrow(geo_loc) #11454

# version 1: records WITH COUNTY
geo_loc1 <- geo_loc[which(!is.na(geo_loc$county)),] # | !is.na(geo_loc$locality)),]
nrow(geo_loc1) #10603
# version 2: records WITH LOCALITY
geo_loc2 <- geo_loc[which(!is.na(geo_loc$locality)),]
nrow(geo_loc2) #10875
# version 3: records MISSING COORDINATES
geo_loc3 <- geo_loc[which(is.na(geo_loc$latitude)),]
nrow(geo_loc3) #4539
# version 4: records WITH LOCALITY & MISSING COORDINATES
geo_loc4 <- geo_loc3[which(!is.na(geo_loc3$locality)),]
nrow(geo_loc4) #3990

# write a csv to upload into geoLocate
write.csv(geo_loc4, file='gbif_consortium_DC_georef_hasLocalANDnoCoord.csv', row.names = F)
  #write.csv(geo_loc, file='df_DC_georef.csv', row.names = F)
  #write.csv(geo_loc1, file='df_DC_georef_hasCounty.csv', row.names = F)
  #write.csv(geo_loc2, file='df_DC_georef_hasLocal.csv', row.names = F)
  #write.csv(geo_loc3, file='df_DC_georef_noCoord.csv', row.names = F)

################################
## 4. Instructions for GeoLocate
################################

# After the page is loaded and before beginning to GeoLocate, check "options" and unmark "do error polygon".
# The application can only process 128 entries at a time, so you will have to go
# through and change the page, select "Page Georeference" and let it run.
# It should take about two to fifteen minutes per page of 128 entries, depending on the computer.
# After, on the bottom there should be an export option. The file should be
# exported as a csv and it can be renamed then as "df_DC_post-georef.csv".

# proceed here to run the file:
# http://www.museum.tulane.edu/geolocate/web/WebFileGeoref.aspx
# use the default options when loading the file.
