############ 4. extract species from raw data frame
########### 2.27.18 Elizabeth Tokarz
###########
###########   make species shapefiles for occurrence and absence data
###########
############### INPUT: raw "occur_all" data csv 
#                 occurrence_compiled_dec2_unique_countyDupRemoved.csv             
#
#     ############### OUTPUT:shapefiles for each species of interest
#                             occurrence shapefiles: "Quercus_(species epithet)" 
#                             absence shapefiles:    "(species epithet)_absence"
#   in species_shapefiles folder in in-use_occurrence_compiled folder in Google Drive
#
#     package: dplyr, raster, maptools, rgdal, sp

library(dplyr)
library(raster)
library(maptools)
library(rgdal)
library(sp)

# First read in the script without duplicates
occur_all <- read.csv(file=paste0(compiled, '/occurrence_compiled_dec2_unique_countyDupRemoved.csv'), as.is=T)

# We want to make a shapefile for each of the species occurrences, so let's make a function for that
create_species_shp <- function(no){
  # we will pull all occurrences within one species for our starting subset
sp_subset <- occur_all[occur_all$species==unique(occur_all$species)[no],]
# then we will consider the coordinates of the occurrences
coordinates(sp_subset)=~long_round+lat_round
# and add a projection to be able to make a shapefile
proj4string(sp_subset)<- CRS("+proj=longlat +datum=WGS84")
LLcoor<-spTransform(sp_subset,CRS("+proj=longlat"))
# THe shapefile will be written into a new folder with the name of the species
writeOGR(LLcoor, dsn = paste0(compiled, "/species_shapefiles/", unique(occur_all$species)[no], ".shp"), layer ='toumeyi', driver = 'ESRI Shapefile')
}

# Now we can make a loop
for (i in 1:length(unique(occur_all$species))){
  create_species_shp(i)
}

# Note that this function allows us to name the files with the species name, 
# but that canbyi and graciliformis should be combined into one shapefile
# Fix graciliformis shapefile doing the same as above, but using the speciesKey 
# instead of the name to gather the species subset
sp_subset <- occur_all[occur_all$speciesKey==2878132,]
coordinates(sp_subset)=~long_round+lat_round
proj4string(sp_subset)<- CRS("+proj=longlat +datum=WGS84")
LLcoor<-spTransform(sp_subset,CRS("+proj=longlat"))
writeOGR(LLcoor, dsn = paste0(compiled, "/species_shapefiles/Quercus graciliformis.shp"), layer ='toumeyi', driver = 'ESRI Shapefile')

#################################################################################
# We can also make shapefiles for absence data by species
# this file was created in 2.1 at the end of the fia section.
absent <- read.csv(file=paste0(compiled, '/fia_absence_compiled.csv'), as.is=T)
# make vector of the species column names in the "absent" data frame
absent_sp <- c("arkansana","austrina", "engelmannii", "georgiana", "graciliformis", 
               "havardii", "laceyi", "lobata", "oglethorpensis", "robusta", 
               "similis", "tardifolia", "toumeyi")
# these represent columns 9 to 21 in the "absence" data frame.
# In our function, we will call these columns in one by one to make absence shapefiles.
nos <- seq(9, 21, 1)

# In this function, we will first prepare the coordinates by rounding the 
#longitude and latitude, as was previously done with the occur_all data frame,
# and then make shapefiles in the same way as with the occurrence data.
create_absence_shp <- function(ab_sp, no){
  sp_subset <- absent[absent[, no] < 1,7:8]
  # remove NA coordinates from plot so can round without error
  sp_subset <- sp_subset[!is.na(sp_subset$LON),]
  sp_subset <- sp_subset[!is.na(sp_subset$LAT),]
  sp_subset$long_round <- round(sp_subset$LON, 3)
  sp_subset$lat_round <- round(sp_subset$LAT, 3)
  coordinates(sp_subset)=~long_round+lat_round
  proj4string(sp_subset)<- CRS("+proj=longlat +datum=WGS84")
  LLcoor<-spTransform(sp_subset,CRS("+proj=longlat"))
  writeOGR(LLcoor, dsn = paste0(compiled, "/species_shapefiles/", ab_sp, "_absence.shp"), layer ='toumeyi', driver = 'ESRI Shapefile')
}

# We could apply this function with a loop, but each shapefile takes 40-60 minutes to create.
#for (i in 1:length(nos)){
#  create_absence_shp(absent_sp[i], nos[i])
#}
# We can also make them one at a time.
create_absence_shp(absent_sp[3], nos[3])

