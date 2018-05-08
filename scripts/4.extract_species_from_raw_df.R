############ 4. extract species from raw data frame
########### 2.27.18 Elizabeth Tokarz
###########
###########
###########   make species shapefiles
###########
############### INPUT: raw "occur_all" data csv 
#                 occurrence_compiled_dec2_unique_countyDupRemoved.csv             
#
#     package: dplyr
#
#     ############### OUTPUT:shapefiles for each species of interest
#                             occurrence shapefiles: "Quercus_(species epithet)" 
#                             absence shapefiles:    "(species epithet)_absence"
#   in species_shapefiles folder in in-use_occurrence_compiled folder in Google Drive
#
library(dplyr)
library(raster)
library(maptools)
library(rgdal)
library(sp)

occur_all <- read.csv(file=paste0(compiled, '/occurrence_compiled_dec2_unique_countyDupRemoved.csv'), as.is=T)

# We want to make a shapefile for each of the species occurrences, so let's make a function for that

create_species_shp <- function(no){
# First unique(occur_all$species)[1]
sp_subset <- occur_all[occur_all$species==unique(occur_all$species)[no],]
coordinates(sp_subset)=~long_round+lat_round
proj4string(sp_subset)<- CRS("+proj=longlat +datum=WGS84")
LLcoor<-spTransform(sp_subset,CRS("+proj=longlat"))
writeOGR(LLcoor, dsn = paste0(compiled, "/species_shapefiles/", unique(occur_all$species)[no], ".shp"), layer ='toumeyi', driver = 'ESRI Shapefile')
}

# Note that this function allows us to name the files with the species name, but that canbyi and graciliformis should be combined into one shapefile

# Now we can make a loop
for (i in 1:length(unique(occur_all$species))){
  create_species_shp(i)
}

# Fix graciliformis shapefile
sp_subset <- occur_all[occur_all$speciesKey==2878132,]
coordinates(sp_subset)=~long_round+lat_round
proj4string(sp_subset)<- CRS("+proj=longlat +datum=WGS84")
LLcoor<-spTransform(sp_subset,CRS("+proj=longlat"))
writeOGR(LLcoor, dsn = paste0(compiled, "/species_shapefiles/Quercus graciliformis.shp"), layer ='toumeyi', driver = 'ESRI Shapefile')

#################################################################################
# We can also make shapefiles for absence data by species
# this file was created in 2.1 at the end of the fia section.
absent <- read.csv(file=paste0(compiled, '/fia_absence_compiled.csv'), as.is=T)
str(absent)
# make vector of species column names in absent
absent_sp <- c("arkansana","austrina", "engelmannii", "georgiana", "graciliformis", 
               "havardii", "laceyi", "lobata", "oglethorpensis", "robusta", 
               "similis", "tardifolia", "toumeyi")
# these represent columns 9 to 21
nos <- seq(9, 21, 1)

create_absence_shp <- function(ab_sp, no){
  # First unique(occur_all$species)[1]
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

# attempt to do this all at once
##sapply(absent_sp, create_absence_shp(absent_sp, nos)) #does something that takes up a lot of memory

# how about a loop instead
#
#for (i in 1:length(nos)){
#  create_absence_shp(absent_sp[i], nos[i])
#}

create_absence_shp(absent_sp[3], nos[3])
# takes about 40 minutes to do 1, but the loop should work!
# start 2:21