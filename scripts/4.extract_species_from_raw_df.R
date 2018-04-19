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
#
#
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
