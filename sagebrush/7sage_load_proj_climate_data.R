##7prep_climate_data.R
####################################################################################
#bring in file with climate codes
####################################################################################
library(readxl)
# clims <- read.delim('climate/clim_codes.txt', header=TRUE, sep='\t', as.is=TRUE)
clims <- read_excel('climate/clim_codes.xlsx')

##list names
# bio.names <- c('bio_1', 'bio_2', 'bio_3', 'bio_4', 'bio_5', 
#         			 'bio_6', 'bio_7', 'bio_8', 'bio_9', 'bio_10', 
# 			    		 'bio_11', 'bio_12', 'bio_13', 'bio_14', 'bio_15', 
# 					     'bio_16', 'bio_17', 'bio_18', 'bio_19')
  options(java.parameters = '-Xmx16g')

clim <- clims[clims$fullmod == model.climate,]
prj.fldr <- clim$folder

lyrs <- list.files(paste0(r.prj.loc, prj.fldr), full.names=TRUE) ## for the OakCons rasters; Folder can be whatever folder name we choose. The default here is 'OakCons' but can be changed.
# lyrs <- list.files(paste0('climate/AdaptWest/future/', prj.fldr), full.names=TRUE) ## for the AdaptWest rasters
# lyrs <- list.files(paste0('climate/RMRS/future/', prj.fldr), full.names=TRUE) ## for the RMRS rasters
# lyrs <- filter(lyrs, basename())
# lyrs <- filter(lyrs, grepl('.tif', ))
bio.names <- gsub('.tif', '', gsub('oak_big_', '', basename(lyrs)))
#
####################################################################################
# bio.names <- c('d100', 'dd0', 'dd5', 'fday', 'ffp', 
# 					'gsdd5', 'gsp', 'map', 'mat', 'mmax', 
# 					'mmin', 'mtcm', 'mtwm', 'mmindd0', 'sday', 
# 					'smrp', 'smrpb', 'sprp', 'winp', 'adi', 
# 					'sdi', 'pratio', 'tdiff', 'adimindd0', 'gspdd5', 
# 					'gspmtcm', 'gsptd', 'mapdd5', 'mapmtcm', 'maptdiff', 
# 					'mtcmgsp', 'mtcmmap', 'sdimindd0', 'tdgsp', 'tdmap', 
# 					'sdimtcm', 'dd0map', 'dd0gsp', 'adimtcm', 'dd5gsp', 
# 					'dd5mtcm', 'pratiodd5', 'pratiomtcm'
# )
####################################################################################

####################################################################################
#####  Load rasters  ###############################################################################
####################################################################################
#loadRasters.R
require(raster)
  options(java.parameters = '-Xmx16g')
sdm.prj <- stack(lyrs)
names(sdm.prj) <- bio.names
#
rm(lyrs)
prj <- sdm.prj

rm(sdm.prj)
##rerun the polygon file if corrupted or missing
# source('make_sagebrush_area_polygon.R')

##bring in the area polygon to which you will be cropping/clipping/masking all of your rasters
clip_area <- readOGR('Global_Admin_Areas', 'oak_area')
# sage_area <- readOGR('Global_Admin_Areas/sagebrush_area', 'sagebrush_area55')
# proj4string(west) <- '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs'

##change the projection to match your core rasters...with the oaks it is likely PRISM or dayMet
clip_area <- spTransform(sage_area, CRS('+proj=lcc +lat_1=49.0 +lat_2=77.0 +lat_0=0.0 +lon_0=-95.0 +x_0=0.0 +y_0=0.0 +ellps=WGS84 +units=m +no_defs'))
# clip_area <- spTransform(sage_area, CRS('+proj=lcc +lat_1=49.0 +lat_2=77.0 +lat_0=0.0 +lon_0=-95.0 +x_0=0.0 +y_0=0.0 +ellps=WGS84 +units=m +no_defs')) ## this is for the AdaptWest rasters

