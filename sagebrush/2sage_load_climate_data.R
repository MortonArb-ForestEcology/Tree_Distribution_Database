##2prep_climate_data.R
####################################################################################
#bring in file with climate codes
####################################################################################
clims <- read.delim("climate/clim_codes.txt", header=TRUE, sep="\t", as.is=TRUE)

##list names
lyrs <- list.files("climate/AdaptWest/current", full.names=TRUE)
# lyrs <- list.files("climate/RMRS/current/", full.names=TRUE)
bio.names <- gsub(".tif", "", gsub("sage55_", "", basename(lyrs)))
#
####################################################################################
# bio.names <- c("d100", "dd0", "dd5", "fday", "ffp", 
# 					"gsdd5", "gsp", "map", "mat", "mmax", 
# 					"mmin", "mtcm", "mtwm", "mmindd0", "sday", 
# 					"smrp", "smrpb", "sprp", "winp", "adi", 
# 					"sdi", "pratio", "tdiff", "adimindd0", "gspdd5", 
# 					"gspmtcm", "gsptd", "mapdd5", "mapmtcm", "maptdiff", 
# 					"mtcmgsp", "mtcmmap", "sdimindd0", "tdgsp", "tdmap", 
# 					"sdimtcm", "dd0map", "dd0gsp", "adimtcm", "dd5gsp", 
# 					"dd5mtcm", "pratiodd5", "pratiomtcm"
# )
####################################################################################

####################################################################################
#####  Load rasters  ###############################################################################
####################################################################################
#loadRasters.R
require(raster)
  options(java.parameters = "-Xmx16g")
sage.cur <- stack(lyrs)
names(sage.cur) <- bio.names
#
rm(lyrs)
cur <- sage.cur
cur.w <- cur

rm(sage.cur)
##rerun the polygon file if corrupted or missing
# source("make_sagebrush_area_polygon.R")
require(rgdal)
##bring in the sagebrush area polygon
sage_area <- readOGR("Global_Admin_Areas/sagebrush_area", "sagebrush_area55")
# proj4string(west) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
sage_area <- spTransform(sage_area, CRS('+proj=lcc +lat_1=49.0 +lat_2=77.0 +lat_0=0.0 +lon_0=-95.0 +x_0=0.0 +y_0=0.0 +ellps=WGS84 +units=m +no_defs'))

