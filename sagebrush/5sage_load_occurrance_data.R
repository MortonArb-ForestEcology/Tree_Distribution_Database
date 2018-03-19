##5sage_load_occurrance_data.R
library(readxl)
occ.common <- read_excel(spp.occ.loc)

##add column to indicate that species is present (1=present; 0=absent)
occ.common$sppres <- 1
attach(occ.common)
occ.common <- occ.common[,cbind('id_num', 'site', 'sppres', 'SDMNAME', 'x_coord', 'y_coord', 'elev_m')]

detach(occ.common)
names(occ.common) <- c('id_num', 'site', 'sppres', 'SDMNAME', 'x_coord', 'y_coord', 'elev_m')

# occ.c2 <- occ.common[,c('id_num', 'site', 'sppres', 'SDMNAME', 'x_coord', 'y_coord', 'elev_m')]
# ids3 <- unique(as.character(occ.c2$SDMNAME))
# 
#   	occ <- SpatialPointsDataFrame(cbind(occ.c2$x_coord, occ.c2$y_coord), occ.c2)
# proj4string(occ) <- '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs'

####################################################################################
##prep the targeted group absence data (for background)
####################################################################################
#       load('plants/sagebrush/gambleOak.RData')
# #               head(gambleOak, 2)
#       load('plants/sagebrush/cele3.RData')
# #               head(cele3, 2)
#       load('plants/sagebrush/pimo.RData')
# #               head(pimo, 2)
# 
#       blackbrush <- read.csv('plants/sagebrush/CORA_loc_Bryce.csv', as.is=TRUE, header=TRUE)
#       cali <- read.csv('plants/sagebrush/California_absence_pts_2014-02-13.csv', as.is=TRUE, header=TRUE)
# #         head(cali)
# #         unique(cali$taxon_name)
# 
#       gambleOak$SDMNAME <- 'Quercus_gambelii'
#       cele3$SDMNAME     <- 'Cercocarpus_ledifolius'
#       pimo$SDMNAME      <- 'Pinus_monophylla'
#       cali$SDMNAME      <- gsub('\\.', '', cali$taxon_name)
#       cali$SDMNAME      <- gsub(' ', '_', cali$SDMNAME)
#         cali$verbatim_elevation[cali$verbatim_elevation == ''] <- NA
# 
#       go <- gambleOak[,c('CN', 'SDMNAME', 'x_coord', 'y_coord', 'elev')]
#       cl <- cele3[,c('CN', 'SDMNAME', 'x_coord', 'y_coord', 'elev')]
#       pm <- pimo[,c('CN', 'SDMNAME', 'x_coord', 'y_coord', 'elev')]
#       bb <- blackbrush[,c('PointID', 'species', 'long', 'y_coord', 'elev_m')]
#       ca <- cali[,c('Accession_id', 'SDMNAME', 'x_coord', 'y_coord', 'verbatim_elevation')]
# 
#       names(go) <- c('uid', 'SDMNAME', 'x_coord', 'y_coord', 'elev'); names(cl) <- c('uid', 'SDMNAME', 'x_coord', 'y_coord', 'elev')
#       names(pm) <- c('uid', 'SDMNAME', 'x_coord', 'y_coord', 'elev'); names(bb) <- c('uid', 'SDMNAME', 'x_coord', 'y_coord', 'elev')
#       names(ca) <- c('uid', 'SDMNAME', 'x_coord', 'y_coord', 'elev')
# 
#       bgdata <- rbind(go, cl, pm, bb, ca)
#         head(bgdata); tail(bgdata)
# 
#       ##don't include Pinus monophylla for now
# #       bgdata <- bgdata[bgdata$SDMNAME != 'Pinus_monophylla',]
# # 
#       write.table(bgdata, file.path('plants', 'sagebrush', 'sagebrush_tga.csv'), sep=',', row.names=FALSE, col.names=TRUE, append=FALSE)
####################################################################################
##load targeted group data for background
####################################################################################
  bgdata <- read.csv('plants/sagebrush/sagebrush_tga.csv', as.is=TRUE)
#   abs.pts <- read.csv('plants/sagebrush/sagebrush_atw_random_abs.csv', as.is=TRUE)
####################################################################################
# 
# abs.pts$uid <- 100001:(nrow(abs.pts)+100000)
# abs.pts$uid <- paste0('random.abs_', abs.pts$uid)
# abs.pts$SDMNAME <- 'random_absence'
# abs.pts$elev <- 'NA'
# abs.pts <- abs.pts[,c('uid', 'SDMNAME', 'x_coord', 'y_coord', 'elev')]
# 
# # abs.pts <- cbind(abs.pts$uid, abs.pts$SDMNAME, abs.pts$x_coord, abs.pts$y_coord, abs.pts$elev)
# abs.pts$sppres <- 0
# abs.pts$site <- abs.pts$uid
# 
# bgdata <- rbind(bgdata, abs.pts)
# 
#       write.table(bgdata, file.path('plants', 'sagebrush', 'sagebrush_tga.csv'), sep=',', row.names=FALSE, col.names=TRUE, append=FALSE)
# 
# head(bgdata)
# names(bgdata)

bgdata$sppres <- 0
bgdata$site <- bgdata$uid
bgdata <- bgdata[,c('uid', 'site', 'sppres', 'SDMNAME', 'x_coord', 'y_coord', 'elev')]
	head(bgdata)
occ.c2 <- occ.common[,c('id_num', 'site', 'sppres', 'SDMNAME', 'x_coord', 'y_coord', 'elev_m')]

names(bgdata) <- names(occ.c2)
# addNOobs <- bgdata
ids3 <- unique(as.character(occ.c2$SDMNAME))

##combine the presence and absence data
occ2 <- rbind(occ.c2, bgdata)
#   names(occ2)
    occ <- SpatialPointsDataFrame(cbind(occ2$x_coord, occ2$y_coord), occ2)
    	## set the projection
			proj4string(occ) <- '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs'
## for a different projection to match the AdaptWest projection
occ <- spTransform(occ, CRS('+proj=lcc +lat_1=49.0 +lat_2=77.0 +lat_0=0.0 +lon_0=-95.0 +x_0=0.0 +y_0=0.0 +ellps=WGS84 +units=m +no_defs'))

####################################################################################
# file.path('SDM_fxns', 'sage_occ.set.R')
