rm(list=ls())
##sagebrush_sdm_MC.R
getwd()
####################################################################################################
##To start: set your basic parameters here, including file paths for the species occurrence data.
####################################################################################################
## Set the location of the occurrence data
		spp.occ.loc <- 'plants/sagebrush/4Xbigsagebrush.csv'
		# spp.occ.loc <- 'occurrence_data/oaks_R_us.xlsx' # 'oaks_R_us.xlsx' or whatever we call this file.

## Set the taxa to model
SDMNAME         <- 'Artemisia_tridentata_subsp_wyomingensis'
# SDMNAME         <- 'Artemisia_tridentata_tetraploids'

## Set the climate to which you want to model
				## I would suggest we use 'current' or 'baseline' for the dataset on which the models are created	
				## Maybe we could use 'current' for a strict number of years and use 'baseline' if we vary the climate data for the years collected
model.climate   <- 'current' 

## Set the number of variables (rasters) to use in the model. The RandomForests algorithm will reduce our larger dataset to this many variables.
nvars <- 7 # nvars <- 1 

## Set the variable set to use, which is pulled from 'variable_set.xlsx' in another script.
mset            <- 'm193' ##top 7 chosen by PCA
	# mset            <- 'm001' ##2,8,10,12,17
	# mset            <- 'm010' ##top 4 chosen by PCA
	# mset            <- 'm011' ##top 5 chosen by PCA
	# mset            <- 'm012' ##top 6 chosen by PCA
	# mset            <- 'm013' ##top 8 chosen by PCA
	# mset            <- 'm014' ##top 9 chosen by PCA
	# mset            <- 'm015' ##top 10 chosen by PCA
	# mset            <- 'm241' ##top 10 chosen by PCA
	# mset            <- 'm016' ##top 10 chosen by PCA
	# mset            <- 'm233' ##top 7 chosen by PCA

## Set the model algorithm. For the OakCons project, this will likely always be RandomForest
model_algorithm <- 'RandomForest'
	# model_algorithm <- 'MaxEnt'

## Set the threshold value for rasters (this is if setting a threshhold value for output rasters to measure total area)
th.v <- 0.6

####################################################################################################
###  1. set basic parameters, packages and working directories
####################################################################################################
source('SDM_fxns/1sage_set_sdm_model_run.R')

####################################################################################################
###  2. prepare/load climate (predictor) data, including the area spatialpolygon
####################################################################################################
source('SDM_fxns/2sage_load_climate_data.R')

####################################################################################################
###  3. load functions
####################################################################################################
source('SDM_fxns/3sage_functions.R')

####################################################################################################
###  4. set parameters and variables
####################################################################################################
source('SDM_fxns/4sage_set_parameters.R')

####################################################################################################
##set the name extra
nm.extra <- paste0(nvars, 'v_', ms$neg.occ, '_prism') ## 'prism' is extension for runs using PRISM climate data
nm.extra <- paste0(nvars, 'v_', ms$neg.occ, '_aw') ## 'aw' is extension for runs using AdaptWest climate data
# nm.extra <- paste0(nvars, 'v_', ms$neg.occ)
# nm.extra <- paste0(nvars, 'v_', ms$neg.occ, '_ra')
# nm.extra <- paste0(nvars, 'v_', ms$neg.occ, '_rmrs') ## 'rmrs' is extension for runs using Rocky Mountain Research Station climate data
# nm.extra <- paste0(nvars, 'v_', ms$neg.occ, '_response')

####################################################################################################
####################################################################################################
###
### this may be where to start parallel code
###
####################################################################################################
####################################################################################################


####################################################################################################
###  5. load occurrance data (presences and absences)
####################################################################################################
source('SDM_fxns/5sage_load_occurrance_data.R')
##remove Q. gambelii
##remove some of the common plants that don't have many occurrences or that seem out of place
	##plants to remove
# 		rm.pls <- c('Pinus_monophylla', 'Rhamnus_pilosa', 'Rhamnus_pirifolia', 'Dendromecon_harfordii', 'Quercus_gambelii')
		rm.pls <- c('Pinus_monophylla', 'Quercus_gambelii')
# 		rm.pls <- c('Pinus_monophylla')

bgdata  <-  bgdata[!bgdata$SDMNAME %in% rm.pls, ]
occ2    <-  occ2[!occ2$SDMNAME %in% rm.pls, ]
occ     <-  occ[!occ@data$SDMNAME %in% rm.pls, ]

	rm(rm.pls)
####################################################################################################
###  6. prepare predictor variables
####################################################################################################
source('SDM_fxns/6sage_prep_predictor_variables_and_run.R')
####################################################################################################
##load forests data for saving outputs
load(file.path('SDM_output', 'model_output', SDMNAME, paste0(SDMNAME, '_forests', nvars, '.', nm.extra,'.RData')))

##save variables for pulling into Maxent models
sink(file.path('SDM_output', paste0('sagebrush_RFfits_', nm.extra, '.txt')), append=TRUE)
vars.e <- names(cur[[vars]])
cat('\nVariables: ', vars.e, '\n\nConfusion matrices:\n\n')

lapply(forests, '[[', 'confusion')

cat('\nMean confusion matrix:\n\n')

mean.list(lapply(forests, '[[', 'confusion'))

cat('\nMean importance:\n\n')

mean.list(lapply(forests, '[[', 'importance'))

cat('\n#########################\n'); sink()

write.table(vars, file.path('SDM_output', paste0('sagebrush_RF_', nm.extra, '.txt')), sep=',', row.names=FALSE, col.names=FALSE, append=FALSE)
write.table(mean.list(lapply(forests, '[[', 'importance')), file.path('SDM_output', paste0('sagebrush_RF_', nm.extra, '_mean_importance', '.txt')), sep=',', row.names=TRUE, col.names=TRUE, append=FALSE)
write.table(mean.list(lapply(forests, '[[', 'confusion')), file.path('SDM_output', paste0('sagebrush_RF_', nm.extra, '_mean_confusion', '.txt')), sep=',', row.names=TRUE, col.names=TRUE, append=FALSE)
# write.table(vars, file.path('SDM_output', paste0('sagebrush_RF_', nm.extra, '.txt')), sep=',', row.names=FALSE, col.names=FALSE, append=FALSE)

##  7. load the forests data
load(file.path('SDM_output', 'model_output', SDMNAME, paste0(SDMNAME, '_forests', nvars, '.', nm.extra,'.RData')))


v.sets <- read.delim('variable_sets.txt', sep='\t', header=TRUE, as.is=TRUE)
  vs <- v.sets[v.sets$var.layers == ms$var.layers & v.sets$mod.set == ms$mset,]
predictors <- subset(cur, vars)

require(randomForest)
require(raster)
# require(multicore)

p.rf <- vector('list', length(forests))
names(p.rf) <- names(forests)

  for (n in names(forests)) {
  # for (n in names(forests)) {
print(paste('Starting', names(forests[n])))
  	p.rf[[n]] <- predict(predictors, forests[[n]], type='prob', index=2, na.rm=TRUE, progress='text') #ext=extent(cur) has been removed
  	# p.rf[[n]] <- predict(predictors, forests[[n]], ext=extent(cur), type='prob', index=2, na.rm=TRUE, progress='text')
#   	p.rf[[n]] <- predict(predictors, forests[[n]], ext=extent(cur), type='response', index=2, na.rm=TRUE, progress='text')
  }

# p.rfs <- stack(p.rf)
p.rfs <- stack(p.rf)

    save(p.rfs, forests, vars, cdata, ysp, obsSamp, repl.occs, SDMNAME, file=file.path(model.outdir, SDMNAME, paste0(SDMNAME, '.', vs$vi, '.', ms$ma, '.', nm.extra,'.RData')))

    load(file.path(model.outdir, SDMNAME, paste0(SDMNAME, '.', vs$vi, '.', ms$ma, '.', nm.extra,'.RData')))

  p.m.cur.reps.filename <- paste0(SDMNAME, '.', model.climate,'.', vs$vi, '.', nm.extra, '.', ms$ma)

if (!file.exists(r_loc)){dir.create(r_loc, recursive=TRUE)}

# 	p.m.curr.sum <- sum(p.rfs)
  ## create mean raster
  p.m.curr.mean <- calc(p.rfs, mean, filename=file.path(r_locc, paste0(p.m.cur.reps.filename, '.tif')), overwrite=TRUE)
		# plot(p.rfs)
		# plot(p.m.curr.mean)

  ## create binary raster of predicted suitable areas with threshold value of 0.60
    p.m.curr.thresh <- p.m.curr.mean > th.v
	    			# plot(p.m.curr.thresh)
    			writeRaster(p.m.curr.thresh, filename=file.path(r_locc, paste0(p.m.cur.reps.filename, '.t', th.v*100, '.tif')), format='GTiff', datatype='INT1U', overwrite=TRUE, progress='text')
    			
#     			
#   ##convert raster to polygon and output shapefile
# 		## first, create the directory (if not existing already) for the shapefile polygon
# 		if (!file.exists(s_loc)){dir.create(s_locc, recursive=TRUE)}
# 
# 
#   p.m.cur.fname <- paste0(SDMNAME, '.', model.climate,'.', vs$vi, '.', nm.extra, '.', ms$ma)
# 		outshp <- file.path(s_locc, paste0(p.m.cur.fname, '.t', th.v*100))
# 		
# 		r2p <- rasterToPolygons(p.m.curr.thresh, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
# 		
# 		writeOGR(r2p, dsn=dirname(outshp), layer=basename(outshp), driver='ESRI Shapefile', overwrite_layer=TRUE)
# 		
# 	 # thresh_raster2poly(in.raster=p.m.curr.thresh, out.dir=s_locc, outshape=outshp)
# plot(p.m.curr.thresh)

