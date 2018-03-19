##sagebrush_sdm_prj_MC.R
rm(list=ls())
getwd()

####################################################################################################
###  run projections for the future
####################################################################################################
## Set the taxa to project
	SDMNAME         <- 'Artemisia_tridentata_subsp_wyomingensis'

## Set the source location of the projection rasters
	r.prj.loc <- 'climate/OakCons/future/'
	

## Set the number of variables (rasters) that was used in the model.
	nvars <- 7 # nvars <- 1 

## Set the variable set to use, which is pulled from 'variable_set.xlsx' in another script.
	mset  <- 'm193' ##top 7 chosen by PCA

## Set the variable set that we will use to project the models
	vars.incl <- 'set75'

## Set the model algorithm that we used to make the models.
	model_algorithm <- 'RandomForest'

## Set the threshold value for rasters
	th.v <- 0.6

## Load the climate codes Excel file
library(readxl)
	clims <- read_excel('climate/clim_codes.xlsx')
# f.clims <- clims[clims$suitability == 'y' & clims$yrs == '2020s',]
# yrs <- c('2080s')

## Set the decades to project the model. This can be one or more years and the code will iterate through the decades.
yrs <- c('2020s', '2050s', '2080s')

## Set the climates to which you want to project the model (tyhis is the prefix to all of our OakCons raster data)
				## We will probably have a specific file name prefix for the oak data rasters
				## For sagebrush, we chose an ensemble model at both rcp 4.5 and rcp 8.5, the climate projection extremes
	model.climates <- c('Ensemble_rcp45_', 'Ensemble_rcp85_')

	for (yr in yrs){
		print(yr)
		for (model.climate in model.climates){
			print(model.climate)
	
	f.clims <- clims[clims$yrs == yr,]
	
	model.climate <- paste0(model.climate, yr)

## Arrange the climate dataframe into order by the emission scenario
require(plyr)
require(rgdal)

	f.clims <- arrange(f.clims, rcp, fullmod)

##now loop through all future clims that need to be run using the model from current conditions
# for (m in 1:nrow(f.clims)){

# model.climate <- f.clims[m,]$fullmod

cat('Starting the future projection for ', model.climate, '.\n')
###  1. set basic parameters, packages and working directories
####################################################################################################
source('SDM_fxns/1sage_set_sdm_model_run.R')
####################################################################################################
###  2. prepare/load climate (predictor) data, including the area spatialpolygon
####################################################################################################
source('SDM_fxns/7sage_load_proj_climate_data.R')
####################################################################################################
###  3. load functions
####################################################################################################
source('SDM_fxns/3sage_functions.R')
####################################################################################################
###  4. set parameters and variables
####################################################################################################
source('SDM_fxns/4sage_set_parameters.R')

## Set the name extra
nm.extra <- paste0(nvars, 'v_', ms$neg.occ, '_prism') ## 'prism' is extension for runs using PRISM climate data
# nm.extra <- paste0(nvars, 'v_', ms$neg.occ, '_aw') ## "aw" is extension for runs using AdaptWest climate data
# nm.extra <- paste0(nvars, 'v_', ms$neg.occ, '_rmrs') ## "rmrs" is extension for runs using Rocky Mountain Research Station climate data
# nm.extra <- paste0(nvars, 'v_', ms$neg.occ, '_ra')
# nm.extra <- paste0(nvars, 'v_', ms$neg.occ, '_response')

###  Run projections for the future
load(file.path('SDM_output', 'model_output', SDMNAME, paste0(SDMNAME, '_forests', nvars, '.', nm.extra,'.RData')))

v.sets <- read_excel('variable_sets.xlsx')
  vs <- v.sets[v.sets$var.layers == ms$var.layers & v.sets$mod.set == ms$mset,]
predictors <- subset(prj, vars)

require(randomForest)
require(raster)
# require(multicore)

p.rf <- vector('list', length(forests))
names(p.rf) <- names(forests)

  for (n in names(forests)) {
print(paste('Starting', names(forests[n])))
  	p.rf[[n]] <- predict(predictors, forests[[n]], ext=extent(prj), type='response', index=2, na.rm=TRUE, progress='text')
#   	p.rf[[n]] <- predict(predictors, forests[[n]], ext=extent(prj), type='prob', index=2, na.rm=TRUE, progress='text')
  }

p.rfs <- stack(p.rf)

  p.m.prj.fname <- paste0(SDMNAME, '.', model.climate,'.', vs$vi, '.', nm.extra, '.', ms$ma)

    save(p.rfs, forests, vars, cdata, ysp, obsSamp, repl.occs, SDMNAME, file=file.path(model.outdir, SDMNAME, paste0(p.m.prj.fname,'.RData')))

if (!file.exists(r_loc)){dir.create(r_loc, recursive=TRUE)}
#     load(file.path(model.outdir, SDMNAME, paste0(p.m.prj.fname,'.RData')))
  p.m.curr.mean <- calc(p.rfs, mean, filename=file.path(r_loc, paste0(p.m.prj.fname, '.tif')), overwrite=TRUE)
    p.m.curr.thresh <- p.m.curr.mean > th.v
	    			# plot(p.m.curr.thresh)
    			writeRaster(p.m.curr.thresh, filename=file.path(r_loc, paste0(p.m.prj.fname, '.t', th.v*100, '.tif')), format='GTiff', datatype='INT1U', overwrite=TRUE, progress='text')

	}
}
# 
#   ##convert raster to polygon and output shapefile
# 		## first, create the directory (if not existing already) for the shapefile polygon
# 		if (!file.exists(s_loc)){dir.create(s_loc, recursive=TRUE)}
# 
# 
#   p.m.prj.fname <- paste0(SDMNAME, '.', model.climate,'.', vs$vi, '.', nm.extra, '.', ms$ma)
# 		outshp <- file.path(s_loc, paste0(p.m.prj.fname, '.t', th.v*100))
# 		
# 	 thresh_raster2poly(in.raster=p.m.curr.thresh, out.dir=s_loc, outshp)
# # plot(p.m.curr.thresh)
