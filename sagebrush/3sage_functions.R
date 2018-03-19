##3sage_functions.R
####################################################################################################
##  pca_subset() chooses the variables through PCA; you define the number of variables
####################################################################################################
pca_subset <- function(cdata, firstvar='AHM', sname=SDMNAME, nvars, stddev=1){
																	 	##firstvar='AHM' for the first AdaptWest raster
																		##firstvar='bio_1' for the first WorldClim (Bioclim) raster
																		##firstvar='XXX' for the first OakCons raster
	
	outname <- paste0(sname, '_RFfits_')

	sink(file.path('SDM_output', paste0(outname, nm.extra, '.txt')), append=TRUE)
	cat(as.character(Sys.time()), '\n', sname, '\n', model_algorithm, ' model\n'); sink()

#   firstvar='bio_1'
require(randomForest)

spname = gsub('_', ' ', sname)
spname = gsub('var','var.',spname)
spname = gsub('subsp','subsp.',spname)

# spyes = cdata$SPCD
# cat ('Number of present observations: ',sum(spyes),'\n')

# fv = match('mat',colnames(cdata))
fv = match(firstvar, colnames(cdata))
cols=fv:ncol(cdata)
cdata <- cdata[complete.cases(cdata[,cols]),]

spyes <- cdata$sppres

prcdata=prcomp(cdata[,cols])$x[,1:2] # first 2 pcs, considering first 19 cols, all observations.
rownames(prcdata)=rownames(cdata)

sdin = apply(cdata[spyes==1, cols], 2, sd)
env  = apply(cdata[spyes==1, cols], 2, range)

nStd = find('nStandardDeviations')[1]
if (is.na(nStd)) nStd = 'no'
# 	cat ('nStd=', nStd,'\n'); flush.console()
	sink(file.path('SDM_output', paste0(outname, nm.extra, '.txt')), append=TRUE)
	cat ('nStd=', nStd,'\n'); sink(); flush.console()
if (nStd == '.GlobalEnv') if (!is.null(nStandardDeviations)) sdin=sdin*nStandardDeviations
env[1,]=env[1,]-sdin*stddev
env[2,]=env[2,]+sdin*stddev

tags = matrix(data=FALSE,nrow=nrow(cdata),ncol=length(cols))
colnames(tags)=colnames(cdata)[cols]
rownames(tags)=rownames(cdata)

for (var in colnames(tags)) {tags[, var] = cdata[,var] >= env[1,var] & cdata[,var] <= env[2,var]}

envBoth=apply(tags,1,all) ##for within both PC1 and PC2
envNotPresent = rownames(cdata)[envBoth & spyes==0]
InEnvSPNO = length(envNotPresent)
Nyes = sum(spyes)
# Nyes = nrow(cdata)

totsamp = 2*2.5*Nyes
size = totsamp*.4

prcdata=prcdata[!envBoth,]
div=ceiling(nrow(prcdata)/10)
PC1 = factor(floor(rank(prcdata[,1])/(div+.001)))
names(PC1)=NULL
ns=length(levels(PC1))
sz1=floor((totsamp*.2*.5)/ns)   # 20% for this method, half on first pc
PC2 = factor(floor(rank(prcdata[,2])/(div+.001)))
names(PC2)=NULL
ns=length(levels(PC2))
sz2=floor((totsamp*.2*.5)/ns)   # 20% for this method, half on second pc

nForests=round(InEnvSPNO/(2*Nyes))
if (nForests > 30) nForests=30
if (nForests < 10) nForests=10
if (Nyes > 5000) nForests = 5

obsSamp = vector('list', nForests)
names(obsSamp)=paste('S', 1:nForests, sep='')
ysp     <- obsSamp
forests <- obsSamp
repl.occs <- obsSamp

addNO = find('addNOObs')[1]
if (is.na(addNO)) addNO = 'no'
# cat ('addNO=', addNO,'\n'); flush.console()
	sink(file.path('SDM_output', paste0(outname, nm.extra, '.txt')), append=TRUE)
	cat ('addNO=', addNO,'\n'); sink(); flush.console()
if (addNO != 'no')
{
  norows=(nrow(cdata)+1):(nrow(cdata)+nrow(addNOObs))
  if (Nyes < nrow(addNOObs)) norows=sample(norows, Nyes)
  keepcols=intersect(colnames(cdata),colnames(addNOObs))
  cdata=rbind(cdata[,keepcols], addNOObs[,keepcols])
#   fv = match('mat',colnames(cdata))
  fv = match(firstvar, colnames(cdata))
  cols=fv:ncol(cdata)
} 

	sink(file.path('SDM_output', paste0(outname, nm.extra, '.txt')), append=TRUE)
	cat ('presence pts: ', Nyes,'\n', 'absence pts: ', nrow(bgdata), '\n\nOut-of-bag error:\n'); sink(); flush.console()

yesrows = (1:nrow(cdata))[spyes==1]
# n <- names(obsSamp)[1]
for (n in names(obsSamp))
{
  cat ('n=',n,'\n'); flush.console()
  sampPC1 = tapply(rownames(prcdata), PC1, sample, size=sz1)
  sampPC2 = tapply(rownames(prcdata), PC2, sample, size=sz2)
  allSampOut=unique(c(unlist(sampPC1), unlist(sampPC2)))

  sizNot = totsamp*.4
  sampNot = if (length(envNotPresent) > sizNot) sample(envNotPresent, sizNot) else  envNotPresent
  samp = c(allSampOut, sampNot)

  isamp = match(samp, rownames(cdata))
  
  repl.occ <- occ[rownames(occ@data) %in% c(yesrows, samp),]

  rows=c(yesrows, isamp, yesrows)  # yes rows are in twice

  if (addNO != 'no') rows=c(rows, if (Nyes < nrow(addNOObs)) sample(norows, Nyes) else norows)  

  obsSamp[[n]] = cdata[rows, cols]
  ysp[[n]] = as.factor(cdata$sppres[rows])
  repl.occs[[n]] <- repl.occ
}

vars = names(cdata)[cols]
##todel is the number of predictor variables to remove each time; Crookston chose 4 to cut 35 variables down to 18 or 8
# todel=2
todel=3
# todel=4
# require(multicore)
require(parallel)
while (length(vars) > 1)
{
  for (n in names(obsSamp))
  {
   mcparallel(randomForest(y=ysp[[n]], x=obsSamp[[n]][,vars], ntree=100, importance=TRUE,
                   proximity=FALSE, norm.votes=FALSE))
  }
  forests = mccollect()

	  if (length(vars) == nvars) {
	    names(forests) <- names(obsSamp)
	    save(forests, vars, cdata, ysp, obsSamp, repl.occs, sname, file=file.path(model.outdir, sname, paste0(sname, '_forests', nvars, '.', nm.extra,'.RData')))
	#     vars.layers <- paste( which(names(cdata[,cols]) %in% vars), collapse=',' )
	#     loadList <- list(paste( which(names(cdata[,cols]) %in% vars), collapse=',' ), nForests)
	#     names(loadList) <- c('vars.layers', 'nReps')
	
	  return(list(var.layers=paste(which(names(cdata[,cols]) %in% vars), collapse=','), nReps=nForests))
	#   return(loadList)
	  }

      gi = unlist(lapply(forests, function (x) { cbind(importance(x)[,'MeanDecreaseAccuracy'])} ))
      dim(gi)=c(length(vars), length(obsSamp))
      colnames(gi)=names(obsSamp)
      rownames(gi)=vars

      top = sort(apply(gi,1,mean), decreasing=TRUE, index.return=TRUE)$ix
      meanOOB = mean(unlist(lapply(forests, function (x) tail(x$err.rate[,'OOB'],1))))

			sink(file.path('SDM_output', paste0(outname, nm.extra, '.txt')), append=TRUE)
      cat ('meanOOB=', meanOOB, ' delete=', vars[top[(length(vars)-todel+1):length(vars)]], '\n'); sink(); flush.console()
	
      vars = vars[top[1:(length(vars)-todel)]]
			if (length(vars) > 17+todel){todel <- todel} else todel= max(1, todel-1)  	

	}
  names(forests)=names(obsSamp)  
# return(paste( which(names(cdata[,cols]) %in% vars), collapse=',' ))
}
####################################################################################################
##  other functions
####################################################################################################
## mean.list : calculate the mean of a list of matrices
mean.list <- function (x, ...) 
{
    if (!all(sapply(x, is.matrix))) 
        stop(''x' must be a list containing matrices')
    dims <- sapply(x, dim)
    n <- dims[1, 1]
    p <- dims[2, 1]
    if (!all(n == dims[1, ]) || !all(p == dims[2, ])) 
        stop('the matrices must have the same dimensions')
    mat <- matrix(unlist(x), n * p, length(x))
    mm <- matrix(rowMeans(mat, ...), n, p)
    dimnames(mm) <- dimnames(x[[1]])
    mm
}



####################################################################################################
# x <- file.path(r_locc, paste0(p.m.cur.reps.filename, '.t', th.v*100, '.tif'))
# pypath='/Library/Frameworks/Python.framework/Versions/2.7/Programs/gdal_polygonize.py'
# 
# gdal_polygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',
#                               pypath='/Library/Frameworks/GDAL.framework/Versions/1.9/Programs/gdal_polygonize.py', readpoly=TRUE, quiet=TRUE) { #, overshp=TRUE
#   if (is.null(pypath)) {
#         pypath <- Sys.which('gdal_polygonize.py')
#   }
#   if (!file.exists(pypath)) stop('Can't find gdal_polygonize.py on your system.')
#   owd <- getwd()
#   on.exit(setwd(owd))
#   # setwd(dirname(pypath))
#   if (!is.null(outshape)) {
#     outshape <- sub('\\.shp$', '', outshape)
#     f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
#     if (any(f.exists))
#       stop(sprintf('File already exists: %s',
#                    toString(paste(outshape, c('shp', 'shx', 'dbf'),
#                                   sep='.')[f.exists])), call.=FALSE)
#   } else outshape <- tempfile()
#   if (is(x, 'Raster')) {
#     require(raster)
#     writeRaster(r, {f <- tempfile(fileext='.tif')})
#     rastpath <- normalizePath(f)
#   } else if (is.character(x)) {
#     rastpath <- normalizePath(x)
#   } else stop('x must be a file path (character string), or a Raster object.')
#   system2('python', args=(sprintf(''%1$s' '%2$s' -f '%3$s' '%4$s.shp'',
#                                   pypath, rastpath, gdalformat, outshape)))
#   # system2('python', args=(sprintf(''%1$s' '%2$s' -f '%3$s' '%4$s.shp'',
#   #                                 pypath, rastpath, gdalformat, outshape)))
#   if (readpoly) {
#     shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
#     return(shp)
#   }
#   return(NULL)
# }
# # x <- in.raster
# # r <- in.raster
# gdal_polygonizeR <- function(x=in.raster, outshape=NULL, gdalformat = 'ESRI Shapefile',
#                              pypath='/Library/Frameworks/GDAL.framework/Versions/1.9/Programs/gdal_polygonize.py', readpoly=TRUE, quiet=TRUE) { #, overshp=TRUE
#   if (is.null(pypath)) {
#         pypath <- Sys.which('gdal_polygonize.py')
#   }
#   if (!file.exists(pypath)) stop('Can't find gdal_polygonize.py on your system.')
#   owd <- getwd()
#   on.exit(setwd(owd))
#   # setwd(dirname(pypath))
#   if (!is.null(outshape)) {
#     outshape <- sub('\\.shp$', '', outshape)
#     f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
#     if (any(f.exists))
#       stop(sprintf('File already exists: %s',
#                    toString(paste(outshape, c('shp', 'shx', 'dbf'),
#                                   sep='.')[f.exists])), call.=FALSE)
#   } else outshape <- tempfile()
# #   if (is(x, 'Raster')) {
# #     require(raster)
# #     writeRaster(r, {f <- tempfile(fileext='.tif')})
# # #     writeRaster(x, {f <- tempfile(fileext='.asc')})
# #     rastpath <- normalizePath(f)
# #   } else if (is.character(x)) {
# #     rastpath <- normalizePath(x)
# #   } else stop('x must be a file path (character string), or a Raster object.')
#   system2('python', args=(sprintf("'%1$s' '%2$s' -f '%3$s' '%4$s.shp'",
#                                   pypath, rastpath, gdalformat, outshape)))
#   # system2('python', args=(sprintf("'%1$s' '%2$s' -f '%3$s' '%4$s.shp'",
#   #                                 pypath, rastpath, gdalformat, outshape)))
#   if (readpoly) {
#     shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
#     return(shp)
#   }
#   return(NULL)
# }

# ##the fxn to call
# thresh_raster2poly <- function(in.raster, out.dir=s_loc, outshp){
# 	# out.dir <- file.path(f_out, 'poly_models_thresh', dir.nm)
# 		if (!file.exists(out.dir)){dir.create(out.dir, recursive=TRUE)}
# 	# in.raster <- file.path(f_out, 'raster_models', eachPath)
# 	# out.nm <- gsub(pattern='.tif', replacement='', base.nm)
# 	# outshp <- file.path(f_out, 'poly_models_thresh', dir.nm, out.nm)
# 
# 	gdal_polygonizeR(in.raster, outshape=outshp, readpoly=FALSE)
# 
# }


####################################################################################################
# ##kfold the presence and absence occurrences
# kfold_set <- function(pocc, nocc, k.folds=5, proj2use='+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs'){
#       test_per <- (1/k.folds)*100    
#     nr <- nrow(pocc) #; nr
#     s <- sample(nr, 1/k.folds * nr)
#       ptrain <- pocc[-s, ]
#       ptest  <- pocc[s, ]
#       
#     nr <- nrow(nocc) #; nr
#     s <- sample(nr, 1/k.folds * nr)
#       atrain <- nocc[-s, ]
#       atest  <- nocc[s, ]
# 
#     train <- rbind(ptrain, atrain)
#     test  <- rbind(ptest, atest)
# #   names(occ2)
#     occ.train <- SpatialPointsDataFrame(cbind(train1$x_coord, train1$y_coord), train)
#     occ.test  <- SpatialPointsDataFrame(cbind(test1$x_coord, test1$y_coord), test)
# #     occ <- SpatialPointsDataFrame(cbind(occ.c2$x_coord, occ.c2$y_coord), occ.c2)
# proj4string(occ.train) <- proj2use
# proj4string(occ.test)  <- proj2use
# 
# return(list(occ.train, occ.test))
# }
