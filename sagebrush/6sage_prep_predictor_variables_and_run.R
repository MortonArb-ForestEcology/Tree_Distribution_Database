##6sage_prep_predictor_variables.R
if (!file.exists(file.path(model.outdir, SDMNAME))){dir.create(file.path(model.outdir, SDMNAME), recursive=TRUE)}
####################################################################################################
# source('SDM_fxns/RfitForestsSage.R')
####################################################################################################
# ##   extract the predictor variable values for each occurrence point
# var.ex <- extract(cur.w, occ)
# cdata <- cbind(occ@data, var.ex)
#   ## add rownames so that can use them in the random forest fitting part
# ##remove those with NA for the predictor variables
# cdata <- cdata[!is.na(cdata$bio_1),]
# #   rownames(cdata)
####################################################################################################
## prepare the variables sets
####################################################################################################
if (ms$var.layers == 'top 7 chosen by RF PCA') {
##   extract the predictor variable values for each occurrence point
var.ex <- extract(cur.w, occ)
# occ@data$lon <- coordinates(occ)[,1]
# occ@data$lat <- coordinates(occ)[,2]
cdata <- cbind(occ@data, var.ex)
	 head(cdata); tail(cdata)
  ## add rownames so that can use them in the random forest fitting part
##remove those with NA for the predictor variables
# cdata <- cdata[!is.na(cdata$AHM),] ##cdata$AHM for the AdaptWest data
# cdata <- cdata[!is.na(cdata$bio_1),] ##cdata$bio1 for the WorldClim data
# cdata <- cdata[!is.na(cdata$adi),] ##cdata$adi for the RMRS data

## need to remove all rows where a vale is NA
# cdata <- cdata[complete.cases(cdata[,cols]),]

pcas <- pca_subset(cdata, firstvar='AHM', nvars, stddev=1) ##firstvar='AHM' for the AdaptWest data
# pcas <- pca_subset(cdata, firstvar='XXX', nvars, stddev=1) ##firstvar='XXX' for the OakCons data
# pcas <- pca_subset(cdata, firstvar='bio_1', nvars) ##firstvar='bio1' for the WorldClim data
# pcas <- pca_subset(cdata, firstvar='adi', nvars, stddev=1) ##firstvar='adi' for the RMRS data
vars <- as.numeric(noquote(unlist(strsplit(pcas$var.layers, ','))))
nReps <- pcas$nReps
  } else if (ms$var.layers == 'top 4 chosen by RF PCA') {
##   extract the predictor variable values for each occurrence point
var.ex <- extract(cur.w, occ)
cdata <- cbind(occ@data, var.ex)
  ## add rownames so that can use them in the random forest fitting part
##remove those with NA for the predictor variables
cdata <- cdata[!is.na(cdata$AHM),] ##cdata$AHM for the AdaptWest data
# cdata <- cdata[!is.na(cdata$bio_1),] ##cdata$bio_1 for the WorldClim data
# cdata <- cdata[!is.na(cdata$adi),] ##cdata$adi for the RMRS data
#   rownames(cdata)

# pcas <- pca_subset(cdata, firstvar='bio_1', nvars) ##firstvar='bio1' for the WorldClim data
# pcas <- pca_subset(cdata, firstvar='adi', nvars, stddev=1) ##firstvar='adi' for the RMRS data
pcas <- pca_subset(cdata, firstvar='AHM', nvars, stddev=1) ##firstvar='adi' for the AdaptWest data
vars <- as.numeric(noquote(unlist(strsplit(pcas$var.layers, ','))))
nReps <- pcas$nReps
  } else if (ms$var.layers == 'top 5 chosen by RF PCA') {
##   extract the predictor variable values for each occurrence point
var.ex <- extract(cur.w, occ)
cdata <- cbind(occ@data, var.ex)
  ## add rownames so that can use them in the random forest fitting part
##remove those with NA for the predictor variables
cdata <- cdata[!is.na(cdata$AHM),] ##cdata$AHM for the AdaptWest data
# cdata <- cdata[!is.na(cdata$bio_1),] ##cdata$bio1 for the WorldClim data
# cdata <- cdata[!is.na(cdata$adi),] ##cdata$adi for the RMRS data
#   rownames(cdata)

pcas <- pca_subset(cdata, firstvar='AHM', nvars, stddev=1) ##firstvar='AHM' for the AdaptWest data
# pcas <- pca_subset(cdata, firstvar='bio_1', nvars) ##firstvar='bio1' for the WorldClim data
# pcas <- pca_subset(cdata, firstvar='adi', nvars, stddev=1) ##firstvar='adi' for the RMRS data
vars <- as.numeric(noquote(unlist(strsplit(pcas$var.layers, ','))))
nReps <- pcas$nReps
  } else if (ms$var.layers == 'top 6 chosen by RF PCA') {
##   extract the predictor variable values for each occurrence point
var.ex <- extract(cur.w, occ)
cdata <- cbind(occ@data, var.ex)
  ## add rownames so that can use them in the random forest fitting part
##remove those with NA for the predictor variables
cdata <- cdata[!is.na(cdata$AHM),] ##cdata$AHM for the AdaptWest data
# cdata <- cdata[!is.na(cdata$bio_1),] ##cdata$bio1 for the WorldClim data
# cdata <- cdata[!is.na(cdata$adi),] ##cdata$adi for the RMRS data
#   rownames(cdata)

# pcas <- pca_subset(cdata, firstvar='bio_1', nvars) ##firstvar='bio1' for the WorldClim data
# pcas <- pca_subset(cdata, firstvar='adi', nvars, stddev=1) ##firstvar='adi' for the RMRS data
pcas <- pca_subset(cdata, firstvar='AHM', nvars, stddev=1) ##firstvar='AHM' for the AdaptWest data
vars <- as.numeric(noquote(unlist(strsplit(pcas$var.layers, ','))))
nReps <- pcas$nReps
  } else if (ms$var.layers == 'top 8 chosen by RF PCA') {
##   extract the predictor variable values for each occurrence point
var.ex <- extract(cur.w, occ)
cdata <- cbind(occ@data, var.ex)
  ## add rownames so that can use them in the random forest fitting part
##remove those with NA for the predictor variables
cdata <- cdata[!is.na(cdata$AHM),] ##cdata$adi for the AdaptWest data
# cdata <- cdata[!is.na(cdata$bio_1),] ##cdata$bio1 for the WorldClim data
# cdata <- cdata[!is.na(cdata$adi),] ##cdata$adi for the RMRS data
#   rownames(cdata)

# pcas <- pca_subset(cdata, firstvar='bio_1', nvars) ##firstvar='bio1' for the WorldClim data
# pcas <- pca_subset(cdata, firstvar='adi', nvars, stddev=1) ##firstvar='adi' for the RMRS data
pcas <- pca_subset(cdata, firstvar='AHM', nvars, stddev=1) ##firstvar='AHM' for the AdaptWest data
vars <- as.numeric(noquote(unlist(strsplit(pcas$var.layers, ','))))
nReps <- pcas$nReps
  } else if (ms$var.layers == 'top 9 chosen by RF PCA') {
##   extract the predictor variable values for each occurrence point
var.ex <- extract(cur.w, occ)
cdata <- cbind(occ@data, var.ex)
  ## add rownames so that can use them in the random forest fitting part
##remove those with NA for the predictor variables
cdata <- cdata[!is.na(cdata$AHM),] ##cdata$AHM for the AdaptWest data
# cdata <- cdata[!is.na(cdata$bio_1),] ##cdata$bio1 for the WorldClim data
# cdata <- cdata[!is.na(cdata$adi),] ##cdata$adi for the RMRS data
#   rownames(cdata)

# pcas <- pca_subset(cdata, firstvar='bio_1', nvars) ##firstvar='bio1' for the WorldClim data
# pcas <- pca_subset(cdata, firstvar='adi', nvars, stddev=1) ##firstvar='adi' for the RMRS data
pcas <- pca_subset(cdata, firstvar='AHM', nvars, stddev=1) ##firstvar='AHM' for the AdaptWest data
vars <- as.numeric(noquote(unlist(strsplit(pcas$var.layers, ','))))
nReps <- pcas$nReps
  } else if (ms$var.layers == 'top 10 chosen by RF PCA') {
##   extract the predictor variable values for each occurrence point
var.ex <- extract(cur.w, occ)
cdata <- cbind(occ@data, var.ex)
  ## add rownames so that can use them in the random forest fitting part
##remove those with NA for the predictor variables
cdata <- cdata[!is.na(cdata$AHM),] ##cdata$AHM for the AdaptWest data
# cdata <- cdata[!is.na(cdata$bio_1),] ##cdata$bio1 for the WorldClim data
# cdata <- cdata[!is.na(cdata$adi),] ##cdata$adi for the RMRS data
#   rownames(cdata)

# pcas <- pca_subset(cdata, firstvar='bio_1', nvars) ##firstvar='bio1' for the WorldClim data
# pcas <- pca_subset(cdata, firstvar='adi', nvars, stddev=1) ##firstvar='adi' for the RMRS data
pcas <- pca_subset(cdata, firstvar='AHM', nvars, stddev=1) ##firstvar='AHM' for the AdaptWest data
vars <- as.numeric(noquote(unlist(strsplit(pcas$var.layers, ','))))
nReps <- pcas$nReps
  } else if (ms$var.layers == 'top var chosen by RF PCA') {
##   extract the predictor variable values for each occurrence point
var.ex <- extract(cur.w, occ)
cdata <- cbind(occ@data, var.ex)
  ## add rownames so that can use them in the random forest fitting part
##remove those with NA for the predictor variables
cdata <- cdata[!is.na(cdata$AHM),] ##cdata$AHM for the AdaptWest data
# cdata <- cdata[!is.na(cdata$bio_1),] ##cdata$bio1 for the WorldClim data
# cdata <- cdata[!is.na(cdata$adi),] ##cdata$adi for the RMRS data
#   rownames(cdata)

# pcas <- pca_subset(cdata, firstvar='bio_1', nvars) ##firstvar='bio1' for the WorldClim data
# pcas <- pca_subset(cdata, firstvar='adi', nvars, stddev=1) ##firstvar='adi' for the RMRS data
pcas <- pca_subset(cdata, firstvar='AHM', nvars, stddev=1) ##firstvar='AHM' for the AdaptWest data
vars <- as.numeric(noquote(unlist(strsplit(pcas$var.layers, ','))))
nReps <- pcas$nReps
  } else {
    print('this is not RandomForest, idiot')
vars <- as.numeric(noquote(unlist(strsplit(ms$var.layers, ','))))
nReps <- ms$reps
    }
