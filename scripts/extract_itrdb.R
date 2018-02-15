extract.itrdb <- function(area.extract=NULL, download.types=c("Chronology", "Raw Measurements", "Correlation Stats"), dir.out){
  library(curl)
  library(XML)
  library(stringr)
  library(raster)
  library(RCurl)
  library(jsonlite)
  
  if(!dir.exists(dir.out)) dir.create(dir.out, recursive=T)
  
  if(is.null(area.extract)){
    stop("no shapefile/bounding box provided! Do you really want ALL ITRDB locations?? If so, provide a bounding box for the whole world")
  }
  
  itrdb.base <- paste0("https://www.ncdc.noaa.gov/paleo-search/study/search.json?headersOnly=true&dataPublisher=NOAA&dataTypeId=18")

  xmin = xmin(area.extract)
  xmax = xmax(area.extract)
  ymin = ymin(area.extract)
  ymax = ymax(area.extract)
  
  
  itrdb.url <- paste0(itrdb.base, "&minLat=",ymin, "&maxLat=", ymax,
                      "&minLon=", xmin, "&maxLon=", xmax)

  noaa.meta <- RCurl::getURL(itrdb.url)
  noaa.meta <- jsonlite::fromJSON(noaa.meta)$study
  
  for(i in 1:ncol(noaa.meta)){
    noaa.meta[,i] <- as.factor(noaa.meta[,i])
  }
  summary(noaa.meta)
  
  # Set up where we're going to store everything
  # itrdb.metadata <- data.frame(ID.study=NA, investigators=NA, studyCode=NA, species=NA, lat=NA, lon=NA, elev=NA, ITRDB.id=NA, yr.min=NA, yr.max=NA)
  
  # Working with one site for now; 
  # # will need to add loop here in future
  pb <- txtProgressBar(min=0, max=nrow(noaa.meta), style=3)
  for(i in 1:nrow(noaa.meta)){
    setTxtProgressBar(pb, i)
    
    id.xml <- noaa.meta[i, "xmlId"]
    
    # Download and save the XML document with all of the metadata for the site
    # We can extract info from this site for our use later
    site.dat <- jsonlite::fromJSON(paste0("https://www.ncdc.noaa.gov/paleo-search/study/search.json?xmlId=", id.xml))$study
    # summary(site.dat)
    
    # itrdb.metadata[i,"investigators" ]  <- site.dat$investigators
    site.coords <- rev(as.numeric(site.dat$site[[1]]$geo$geometry$coordinates[[1]]))
    site.sp <- SpatialPoints(coords = matrix(site.coords, ncol=2), proj4string=CRS(projection(area.extract)))
    in.poly <- !is.na(over(site.sp, area.extract)[1])
    
    if(in.poly==FALSE) next
    
    noaa.meta[i ,"studyCode"] <- site.dat$studyCode
    noaa.meta[i, "siteName" ] <- site.dat$site[[1]]$siteName
    noaa.meta[i, "Longitude"] <- site.coords[1]
    noaa.meta[i, "Latitude" ] <- site.coords[2]
    noaa.meta[i, "Elevation"] <- mean(as.numeric(site.dat$site[[1]]$geo$properties[,c("minElevationMeters", "maxElevationMeters")]))
    
    # Extract the species info where available
    if(length(site.dat$site[[1]]$paleoData[[1]]$species[[1]])>0){
      noaa.meta[i,"species.code"   ]  <- site.dat$site[[1]]$paleoData[[1]]$species[[1]]$speciesCode
      noaa.meta[i,"species.name"   ]  <- site.dat$site[[1]]$paleoData[[1]]$species[[1]]$scientificName
    }
    
    
  
    # Extract for the start/end dates
    noaa.meta[i,"yr.min"] <- as.numeric(site.dat$earliestYearCE)
    noaa.meta[i,"yr.max"] <- as.numeric(site.dat$mostRecentYearCE)
    
    # Going through the URLs to find a .rwl file
    site.dat <- site.dat$site[[1]]$paleoData[[1]]$dataFile[[1]]
    
    data.urls <- site.dat[site.dat$urlDescription %in% download.types,]
    
    # Make a note of how many files were downloaded
    noaa.meta[i,"files.download"] <- nrow(data.urls)
    
    if(nrow(data.urls)==0) next 
    for(j in 1:nrow(data.urls)){
      download.file(data.urls$fileUrl[j], file.path(dir.out, data.urls$linkText[j]), quiet=T)
    } # End looping thorugh URL types
    
  
  }
  # Get rid of the records we didn't actually pull anything for
  noaa.meta <- noaa.meta[!is.na(noaa.meta$files.download) & noaa.meta$files.download>0,]
  noaa.meta$species.code <- as.factor(noaa.meta$species.code)
  noaa.meta$species.name <- as.factor(noaa.meta$species.name)
  noaa.meta$studyCode    <- as.factor(noaa.meta$studyCode)
  noaa.meta$siteName     <- as.factor(noaa.meta$siteName)
  # summary(noaa.meta)
  
  return(noaa.meta)
}
