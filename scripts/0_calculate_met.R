# -----------------------------------------------------
# Calculting some yearly stats off of daily met values
# -----------------------------------------------------
# 
# --------------------------------
# What we want
# --------------------------------
# For all vars: mean, min, max, SD
# 1. Temperature:
#    1.a. Annual (deg. C)
#    1.b. Warmest Month/July (deg. C)
#    1.c. Coldest Month/January (deg. C)
#    1.d. duration of freeze-free period (# days)
# 2. Precipitation:
#    2.a. Annual (mm)
#    2.b. Warmest Month/July (mm)
#    2.c. Coldest Month/January (mm)
#    2.d. Number rainless days (# days)
#    2.e. Duration of longest rainless period (# days)
# --------------------------------
# -----------------------------------------------------

# -----------------------------------------------------
# Set file paths, load libraries
# -----------------------------------------------------
library(raster); 
#library(rgdal); library(rgeos)

path.met <- "/home/data/PRISM"
in.base <- file.path(path.met, "daily")
out.base <- file.path(path.met, "yearly_calculated")
if(!dir.exists(out.base)) dir.create(out.base, recursive = T)

# Set some raster Options to speed up calcs
rasterOptions(maxmemory = 2e10)
# -----------------------------------------------------


# -----------------------------------------------------
# getting the data
#    2.a. Annual (mm)
#    2.b. Warmest Month/July (mm)
#    2.c. Coldest Month/January (mm)
#    2.d. Number rainless days (# days)
#    2.e. Duration of longest rainless period (# days)
# -----------------------------------------------------
# precip first
# Function to calculate the duration of rainless days
dur.rainless <- function(x){
  dry <- NA # Duration of dry spells
  ddry <- NA # Middle day of dry period
  tally=0
  if(!is.na(x[1])){
    for(i in 1:length(x)){
      if(x[i]==0){
        tally=tally+1
      }
      # If we have rain and it resets our tally,
      #  - store tally in our vector; then reset
      if(x[i]>0 & tally>0){
        ddry <- c(ddry, i-tally/2)
        dry <- c(dry, tally)
        tally=0
      }
    }
  }
  
  mdry <- which(dry==max(dry, na.rm=T))[1]
  dry2 <- ifelse(all(is.na(dry)), NA, dry[mdry])
  ddry2 <- ifelse(all(is.na(ddry)), NA, ddry[mdry])
  # if(length(dry)==0){
  #   ddry <- NA
  #   dry  <- NA
  # } else {
  #   mdry <- which(dry==max(dry, na.rm=T))
  #   ddry <- ddry[mdry]
  #   dry  <- dry[mdry]
  # }
  out <- c(NA, NA)
  names(out) <- c("dry.date", "dry.dur")
  out[1] <- ddry2
  out[2] <- dry2
  return(out)
}

temp.rainless <- function(dry.date, dry.dur, temp){
  # Index the days we want for each cell
  ddry1 <- round(dry.date-dry.dur/2)+1
  ddry2 <- round(dry.date+dry.dur/2)-1

  # Create a blank raster
  temp2 <- ddry1
  temp2[] <- NA
  
  # Looping through each cell to do the temperature calculation
  # NOTE: THIS IS VERY SLOW!! but I coudln't figure out how to fix it
  pb <- txtProgressBar(min=0, max=dim(temp2)[1]*dim(temp2)[2], style=3)
  pb.ind <- 1
  for(i in 1:dim(temp2)[1]){
    for(j in 1:dim(temp2)[2]){
      setTxtProgressBar(pb, pb.ind); pb.ind <- pb.ind+1
      if(is.na(dry.date[i,j]) | (abs(dry.date[i,j])==Inf)) next
      
      temp.vec <- as.vector(temp[i,j,])
      
      temp2[i,j] <- mean(temp.vec[ddry1[i,j]:ddry2[i,j]], na.rm=T)
    }
  }
  return(temp2)
}

dur.warm <- function(x, thresh=0){
  warm <- NA # Duration of warm spells
  tally=0
  if(!is.na(x[1])){
    for(i in 1:length(x)){
      if(x[i]>thresh){
        tally=tally+1
      }
      # If we have below-freezing temps, it resets our tally
      #  - store tally in our vector; then reset
      if(x[i]<=thresh & tally>0){
        warm <- c(warm, tally)
        tally=0
      }
    }
  }
  
  mwarm <- which(warm==max(warm, na.rm=T))[1]
  warm2 <- ifelse(all(is.na(warm)), NA, warm[mwarm])
  
  return(warm2)
}

# Setting up the file paths and doing a loop
path.ppt <- file.path(in.base, "ppt")
path.tmax <- file.path(in.base, "tmax")
path.tmin <- file.path(in.base, "tmin")
yrs.ppt <- dir(path.ppt)
pb <- txtProgressBar(min=min(as.numeric(yrs.ppt)), max=max(as.numeric(yrs.ppt)), style = 3)
for(yr in yrs.ppt){
  setTxtProgressBar(pb, as.numeric(yr))

  # setwd(path.ppt)
  files.ppt <- dir(file.path(path.ppt, yr), "bil.bil")
  f.excl <- files.ppt[grep(".xml", files.ppt)]
  files.ppt <- files.ppt[!files.ppt %in% f.excl]

  files.tmax <- dir(file.path(path.tmax, yr), "bil.bil")
  f.excl <- files.tmax[grep(".xml", files.tmax)]
  files.tmax <- files.tmax[!files.tmax %in% f.excl]

  files.tmin <- dir(file.path(path.tmin, yr), "bil.bil")
  f.excl <- files.tmin[grep(".xml", files.tmin)]
  files.tmin <- files.tmin[!files.tmin %in% f.excl]
  
  ppt.all <- stack(file.path(path.ppt, yr, files.ppt))
  tmax.all <- stack(file.path(path.tmax, yr, files.tmax))
  tmin.all <- stack(file.path(path.tmin, yr, files.tmin))
  projection(ppt.all) <- projection(tmin.all) <- projection(tmax.all) <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  # Note: converting this to a brick holds it all in memorym, but is necessary for parallel processing
  # met.all <- brick(stack(file.path(path.ppt, yr, days.yr))) 
  
  met.summary <- stack(calc(ppt.all, sum))
  names(met.summary) <- "ppt.ann"
  met.summary[["ppt.SD"      ]] <- calc(ppt.all, sd)
  met.summary[["ppt.jan"     ]] <- calc(ppt.all[[1:31]], sum)
  met.summary[["ppt.jul"     ]] <- calc(ppt.all[[182:212]], sum) # might be off by a day in leap years, but it'll be okay
  
  met.summary[["tmin.ann"    ]] <- calc(tmin.all, mean)
  met.summary[["tmin.SD"     ]] <- calc(tmin.all, sd)
  met.summary[["tmin.jan"    ]] <- calc(tmin.all[[1:31]], sum)
  met.summary[["tmin.jul"    ]] <- calc(tmin.all[[182:212]], sum) # might be off by a day in leap years, but it'll be okay
  
  met.summary[["tmax.ann"    ]] <- calc(tmax.all, mean)
  met.summary[["tmax.SD"     ]] <- calc(tmax.all, sd)
  met.summary[["tmax.jan"    ]] <- calc(tmax.all[[1:31]], sum)
  met.summary[["tmax.jul"    ]] <- calc(tmax.all[[182:212]], sum) # might be off by a day in leap years, but it'll be okay

  # Doing Drought stats
  # Couldn't figure out how to add multiple layers at once, so doing a cheap work-around
  met.summary[["rainless.n"  ]] <- calc(ppt.all, function(x) length(x[x>0]))
  
  # ppt.test <- crop(ppt.all, extent(-90.5, -90, 37, 37.5))
  # ppt.stats <- calc(ppt.test, dur.rainless, forceapply=T)
  
  ppt.stats <- calc(ppt.all, dur.rainless)
  met.summary[["dry.date"]] <- ppt.stats[[1]]
  met.summary[["dry.dur" ]] <- ppt.stats[[2]]
  rm(ppt.stats)
  
  # Get the max temperature during the rainless period
  # NOTE: This will be VERY slow because I couldn't figure out how to skip a loop
  # tmax.test <- crop(tmax.all, extent(-90.5, -90, 37, 37.5))
  # temp.test <- temp.rainless(ppt.stats[[1]], met.summary[[2]], tmax.test)
  met.summary[["tmax.drought"]] <- temp.rainless(met.summary$dry.date, met.summary$dry.dur, tmax.all)

  # Using our drought function to also get the duration of the freeze-free period
  warm.rast <- calc(tmin.all, dur.warm)
  met.summary[["warm.dur"]] <- warm.rast
  rm(warm.rast)
  
  writeRaster(met.summary, filename=file.path(out.base, paste0("PRISM_Yr_Stats_", yr)))
  
  rm(met.summary)
}

# -----------------------------------------------------
