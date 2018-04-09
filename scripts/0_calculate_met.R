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
  dry <- NA
  tally=0
  if(!is.na(x[1])){
    for(i in 1:length(x)){
      if(x[i]==0){
        tally=tally+1
      }
      # If we have rain and it resets our tally,
      #  - store tally in our vector; then reset
      if(x[i]>0 & tally>0){
        dry <- c(dry, tally)
        tally=0
      }
    }
  }
  dry <- ifelse(all(is.na(dry)), NA, max(dry, na.rm=T))
  return(dry)
}

# Setting up the file paths and doing a loop
path.ppt <- file.path(in.base, "ppt")
yrs.ppt <- dir(path.ppt)
for(yr in yrs.ppt){
  setwd(file.path)
  days.yr <- dir(file.path(path.ppt, yr), "bil.bil")
  f.excl <- days.yr[grep(".xml", days.yr)]
  days.yr <- days.yr[!days.yr %in% f.excl]
  
  met.all <- stack(file.path(path.ppt, yr, days.yr))
  
  met.summary <- stack(calc(met.all, mean))
  names(met.summary) <- "mean"
  met.summary[["sd"]] <- calc(met.all, sd)
  met.summary[["rainless.n"]]   <- calc(met.all, function(x) length(x[x>0]))
  met.summary[["rainless.dur"]] <- calc(met.all, dur.rainless)
  
}

# -----------------------------------------------------
