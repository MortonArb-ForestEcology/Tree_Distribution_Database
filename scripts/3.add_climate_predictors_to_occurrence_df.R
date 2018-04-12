############ 3. add climate predictors to species data frames
########### 2.27.18 Elizabeth Tokarz
###########
###########


############### INPUT: occurrence_compiled_dec2_unique_countyDupRemoved.csv
#                       fia_absence_compiled_first12.csv
#                       fia_absence_compiled_thirteenth.csv
#               (from script 2.2_subset_occurrence_point_data.R)
#
#
#               Also: climate RasterLayers from PRISM database (.bil files)
####### Find PRISM data on ULMUS server in  /home/data/PRISM
###### # unit: mm
# 30-year normal annual precipitation in mm
#"PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil"
# 
# 30-year normal temperatures in degrees C
# mean "PRISM_tmean_30yr_normal_4kmM2_annual_bil.bil"
# maximum "PRISM_tmax_30yr_normal_4kmM2_annual_bil.bil"
# minimum "PRISM_tmin_30yr_normal_4kmM2_annual_bil.bil"
#
#####
##### use raster, rgdal and sp packages to read PRISM data
#
#
#     package: dplyr, raster, rgdal, sp
#
#     ############### OUTPUT: occurrence_compiled_dec2_unique_countyDupRemoved_wClimate.csv
#                     above file with extra columns holding climate and
#                     environmental predictor data
#
#

library(sp)
library(raster)
library(rgdal)
library(dplyr)
library(plyr)

# read in occurrence data
occur_all <- read.csv(file=paste0(compiled, '/occurrence_compiled_dec2_unique_countyDupRemoved.csv'), as.is=T)

# read in absence data
absent_12 <- read.csv(file=paste0(compiled, '/fia_absence_compiled_first12.csv'), as.is=T)
absent_13 <- read.csv(file=paste0(compiled, '/fia_absence_compiled_thirteenth.csv'), as.is=T)

# set wd for PRISM data
# setwd("C:/Users/Elizabeth/Desktop/2017_CTS_fellowship/PRISM")
# /home/data/PRISM/yearly_calculated/PRISM_ppt_2016.gri (example)... load each variable and each year?
# /home/data/PRISM/normal_4km/ppt/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil
# /home/data/PRISM/normal_4km/tmean/PRISM_tmean_30yr_normal_4kmM2_annual_bil.bil
# /home/data/PRISM/normal_4km/tmax/PRISM_tmax_30yr_normal_4kmM2_annual_bil.bil
# /home/data/PRISM/normal_4km/tmin/PRISM_tmin_30yr_normal_4kmM2_annual_bil.bil

# and extract the appropriate climate RasterLayers INSERT APPROPRIATE FILE ADDRESSES BELOW
annual_ppt <- raster("PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil")
annual_mean_temp <- raster("PRISM_tmean_30yr_normal_4kmM2_annual_bil.bil")
annual_max_temp <- raster("PRISM_tmax_30yr_normal_4kmM2_annual_bil.bil")
annual_min_temp <- raster("PRISM_tmin_30yr_normal_4kmM2_annual_bil.bil")


# in this function, extract all PRISM values that you want
extract_PRISM <- function(d.f){
  # make sure order is LON LAT
    d.f_coord <- d.f[, c("long_round", "lat_round")]  
    # extract precipitation data from PRISM
    d.f_ppt <- extract(annual_ppt, d.f_coord)
    # extract mean temperature data
    d.f_met <- extract(annual_mean_temp, d.f_coord)
    # extract max temp
    d.f_mat <- extract(annual_max_temp, d.f_coord)
    # extract min temp
    d.f_mit <- extract(annual_min_temp, d.f_coord)

    # and add all the new values to the end of the data frame    
    d.f_coord$annual_ppt <- d.f_ppt
    d.f_coord$mean_annual_temp <- d.f_met
    d.f_coord$max_annual_temp <- d.f_mat
    d.f_coord$min_annual_temp <- d.f_mit
    # now this is what you will save moving forward, just the numbers
    print(d.f_coord)
}

all_prism <- extract_PRISM(occur_all)
occur_all$annual_ppt <- all_prism$annual_ppt
occur_all$mean_annual_temp <- all_prism$mean_annual_temp
occur_all$max_annual_temp <- all_prism$max_annual_temp
occur_all$min_annual_temp <- all_prism$min_annual_temp

# Now write a csv for future use.
write.csv(occur_all, file = paste0(compiled, "/occur_prism.csv"))


# now repeat with the absences
absent_12$long_round <- round(absent_12$decimalLongitude, 3)
absent_12$lat_round <- round(absent_12$decimalLatitude, 3)
absent_12_prism <- extract_PRISM(absent_12)
absent_12$annual_ppt <- absent_12_prism$annual_ppt
absent_12$mean_annual_temp <- absent_12_prism$mean_annual_temp
absent_12$max_annual_temp <- absent_12_prism$max_annual_temp
absent_12$min_annual_temp <- absent_12_prism$min_annual_temp

absent_13$long_round <- round(absent_13$decimalLongitude, 3)
absent_13$lat_round <- round(absent_13$decimalLatitude, 3)
absent_13_prism <- extract_PRISM(absent_13)
absent_13$annual_ppt <- absent_13_prism$annual_ppt
absent_13$mean_annual_temp <- absent_13_prism$mean_annual_temp
absent_13$max_annual_temp <- absent_13_prism$max_annual_temp
absent_13$min_annual_temp <- absent_13_prism$min_annual_temp

# Write csvs for the absence data too
write.csv(absent_12, file = paste0(compiled, "/absent_12_prism.csv"))
write.csv(absent_13, file = paste0(compiled, "/absent_13_prism.csv"))
