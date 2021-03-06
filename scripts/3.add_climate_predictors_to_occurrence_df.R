############ 3. add climate predictors to species data frames
########### 2.27.18 Elizabeth Tokarz
###########
###########
############### INPUT: occurrence_compiled_dec2_unique_countyDupRemoved.csv
#                       fia_absence_compiled.csv
#               (from script 2.2_subset_occurrence_point_data.R)
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
#     ############### OUTPUT: occurrence_compiled_dec2_unique_countyDupRemoved_wClimate.csv
#                     above file with extra columns holding climate and
#                     environmental predictor data
#
#     package: dplyr, raster, rgdal, sp
#

library(sp)
library(raster)
library(rgdal)
library(dplyr)
library(plyr)

# read in occurrence data
occur_all <- read.csv(file=paste0(compiled, '/occurrence_compiled_dec2_unique_countyDupRemoved.csv'), as.is=T)

# read in absence data
absent <- read.csv(file=paste0(compiled, '/fia_absence_compiled.csv'), as.is=T)

# set wd for PRISM data if on computer
# setwd("C:/Users/Elizabeth/Desktop/2017_CTS_fellowship/PRISM")

# use these file locations for pulling PRISM from the server
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

    # and add all the new values to the end of the temporary data frame    
    d.f_coord$annual_ppt <- d.f_ppt
    d.f_coord$mean_annual_temp <- d.f_met
    d.f_coord$max_annual_temp <- d.f_mat
    d.f_coord$min_annual_temp <- d.f_mit
    # the output will have to be reassigned to the existing input data frame.
    print(d.f_coord)
}

# run the function once to make the temporary data frame
all_prism <- extract_PRISM(occur_all)
# then reassign the columns from the temporary data frame to the large input data frame
occur_all$annual_ppt <- all_prism$annual_ppt
occur_all$mean_annual_temp <- all_prism$mean_annual_temp
occur_all$max_annual_temp <- all_prism$max_annual_temp
occur_all$min_annual_temp <- all_prism$min_annual_temp

# Now write a csv for future use.
write.csv(occur_all, file = paste0(compiled, "/occur_prism.csv"))

# now repeat with the absences
# prepare the coordinates in the absence data frame by rounding the values.
absent$long_round <- round(absent$LON, 3)
absent$lat_round <- round(absent$LAT, 3)
# Then run the same function.
absent_prism <- extract_PRISM(absent)
# And reassign the values to new columns in the exisiting data frame as above.
absent$annual_ppt <- absent_prism$annual_ppt
absent$mean_annual_temp <- absent_prism$mean_annual_temp
absent$max_annual_temp <- absent_prism$max_annual_temp
absent$min_annual_temp <- absent_prism$min_annual_temp

# Write a csv for the absence data too
write.csv(absent, file = paste0(compiled, "/absent_prism.csv"))
