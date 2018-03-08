############ 4. add climate predictors to species data frames
########### 2.27.18 Elizabeth Tokarz
###########
###########


############### INPUT: species data csv in any of the three levels of certainty
#               (from script #3)
#
#               first three letters of species epithet + number representing level 
#               of certainty + _coord.csv
#               ex. Quercus arkansana at county level: ark1_coord.csv 
#                 Quercus arkansana at localized level: ark2_coord.csv
#                 Quercus arkansana at edited localized level: ark3_coord.csv
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
#     ############### OUTPUT: above files with extra columns holding climate and
#                     environmental predictor data
#
#                     ex. ark1_cp.csv
#                         ark2_cp.csv
#                         ark3_cp.csv

library(sp)
library(raster)
library(rgdal)
library(dplyr)

# setwd("C:/Users/Elizabeth/Desktop/2017_CTS_fellowship/compilation_eb")
# decide which species data you would like to work with
#ark1 <- read.csv("ark1_coord.csv")
#ino1 <- read.csv("ino1_coord.csv")
#ark2 <- read.csv("ark2_coord.csv")
#ino2 <- read.csv("ino2_coord.csv")
#ark3 <- 
#ino3 <- 

# set wd for PRISM data
# setwd("C:/Users/Elizabeth/Desktop/2017_CTS_fellowship/PRISM")

# and extract the appropriate climate RasterLayers
annual_ppt <- raster("PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil")
annual_mean_temp <- raster("PRISM_tmean_30yr_normal_4kmM2_annual_bil.bil")
annual_max_temp <- raster("PRISM_tmax_30yr_normal_4kmM2_annual_bil.bil")
annual_min_temp <- raster("PRISM_tmin_30yr_normal_4kmM2_annual_bil.bil")


# in this function, extract all PRISM values that you want
extract_PRISM <- function(d.f){
  # make sure order is LON LAT
    d.f_coord <- d.f[, c("long", "lat")]  
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

ark2_prism <- extract_PRISM(ark2)

# Now write a csv for future use.
write.csv(ark2_prism, file = "ark2_prism.csv")

