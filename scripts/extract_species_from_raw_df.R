############ 3. extract species from raw data frame
########### 2.27.18 Elizabeth Tokarz
###########
###########
###########
###########


############### INPUT: raw data csv (a large file holding all the data we could
#               possibly need for our 28 species; including location, source)
#
#     package: dplyr
#
#     ############### OUTPUT: edited species-specific csv at three levels of certainty 
#                     (a smaller file containing all occurrence data necessary 
#                     for our model, having removed unreasonable occurrence 
#                     outliers)
#
#                     1. County-level
#                     2. All given and localized coordinates, including less reliable sources
#                     3. Given and localized coordinates from reliable sources


# setwd("C:/Users/Elizabeth/Desktop/2017_CTS_fellowship/compilation_eb")
# raw <- read.csv("raw_data_temp_0218.csv")

raw <- read.csv("../data/raw_data_temp_0218.csv")

library(dplyr)

# First we will sort through the data at the county level.
county <- filter(raw, gps_determ=="C")




# Second we will remove all the county level data.
local <- filter(raw, gps_determ!="C")
