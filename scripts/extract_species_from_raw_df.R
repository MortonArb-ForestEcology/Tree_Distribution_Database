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
#raw <- read.csv("gbif_arkansana_inopina.csv")

library(dplyr)

# First we will sort through the data at the county level.
county <- filter(raw, gps_determ=="C")

#group_by(county, species_no)
# Now we will use county to get the county occurrences for each species

# First filter the county occurrences only
ark1 <- filter(county, species_no==3)
ino1 <- filter(county, species_no==15)

# Then use this function to compile a list of unique counties and the coordinates of each
extract_county_one  <- function(d.f){
    d.f_coord <- data.frame()
    sta <- unique(d.f$state)
    
    for (st in (1:length(sta))){
      cou <- unique(ark1[ark1$state==sta[st],]$county)
      
      for (co in (1:length(cou))){
      vec <- which(ark1$state==sta[st] & ark1$county==cou[co])
      
      d.f_coord <- rbind(d.f_coord, ark1[vec[1],c("state", "county", "lat", "long")])
    
      }
    }
    print(d.f_coord)
  }

ark1_coord <- extract_county_one(ark1)
write.csv(ark1_coord, "ark1_coord.csv")

ino1_coord <- extract_county_one(ino1)
write.csv(ino1_coord, "ino1_coord.csv")

#################################################################################
# Second we keep only the given and localized coordinates.
#################################################################################

# First filter the localized occurrences only
local <- filter(raw, gps_determ=="L"|gps_determ=="G")



ark2 <- filter(local, species_no==3)

ark2[1:5,1]

# remove unecessary columns
ark2 <- select(ark2, ï..dataset, species, state, county, lat, long, basis, year, locality,
               source, gps_determ, uncertainty..m., issue)
# filter out non-numeric coordinates
ark2$lat <- as.numeric(as.character(ark2$lat))
ark2$long <- as.numeric(as.character(ark2$long))
ark2 <- filter(ark2, lat > 0 & long < 0)
# check for duplicates to 2 decimal places
ark2 <- ark2[!duplicated(round(ark2[,c("lat","long")], 2)), ]
# now we are ready to write the csvs
ark2w <- select(ark2, state, county, lat, long, gps_determ)

write.csv(ark2w, "ark2_coord.csv")

summary(ark2)
coord <- paste(ark2$lat, ark2$long)
unique(coord)
# After filtering the dataset by species and coordinates, we can remove duplicate 
# or overlapping coordinates

summary(ark2$lat)




# unecessarycolumns and write the csv



head(ark2)






