########### 3.28.18 Elizabeth Tokarz
##
## Plot Quercus lobata points from 3 different GBIF datasets
##  1) pre-uploading into GeoRef
##  2) post-exporting from GeoRef
##  3) post-revisions

# make sure that the final version of the cleaned data looks good when mapped, when compared to the other sets

library(dplyr)

# for windows
# geo_loc <- read.csv(file='G:/My Drive/Distributions_TreeSpecies/in-use_occurrence_raw/gbif_DC_georef.csv', as.is=T)
# post_geo <- read.csv(file='G:/My Drive/Distributions_TreeSpecies/in-use_occurrence_raw/gbif_DC_post-georef.csv', as.is=T)
# revised_geo <- read.csv(file='G:/My Drive/Distributions_TreeSpecies/in-use_occurrence_raw/gbif_DC_post-georef_revised.csv', as.is=T)


# now pull out the lobata occurrences and remove duplicates from "geo_loc" and "post_geo", for comparison sake
# duplicates have already been removed from "revised_geo "
# speciesKey for Quercus lobata is 2878373

  a <- which(geo_loc$speciesKey == 2878373) 
  geo_loc <- geo_loc[a, ]
  geo_loc$lat <- as.numeric(as.character(geo_loc$latitude))
  geo_loc$lon <- as.numeric(as.character(geo_loc$longitude))
  geo_loc <- filter(geo_loc, lat > 0, lon < 0)
  geo_loc <- geo_loc[!duplicated(round(geo_loc[,c("lat","lon")], 2)), ]
  
  b <- which(post_geo$speciesKey == 2878373)
  post_geo <- post_geo[b, ]
  post_geo$lat <- as.numeric(as.character(post_geo$latitude))
  post_geo$lon <- as.numeric(as.character(post_geo$longitude))
  post_geo <- filter(post_geo, lat > 0, lon < 0)
  post_geo <- post_geo[!duplicated(round(post_geo[,c("lat","lon")], 2)), ]
  
  h <- which(revised_geo$speciesKey == 2878373)
  revised_geo <- revised_geo[h, ]

# Let's map them!
library(maps)


#png(filename = "Q_lobata_FIA.png", width = 675, height = 406)
#palette("default")
#map("state")
#points(Q_similis$LON, Q_similis$LAT, col = "blue", pch = 20)
#points(gbif$long, gbif$ï..lat, col = "darkgreen", pch = 20)
#legend("bottomleft", legend = c("FIA", "GBIF"), col = c("blue", "darkgreen"), pch =20)
#title(main = "Quercus similis")
#dev.off()

# a California zoom-in
palette("default")
par(mfrow=c(3,1))

par(mfrow=c(1,1))
map("state", c("California"))
points(geo_loc$longitude, geo_loc$latitude, col = "blue", pch = 20)
title(main = "Quercus lobata pre-GL")

map("state", c("California"))
points(post_geo$longitude, post_geo$latitude, col = "green2", pch = 20)
title(main = "Quercus lobata post-GL")

map("state", c("California"))
points(revised_geo$longitude, revised_geo$latitude, col = "indianred3", pch = 20)
title(main = "Quercus lobata revised-GL")

# The revised data looks to be an average of the pre and post geolocation process
# Using the revised coordinates seems to make the most sense.


