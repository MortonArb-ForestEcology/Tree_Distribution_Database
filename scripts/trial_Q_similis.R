########### 3.1.18 Elizabeth Tokarz
##
## Plot Quercus similis points from FIA and GBIF, just to visualize our data
##
##

# working directory cannot be changed on the server, so simply specify the route whenever uploading a CSV file
#setwd("C:/Users/Elizabeth/Desktop/2017_CTS_fellowship/FIA_unzipped_postgres_data/FIADB_PG/CSV_DATA")

fia_occ <- read.csv("lower_48_Quercus.csv")

similis <- 836

# read in plot data, which includes location according to lat and lon
# be patient. it takes a while.
plot <- read.csv("PLOT.csv")

# Let's make a function to extract the coordinate data

prep_Q_occ <- function(d.f, sp) {
  # From 'treeXX' we want plot identifying codes INVYR, STATECD, UNITCD, COUNTYCD, PLOT, 
  Q_sp <- d.f[which(d.f$SPCD==sp), c("INVYR", "STATECD", "UNITCD", "COUNTYCD",
                                     "PLOT")]  
  # Next we will match up the location IDs and merge the 'treeXX' and 'plot' data frames 
  Q_sp <- merge(Q_sp, plot, by = c("INVYR", "STATECD", "UNITCD", 
                                   "COUNTYCD", "PLOT"), all = F)
  # let's remove unnecessary columns  
  Q_sp <- Q_sp[, c("INVYR", "STATECD", "UNITCD", "COUNTYCD",
                   "PLOT", "LAT", "LON")]
  
  # We just have to add in density here. First make a dataframe 
  # with all unique plots for this state XX and number them with ID numbers.
  u <- unique(Q_sp[,c('INVYR','STATECD','UNITCD', 'COUNTYCD', 'PLOT', 'LAT', 'LON')])
  ID <- seq(from = 1, to = length(u$INVYR), by = 1)
  u_plot <- data.frame(u, ID)
  
  density_test <- merge(u_plot, Q_sp, by = c("INVYR", "UNITCD", "COUNTYCD", "PLOT", "STATECD"), all = F)
  t <- as.numeric(table(density_test$ID))
  u_plot$density <- t
  print(u_plot)
}

Q_similis <- prep_Q_occ(fia_occ, similis)

# Now let's load in our GBIF data
gbif_sim <- read.csv(file='G:/My Drive/Distributions_TreeSpecies/in-use_occurrence_raw/trial_Q_similis.csv', header = T, as.is=T)
head(gbif)

# Let's map them!
library(maps)


png(filename = "Q_similis_FIA.png", width = 675, height = 406)
palette("default")
map("state")
points(Q_similis$LON, Q_similis$LAT, col = "blue", pch = 20)
points(gbif$long, gbif$ï..lat, col = "darkgreen", pch = 20)
legend("bottomleft", legend = c("FIA", "GBIF"), col = c("blue", "darkgreen"), pch =20)
title(main = "Quercus similis")
dev.off()

# and a southeast zoom-in again
png(filename = "Q_similis_FIA_SE.png", width = 675, height = 406)
palette("default")
map("state", c("Texas", "Oklahoma", "Arkansas", "Louisiana", "Mississippi", 
               "Alabama", "Georgia", "Tennessee", "Florida", "South Carolina",
               "North Carolina", "Kentucky", "Virginia", "Missouri"))
points(Q_similis$LON, Q_similis$LAT, col = "blue", pch = 20)
points(gbif$long, gbif$ï..lat, col = "green2", pch = 20)
legend("bottomleft", legend = c("FIA", "GBIF"), col = c("blue", "green2"), pch =20)
title(main = "Quercus similis_SE")
dev.off()
