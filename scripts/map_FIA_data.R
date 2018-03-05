############ 2.
########### 2.23.18 Elizabeth Tokarz
##
## Plot Quercus points, just to visualize our data
##
##

# working directory cannot be changed on the server, so simply specify the route whenever uploading a CSV file
#setwd("C:/Users/Elizabeth/Desktop/2017_CTS_fellowship/FIA_unzipped_postgres_data/FIADB_PG/CSV_DATA")

fia_occ <- read.csv("lower_48_Quercus.csv")

# Let's explore the fia data somewhat
# Which species are represented here
unique(fia_occ$SPCD)
# Only six of our rare oaks.
# let's assign the species names to the numbers
engelmannii <- 811
graciliformis <- 851
laceyi <- 8514
lobata <- 821
oglethorpensis <- 844
similis <- 836

# read in plot data, which includes location according to lat and lon
# be patient. it takes a while.
plot <- read.csv("PLOT.csv")


### BY SPECIES
# Now we will extract the locations for the species one by one and then map them. 
# Let's make a function to make it faster.

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

Q_engelmannii <- prep_Q_occ(fia_occ, engelmannii)  
  
Q_graciliformis <- prep_Q_occ(fia_occ, graciliformis)

Q_laceyi <- prep_Q_occ(fia_occ, laceyi)

Q_lobata <- prep_Q_occ(fia_occ, lobata)

Q_oglethorpensis <- prep_Q_occ(fia_occ, oglethorpensis)

Q_similis <- prep_Q_occ(fia_occ, similis)


### ALL AT ONCE
# **We will try this a new way to enhance the existing FIA data frame with coordinates only and some plot data
# don't need a function if we only do this once

  # let's remove unnecessary columns from the plot csv
  plot2 <- plot[, c("INVYR", "STATECD", "UNITCD", "COUNTYCD",
                   "PLOT", "LAT", "LON")]
  # Next we will match up the location IDs and merge the 'treeXX' and 'plot' data frames 
  fia_coord <- merge(fia_occ, plot2, by = c("INVYR", "STATECD", "UNITCD", 
                                   "COUNTYCD", "PLOT"), all = F)
  
  # We just have to add in density here. First make a dataframe 
  # with all unique plots for this state XX and number them with ID numbers.
  u <- unique(fia_coord[,c('SPCD', 'INVYR','STATECD','UNITCD', 'COUNTYCD', 'PLOT', 'LAT', 'LON')])
  ID <- seq(from = 1, to = length(u$INVYR), by = 1)
  u_plot <- data.frame(u, ID)
  
  density_test <- merge(u_plot, fia_coord, by = c("SPCD", "INVYR", "UNITCD", "COUNTYCD", "PLOT", "STATECD"), all = T)
  t <- as.numeric(table(density_test$ID))
  u_plot$density <- t
  print(u_plot)

  # manipulate u_plot further to add onto raw data block

# Let's map them!
library(maps)

# for the continuous color palettes, we will need a special legend
legend.col <- function(col, lev){
  
  opar <- par
  
  n <- length(col)
  
  bx <- par("usr")
  
  box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000,
              bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 50)
  box.cy <- c(bx[3], bx[3])
  box.sy <- (bx[4] - bx[3]) / n
  
  xx <- rep(box.cx, each = 2)
  
  par(xpd = TRUE)
  for(i in 1:n){
    
    yy <- c(box.cy[1] + (box.sy * (i - 1)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i - 1)))
    polygon(xx, yy, col = col[i], border = col[i])
    
  }
  par(new = TRUE)
  plot(0, 0, type = "n",
       ylim = c(min(lev), max(lev)),
       yaxt = "n", ylab = "",
       xaxt = "n", xlab = "",
       frame.plot = FALSE)
  axis(side = 4, las = 2, tick = FALSE, line = .25)
  par <- opar
}

# Mapping can be a function too...

#map_rare_Q <- function(sp){
#  file <- paste0("Q_", sp, "_FIA.png")
#  png(filename = file, width = 675, height = 406)
#  palette("default")
#  map("state")
#  palette(rainbow(n = length(unique(sp$density))))
#  colr <- rainbow(n = length(unique(sp$density)))
#  points(sp$LON, sp$LAT, col = sp$density, pch = 20)
#  legend.col(col = colr, lev = sp$density)
#  title(main = paste0(sp, "FIA"))
#  dev.off()
#  }
#
#map_rare_Q(Q_laceyi)
# Not sure why it isn't working...

# Do it the long way for now.
# save this as a png
png(filename = "Q_engelmannii_FIA.png", width = 675, height = 406)
# make the outline of the US
palette("default")
map("state")
# change the palette to a continuous scale that traverses the length of plot density
palette(rainbow(n = length(unique(Q_engelmannii$density))))
# add this for use in our legend
colr <- rainbow(n = length(unique(Q_engelmannii$density)))
# plot the points on the map using this new palette
points(Q_engelmannii$LON, Q_engelmannii$LAT, col = Q_engelmannii$density, pch = 20)
# and make the legend
legend.col(col = colr, lev = Q_engelmannii$density)
#add a title before saving
title(main = "Quercus engelmannii FIA")
dev.off()

# Now do one where we zoom in on California
png(filename = "Q_engelmannii_FIA_CA.png", width = 475, height = 406)
palette("default")
map("state", "California")
#map("state", "California", col = "white", bg = "black")
palette(rainbow(n = length(unique(Q_engelmannii$density))))
colr <- rainbow(n = length(unique(Q_engelmannii$density)))
points(Q_engelmannii$LON, Q_engelmannii$LAT, col = Q_engelmannii$density, pch = 20)
legend.col(col = colr, lev = Q_engelmannii$density)
title(main = "Quercus engelmannii FIA_CA")
#title(main = "Quercus engelmannii FIA_CA", col = "white")
dev.off()


png(filename = "Q_graciliformis_FIA.png", width = 675, height = 406)
palette("default")
map("state")
points(Q_graciliformis$LON, Q_graciliformis$LAT, pch = 20)
legend.col(col = colr, lev = Q_graciliformis$density)
title(main = "Quercus graciliformis FIA, density 13")
dev.off()


png(filename = "Q_laceyi_FIA.png", width = 675, height = 406)
palette("default")
map("state")
palette(rainbow(n = length(unique(Q_laceyi$density))))
colr <- rainbow(n = length(unique(Q_laceyi$density)))
points(Q_laceyi$LON, Q_laceyi$LAT, col = Q_laceyi$density, pch = 20)
legend.col(col = colr, lev = Q_laceyi$density)
title(main = "Quercus laceyi FIA")
dev.off()

# now one zoomed in on Texas
png(filename = "Q_laceyi_FIA_TX.png", width = 575, height = 406)
palette("default")
map("state", "Texas")
palette(rainbow(n = length(unique(Q_laceyi$density))))
colr <- rainbow(n = length(unique(Q_laceyi$density)))
points(Q_laceyi$LON, Q_laceyi$LAT, col = Q_laceyi$density, pch = 20)
legend.col(col = colr, lev = Q_laceyi$density)
title(main = "Quercus laceyi_TX FIA")
dev.off()


png(filename = "Q_lobata_FIA.png", width = 675, height = 406)
palette("default")
map("state")
palette(rainbow(n = length(unique(Q_lobata$density))))
colr <- rainbow(n = length(unique(Q_lobata$density)))
points(Q_lobata$LON, Q_lobata$LAT, col = Q_lobata$density, pch = 20)
legend.col(col = colr, lev = Q_lobata$density)
title(main = "Quercus lobata FIA")
dev.off()

# Now a west coast zoom-in
png(filename = "Q_lobata_FIA_CA.png", width = 475, height = 606)
palette("default")
map("state", c("California", "Oregon"))
palette(rainbow(n = length(unique(Q_lobata$density))))
colr <- rainbow(n = length(unique(Q_lobata$density)))
points(Q_lobata$LON, Q_lobata$LAT, col = Q_lobata$density, pch = 20)
legend.col(col = colr, lev = Q_lobata$density)
title(main = "Quercus lobata_CA FIA")
dev.off()


png(filename = "Q_oglethorpensis_FIA.png", width = 675, height = 406)
palette("default")
map("state")
palette(rainbow(n = length(unique(Q_oglethorpensis$density))))
colr <- rainbow(n = length(unique(Q_oglethorpensis$density)))
points(Q_oglethorpensis$LON, Q_oglethorpensis$LAT, col = Q_oglethorpensis$density, pch = 20)
legend.col(col = colr, lev = Q_oglethorpensis$density)
title(main = "Quercus oglethorpensis FIA")
dev.off()

#now zoom in on southeast
png(filename = "Q_oglethorpensis_FIA_SE.png", width = 675, height = 406)
palette("default")
map("state", c("South Carolina", "Georgia", "Alabama"))
palette(rainbow(n = length(unique(Q_oglethorpensis$density))))
colr <- rainbow(n = length(unique(Q_oglethorpensis$density)))
points(Q_oglethorpensis$LON, Q_oglethorpensis$LAT, col = Q_oglethorpensis$density, pch = 20)
legend.col(col = colr, lev = Q_oglethorpensis$density)
title(main = "Quercus oglethorpensis_SE FIA")
dev.off()


png(filename = "Q_similis_FIA.png", width = 675, height = 406)
palette("default")
map("state")
palette(rainbow(n = length(unique(Q_similis$density))))
colr <- rainbow(n = length(unique(Q_similis$density)))
points(Q_similis$LON, Q_similis$LAT, col = Q_similis$density, pch = 20)
legend.col(col = colr, lev = Q_similis$density)
title(main = "Quercus similis FIA")
dev.off()

# and a southeast zoom-in again
png(filename = "Q_similis_FIA_SE.png", width = 675, height = 406)
palette("default")
map("state", c("Texas", "Oklahoma", "Arkansas", "Louisiana", "Mississippi", 
               "Alabama", "Georgia", "Tennessee", "Florida", "South Carolina",
               "North Carolina", "Kentucky", "Virginia", "Missouri"))
palette(rainbow(n = length(unique(Q_similis$density))))
colr <- rainbow(n = length(unique(Q_similis$density)))
points(Q_similis$LON, Q_similis$LAT, col = Q_similis$density, pch = 20)
legend.col(col = colr, lev = Q_similis$density)
title(main = "Quercus similis_SE FIA")
dev.off()
