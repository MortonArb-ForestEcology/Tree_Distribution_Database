############ 2.
########### 2.23.18 Elizabeth Tokarz
##
## Plot Quercus points, just to visualize our data
##
##

# working directory cannot be changed on the server, so simply specify the route whenever uploading a CSV file
#setwd("C:/Users/Elizabeth/Desktop/2017_CTS_fellowship/FIA_unzipped_postgres_data/FIADB_PG/CSV_DATA")

fia_occ <- read.csv("lower_48_Quercus.csv")

# read in plot data, which includes location according to lat and lon
# be patient. it takes a while.
plot <- read.csv("PLOT.csv")


treeGA <- treeGA[, c("INVYR", "STATECD", "UNITCD", "COUNTYCD",
                     "PLOT", "SPCD", "STATUSCD")]



# Now both of the above hold more information than we need, so let's extract some data

# From 'treeXX' we want identifying codes INVYR, STATECD, UNITCD, COUNTYCD, PLOT, 
# Quercus alba (species code is 802)
#
alba <- treeALFL[which(treeALFL$SPCD==802), c("INVYR", "STATECD", "UNITCD", "COUNTYCD",
                                              "PLOT")]

# Next we will match up the location IDs and merge the 'treeXX' and 'plot' data frames 
alba_loc <- merge(alba, plot, by = c("INVYR", "STATECD", "UNITCD", 
                                     "COUNTYCD", "PLOT"), all = F)
# So we have a good amount of rows to work with, so let's remove unnecessary columns
# we can remove them by renaming our dataframe and extracting only the columns of interest
alba_loc <- alba_loc[, c("INVYR", "STATECD", "UNITCD", "COUNTYCD",
                         "PLOT", "LAT", "LON")]

# We just have to add in density here. First make a dataframe 
# with all unique plots for this state XX and number them with ID numbers.
u <- unique(alba_loc[,c('INVYR','STATECD','UNITCD', 'COUNTYCD', 'PLOT', 'LAT', 'LON')])
ID <- seq(from = 1, to = length(u$INVYR), by = 1)
u_plot <- data.frame(u, ID)

density_test <- merge(u_plot, alba, by = c("INVYR", "UNITCD", "COUNTYCD", "PLOT", "STATECD"), all = F)
t <- as.numeric(table(density_test$ID))
u_plot$density <- t

# Now here is the new part, bind the observations from 'u_plot_attach' to existing 'u_plot'

u_plot <- rbind(u_plot, u_plot_attach)
