
################
### LIBRARIES and FUNCTIONS
################

library(dplyr)
library(rgbif)
library(ridigbio)
library(data.table)
library(tidyr)
library(lubridate)

## Subset data and (optionally) write a CSV
gen_subset <- function(orig_data, action, export_name){
  selected_rows <- (action)
  new <- orig_data[selected_rows,]
  if (missing(export_name)){
    return(data.frame(new))
    } else {
      write.csv(new, file = export_name)
      return(data.frame(new))
    }
} 

################
### 1. CREATE UNIFIED DATAFRAME OF ALL OCCURRENCE RECORDS FOR TARGET SPECIES
################

## a. Read in list of target species
sp_list <- as.character(as.list(read.csv(file='./Google Drive/Distributions_TreeSpecies/target_species_list.csv'))$species)
# for windows?
# sp_list <- as.character(as.list(read.csv(file='G:/My Drive/Distributions_TreeSpecies/target_species_list.csv'))$species)


## b. Read in standardized occurrence point datasets (exactly the same column headers in each file)
file_list <- list.files(path = "./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/standard_col", pattern = ".csv", full.names = T)
# for windows?
# file_list <- list.files(path = "G:/My Drive/Distributions_TreeSpecies/in-use_occurrence_raw/standard_col", pattern = ".csv", full.names = T)
file_dfs <- lapply(file_list, read.csv, header = TRUE, fileEncoding="latin1", strip.white = TRUE, colClasses = "character")
  length(file_dfs)

## c. Stack standardized datasets to create one dataframe
df <- data.frame()
for(file in seq_along(file_dfs)){
  df <- rbind(df, file_dfs[[file]])
}
  str(df); nrow(df) #94426 #93192 ELT
df$species_no <- 0

## d. Read in raw occurrence point datasets, standardize column names, and create one dataframe
gbif <- read.csv(file='./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/gbif_raw.csv', as.is=T)
# for windows? but too big to not use server # more columns than column names
# gbif <- read.csv(file='G:/My Drive/Distributions_TreeSpecies/in-use_occurrence_raw/gbif_raw_DarwinCore_edit.csv', as.is=T)
    nrow(gbif) #12195
    # remove extraneous columns
    gbif <- subset(gbif, select = c(basisOfRecord, institutionCode, genus, 
                                      scientificName, species, year, countryCode, 
                                      stateProvince, county, municipality, 
                                      locality, verbatimLocality, occurrenceRemarks, 
                                      associatedTaxa, decimalLatitude, decimalLongitude,
                                      coordinateUncertaintyInMeters,
                                      georeferenceSources, issue))

  setnames(gbif,
    old=c("decimalLatitude","decimalLongitude","basisOfRecord","institutionCode","coordinateUncertaintyInMeters", "countryCode", "stateProvince", "scientificName"),
    new=c("lat","long","basis","source","uncert_m", "country", "state", "synonym"))
  # fix locality data as much as possible  
  sum(is.na(gbif$locality)) #3044
    # when there is no locality information other than that of the verbatim locality column, copy that information to locality
    gbif$locality[is.na(gbif$locality)]  <- gbif$verbatimLocality[is.na(gbif$locality)]
    sum(is.na(gbif$locality)) #1835, better
    # for columns with NA in state, look in locality data for a state abbreviation or name and copy it into the empty column
    sum(is.na(gbif$state)) # 2030
    head(gbif$locality[is.na(gbif$state)])
    # write a function to take the state from the locality
    extract_state <- function(df, loc, rep){
      df$state[grep(pattern = loc, df$locality[is.na(df$state)])] <- rep
    }
    # not sure why the above didn't work...
    gbif2 <- gbif
    
    
    gbif_s_na <- which(is.na(gbif2$state))
    rows <- grep(pattern = "CA", x = gbif2$locality[gbif_s_na]) # These are all the rows we want to target
    # these row numbers refer to the spacing in vector gbif_s_na
    gbif_s_na[rows]
    
    [grep(pattern = "CA", x = gbif2$locality[is.na(gbif2$state)])] <- "California"
    is.na(gbif2$state)[rows] <- "California"
    
    
    gbif2[rows, "state"]
    gbif2[c(20:25), "state"]
    
    
    which(gbif2[is.na(gbif2$state), c("locality", "state")])
    
    <- "California"
    
    # See how this pattern changes. Why?
    sum(is.na(gbif2$state)) # 2030
    grep(pattern = "CA", x = gbif2$locality[is.na(gbif2$state)]) # should replace 666
    gbif2$state[grep(pattern = "CA", x = gbif2$locality[is.na(gbif2$state)])] <- "California"
    gbif2[!is.na(gbif2$state),]
    gbif2[!is.na(gbif2$state), c(8, 11)]
    
    sum(is.na(gbif2$state)) # 1785  
    gbif2$state[grep(pattern = "CA", x = gbif2$locality[is.na(gbif2$state)])] <- "California"
    sum(is.na(gbif2$state)) # 1717  
    gbif2$state[grep(pattern = "CA", x = gbif2$locality[is.na(gbif2$state)])] <- "California"
    sum(is.na(gbif2$state)) # 1700
    gbif2$state[grep(pattern = "CA", x = gbif2$locality[is.na(gbif2$state)])] <- "California"
    sum(is.na(gbif2$state)) # 1692
    gbif2$state[grep(pattern = "CA", x = gbif2$locality[is.na(gbif2$state)])] <- "California"
    sum(is.na(gbif2$state)) # 1692  
    # Doesn't seem to catch all CAs.. does the row number differ after being rewritten?
    #Does the first round catch only appropriate CAs?
    
    gbif2 <- gbif
    # See how this pattern changes. Why?
    sum(is.na(gbif2$state)) # 2030
    grep(pattern = "TX", x = gbif2$locality[is.na(gbif2$state)]) # should replace 14
    gbif2$state[grep(pattern = "TX", x = gbif2$locality[is.na(gbif2$state)])] <- "Texas"
    sum(is.na(gbif2$state)) # 2026
    # what happens to entry 5?
    gbif2[5, ] # It looks good
    # what happens to entry 36?
    gbif[36, ] # It's still NA. Why?
    grep(pattern = "TX", x = gbif2$locality[is.na(gbif2$state)]) # 13 remain
    # Can grep only make one change at a time?
    gbif2$state[grep(pattern = "TX", x = gbif2$locality[is.na(gbif2$state)])] <- "Texas"
    grep(pattern = "TX", x = gbif2$locality[is.na(gbif2$state)]) # now 13 still remain, but their row numbers have shifted...
    gbif2$state[grep(pattern = "TX", x = gbif2$locality[is.na(gbif2$state)])] <- "Texas"
    grep(pattern = "TX", x = gbif2$locality[is.na(gbif2$state)])
    gbif2$state[grep(pattern = "TX", x = gbif2$locality[is.na(gbif2$state)])] <- "Texas"
    grep(pattern = "TX", x = gbif2$locality[is.na(gbif2$state)])
    gbif2$state[grep(pattern = "TX", x = gbif2$locality[is.na(gbif2$state)])] <- "Texas"
    grep(pattern = "TX", x = gbif2$locality[is.na(gbif2$state)])
    
    
    
    gbif2$state[grep(pattern = "CA", x = gbif2$locality[is.na(gbif2$state)])] <- "California"
    gbif2$state[grep(pattern = "California", x = gbif2$locality[is.na(gbif2$state)])] <- "California"
    # When the above runs several times, it catches more empty spaces...why?
    # this doesn't completely work either
    
    extract_state(California, California)
    extract_state(TX, Texas)
    extract_state(Texas, Texas)
    extract_state(FL, Florida)
    extract_state(Florida, Florida)
    extract_state(AZ, Arizona)
    extract_state(UT, Utah)
    extract_state(Utah, Utah)
    extract_state(AL, Alabama)
    extract_state(Alamaba, Alabama)
    extract_state(Arkansas, Arkansas)
    extract_state(Georgia, Georgia)
    extract_state(LA, Louisiana)
    
    # write a csv to upload into georeference
    write.csv(gbif, file='G:/My Drive/Distributions_TreeSpecies/in-use_occurrence_raw/gbif_DC_georef.csv')
    
    table(gbif$state)
    gbif$dataset <- "gbif"
    
consortium <- read.csv(file='./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/consortium_raw.csv', as.is=T)
    # for windows
    # consortium <- read.csv(file='G:/My Drive/Distributions_TreeSpecies/in-use_occurrence_raw/consortium_raw.csv', as.is=T)
    nrow(consortium) #98500
  setnames(consortium,
    old=c("scientificName","decimalLatitude","decimalLongitude","basisOfRecord","habitat","associatedTaxa","institutionCode","coordinateUncertaintyInMeters","stateProvince"),
    new=c("species","lat","long","basis","locality1","locality2","source","uncert_m","state")) ### various locality info needs to be concatenated
    consortium$dataset <- "consortium"
    consortium$county <- NA
idigbio <- read.csv(file='./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/idigbio_raw.csv', as.is=T)
# for windows?
# idigbio <- read.csv(file='G:/My Drive/Distributions_TreeSpecies/in-use_occurrence_raw/idigbio_raw.csv', as.is=T)
    nrow(idigbio) #196485
  idigbio <- setnames(idigbio,
    old=c("dwc.genus","dwc.scientificName","idigbio.geoPoint","dwc.basisOfRecord","idigbio.eventDate","dwc.locality","dwc.institutionCode","dwc.coordinateUncertaintyInMeters","dwc.stateProvince","dwc.county"),
    new=c("genus","species","lat-long","basis","year","locality","source","uncert_m","state","county"))
    idigbio$dataset <- "idigbio"
    # Separate single iDigBio lat/long column into lat and long
  idigbio <- idigbio %>% separate("lat-long", c("lat", "long"), sep=",", fill="right", extra="merge")
    unique(idigbio$lat)
    # reassign the empty latitude values to NA to avoid confusion
    idigbio$lat[which(idigbio$lat==unique(idigbio$lat)[1] )] <- NA
    unique(idigbio$lat) #better
    # remove the extra symbols and change the column to a numeric variable
    # when using gsub, be sure to include fixed=T to avoid confusion of symbols like "
    idigbio$lat <- as.numeric(gsub("{\"lat\": ","",idigbio$lat, fixed = T))
    unique(idigbio$lat) #best
    # repeat for longitude ("long" column)    
    unique(idigbio$long)# NAs already in place
    # first remove the bracket at the end
    idigbio$long <- gsub("}", "", idigbio$long)
    unique(idigbio$long)
    # then remove the extra symbols and change to numeric
    idigbio$long <- as.numeric(gsub(" \"lon\": ","",idigbio$long, fixed = T))
    unique(idigbio$long)
    # next change the "dwc.eventDate" column to year
    unique(idigbio$year)
    # First we have to remove the characters that are not the year, month or day
    idigbio$year <- gsub("T00:00:00+00:00", "", idigbio$year, fixed = T)
    # now use lubridate package
    idigbio$year <- ymd(idigbio$year) # It automatically changes blank entries to NA
    # now we just need to extract the year and save only that.
    idigbio$year <- year(idigbio$year)
    unique(idigbio$year) # looks good
#fia <- read.csv(file='G:/My Drive/Distributions_TreeSpecies/in-use_occurrence_raw/fia_tree_raw.csv', as.is=T)   # where species information is stored
#plot <- read.csv(file='G:/My Drive/Distributions_TreeSpecies/translation_data_raw/fia_plot_raw.csv', as.is=T)   # where coordinates are stored
    # remove unnecessary columns from plot
    plot2 <- plot[, c("INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT", "LAT", "LON")]
    # Match the location IDs and merge the species and plot data frames 
    fia_coord <- merge(fia, plot2, by.y = c("INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT"), all = F)
    # Add in density here. First make a dataframe with all unique plots and number them.
    u <- unique(fia_coord[,c('SPCD', 'INVYR','STATECD','UNITCD', 'COUNTYCD', 'PLOT', 'LAT', 'LON')])
    ID <- seq(from = 1, to = length(u$INVYR), by = 1)
    u_plot <- data.frame(u, ID)
    # using ID as a label that marks unique plots, see how many individual trees of a species are found in each.
    density_test <- merge(u_plot, fia_coord, by = c("SPCD", "INVYR", "UNITCD", "COUNTYCD", "PLOT", "STATECD"), all = T)
    t <- as.numeric(table(density_test$ID))
    # The results of the table show the number of individuals per plot
    u_plot$density <- t
    # manipulate u_plot further to add onto raw data block; rename as fia
    fia <- u_plot
    rm(plot, plot2, fia_coord, density_test, u, ID)
    # Match up SPCD using 
    #fia_sp <- read.csv(file='G:/My Drive/Distributions_TreeSpecies/translation_data_raw/fia_species_raw.csv', as.is=T)    
    fia <- merge(fia, fia_sp, by = "SPCD", all = F)
    fia <- fia[, 1:16]
    # combine columns into single species name
    fia$species <- paste(fia$GENUS, fia$SPECIES, fia$VARIETY, fia$SUBSPECIES)
    # remove unnecessary columns
    fia <- subset(fia, select = -c(COMMON_NAME, GENUS, SPECIES, VARIETY, SUBSPECIES, SPECIES_SYMBOL, SPCD, ID))
    # Match up STATECD and COUNTYCD using
    #fia_cou <- read.csv(file='G:/My Drive/Distributions_TreeSpecies/translation_data_raw/fia_county_raw.csv', as.is=T)    
    fia <- merge(fia, fia_cou, by = c("STATECD", "COUNTYCD"), all = F)
    # remove unnecessary columns
    fia <- fia[, 1:12]
    fia <- subset(fia, select = -c(STATECD, COUNTYCD, UNITCD.x, UNITCD.y, PLOT))
    # rename remaining columns to match other data sets
    setnames(fia,
             old=c("LAT","LON", "INVYR", "STATENM", "COUNTYNM"),
             new=c("lat","long", "year", "state", "county"))
    fia$dataset <- "FIA"
    fia$genus <- "Quercus"
    fia$locality <- NA
    fia$source <- "FIA" 
    fia$basis <- "WILD_PROVENANCE"
    fia$uncert_m <- NA
keep_col <- c("dataset","genus","species","locality","lat","long","source","year","basis","uncert_m","state","county")
datasets <- list(gbif, consortium, idigbio, fia)
#datasets <- list(consortium, idigbio, fia)

df2 <- data.frame()
for (ds in datasets) {
    ds <- ds %>% select(keep_col) 
  df2 <- rbind(df2, ds) 
}

  df2$species_no <- 0
  df2$gps_determ <- NA
  df2$status <- NA
 
  
   
##  e. Stack datasets to create one large file
occur_all <- rbind(df,df2)


## f. make some changes across this dataset to prevent future errors
occur_all$year[is.na(occur_all$year)]<-"1111"
replace <- c("UNKNOWN","\\<0\\>","N/A","NA","^$")
for (i in replace){
  occur_all$year <- gsub(i,"1111",occur_all$year)
}
occur_all$year<-as.numeric(occur_all$year)
occur_all$lat<-as.numeric(occur_all$lat)
occur_all$long<-as.numeric(occur_all$long)
occur_all$locality<-gsub(",",".",occur_all$locality)

# IDK about this one...
## g. Remove points with ---less than 2 digits after the decimal for lat and/or long---
#occur_dec <- occur_all[grep("\\.[0-9][1-9]",occur_all$lat),]
#nrow(occur_dec) #16678
#occur_dec2 <- occur_all[grep("\\.[0-9][1-9]",occur_dec$long),]
#nrow(occur_dec2) #14881

## h. Reorder dataset before subsetting in next script to place higher quality datasets and most recent records first
occur_all <- occur_all[order(factor(occur_all$dataset,levels=c("other_pts","redlist",
                                                                  "consortium","fia","ex_situ","gbif","andrew_hipp","natureserve","bonap","usda"))),]
head(occur_all)
occur_all <- occur_all[order(occur_all$year, na.last = FALSE, decreasing = T),]
unique(occur_all$year)

##REPLACE
# all oaks? compile each occurrence set first better way?
## i. Subset based on target species list and write file
#occur_sp <- gen_subset(occur_all, (occur_all$species %in% sp_list),"./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/standard_col/datasets_combined/occurrence_raw_compiled.csv")
#  nrow(occur_sp) #11508

## i. Write file
write.csv(occur_all,file="./Distributions_TreeSpecies/in-use_occurrence_raw/standard_col/datasets_combined/occurence_raw_compiled_edit.csv")


##REMOVE/MOVE TO NEXT SCRIPT
################
### 2. CREATE DATA SUBSETS ---- working ----
################

## a. Read in occurrence point file -IF- you've added coordinates for species with locality info but no given lat/long
occur_all <- read.csv(file='./Google Drive/Distributions_TreeSpecies/in-use_occurrence_raw/standard_col/datasets_combined/occurrence_raw_compiled_edit.csv', as.is=T)

## e. Replace unwanted values in specific columns and set to number type where necessary
occur_all$species<-gsub("welshii","havardii",occur_all$species)
  occur_all$species<-gsub("dunnii","palmeri",occur_all$species)
occur_all$year[is.na(occur_all$year)]<-"1111"
  replace <- c("UNKNOWN","\\<0\\>","N/A","NA","^$")
  for (i in replace){
    occur_all$year <- gsub(i,"1111",occur_all$year)
  }
  unique(occur_all$year)
occur_all$year<-as.numeric(occur_all$year)
  occur_all$lat<-as.numeric(occur_all$lat)
  occur_all$long<-as.numeric(occur_all$long)
occur_all$locality<-gsub(",",".",occur_all$locality)
  str(occur_all)
  
## b. Remove points with ---less than 2 digits after the decimal for lat and/or long---
occur_dec <- occur_sp[grep("\\.[0-9][1-9]",occur_sp$lat),]
  nrow(occur_dec) #16678
occur_dec2 <- occur_sp[grep("\\.[0-9][1-9]",occur_dec$long),]
  nrow(occur_dec2) #14881

## c. Remove points ---recorded before 1950--- (or whatever year cutoff you want)
#occur_dec2_1950 <- subset(occur_dec2,year>=1950)
#  nrow(occur_dec2_1950) #6265
# unique(occur_dec2_1950$year)

## d. Reorder dataset before duplicate deletion to place higher quality datasets and most recent records first
occur_dec2 <- occur_dec2[order(factor(occur_dec2$DATASET,levels=c("other_pts","redlist",
  "consortium","fia","ex_situ","gbif","andrew_hipp","natureserve","bonap","usda"))),]
  head(occur_dec2)
occur_dec2 <- occur_dec2[order(-occur_dec2$year, na.last = FALSE),]
  unique(occur_dec2$year)
  str(occur_dec2)

## e. Remove ---spatial duplicates--- based on species name and lat/long rounded to 2 digits after the decimal
occur_dec2_unq<-count.dups(occur_dec2)%>%distinct(SPECIES,lat_round,long_round,.keep_all=TRUE)
  nrow(occur_dec2_unq) #4798
  str(occur_dec2_unq)
write.csv(occur_dec2_unq,file="./sp_occ/merged_data/occur_dec2_unq.csv")

## f. Remove ---unnecessary county centroids--- i.e. when higher quality occurrence point is already located within that county
  # Turn occurrence point data into a SpatialPointsDataFrame
coordinates(occur_dec2_unq) <- c("LONG", "LAT")
wgs84 <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
proj4string(occur_dec2_unq) <- wgs84
  # Load shapefile of US county boundaries
counties_map <- readOGR('./sp_occ/cb_2016_us_county_5m/cb_2016_us_county_5m.shp')
  # project to WGS84
counties_wgs <- spTransform(counties_map, wgs84)
  # Spatial join of occurrence points to counties shapefile
pts.poly <- point.in.poly(occur_dec2_unq, counties_wgs)
write.csv(pts.poly,file="./sp_occ/merged_data/occur_counties.csv")
  # Mark occurrence points that are county centroids within counties that are
    # already represented by geolocated points ------ this may need another look ------
occur_counties <- as.data.frame(pts.poly)
  nrow(occur_counties) #4512
  occur_counties <- occur_counties[order(occur_counties$CENTROID, na.last = FALSE),]
duplicates <- occur_counties[duplicated(occur_counties[c(2,15)]),]
  nrow(duplicates) #3825
to_remove <- duplicates[duplicates$CENTROID %in% "x",]
  nrow(to_remove) #468
  to_remove$county_centroid_dup<-rep("x")
occur_remove_marked <- full_join(occur_counties, to_remove)
  str(occur_remove_marked)
  nrow(occur_remove_marked) #4512
occur_clean <- anti_join(occur_counties, to_remove, by = c("LOCALITY","SPECIES"))
  nrow(occur_clean) #4036

## g. Write files
write.csv(occur_remove_marked,file="./sp_occ/merged_data/occur_dup_counties_marked.csv")
write.csv(occur_clean,file="./sp_occ/merged_data/occur_dup_counties_removed.csv")
