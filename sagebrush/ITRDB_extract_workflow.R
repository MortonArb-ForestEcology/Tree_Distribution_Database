dat.out <- "ITRDB_test"
 
# # ----------------------
# # Example of pulling a state shapefile
# # ----------------------
# us <- raster::getData("GADM", country="USA", level=1)
# us$NAME_1 <- as.factor(us$NAME_1)
# states <- c("Pennsylvania")
# # borders <- us[us$NAME_1 %in% c("New York"),]
# borders <- us[us$NAME_1 %in% states,]
# # ----------------------
# 
# # ----------------------
# # Making a square bounding box if you just want an area
# # ----------------------
# xmin = xmin(borders)
# xmax = xmax(borders)
# ymin = ymin(borders)
# ymax = ymax(borders)
# 
# test <- data.frame(x=c(xmin, xmax, xmax, xmin, xmin),
#                    y=c(ymin, ymin, ymax, ymax, ymin))
# 
# test <- Polygon(test)
# test <- SpatialPolygons(list(Polygons(list(test), ID="a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# # ----------------------



# ----------------------
# Getting Europe
# ----------------------
europe <- rworldmap::getMap()
europe <- europe[europe$GLOCAF=="Europe" & !is.na(europe$GLOCAF) & europe$TYPE %in% c("Sovereign country", "Country"),]
europe <- europe[!europe$IMAGE24 %in% c("Middle East", "Greenland"),]
summary(europe)
plot(europe)


# worldmap2 <- ggplot2::map_data("world")
# worldmap2$region <- as.factor(worldmap2$region)

# ----------------------



# ----------------------
# Doing the actual query
# ----------------------
itrdb.out <- extract.itrdb(area.extract=europe, download.types=c("Chronology", "Raw Measurements"), dir.out="~/Desktop/ITRDB_europe")
write.csv(itrdb.out, "~/Desktop/ITRDB_europe/ITRDB_Europe_metadata.csv", row.names=F)
# ----------------------


itrdb.sp <- SpatialPointsDataFrame(coords=itrdb.out[,c("Longitude", "Latitude")], itrdb.out, proj4string = CRS(projection(europe)))

plot(europe)
plot(itrdb.sp, add=T, pch=20, col="red", cex=0.5)
