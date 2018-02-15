dat.out <- "ITRDB_test"


# ----------------------
# Doing the actual query
# ----------------------
itrdb.out <- extract.itrdb(area.extract=europe, download.types=c("Chronology", "Raw Measurements"), dir.out="~/Desktop/ITRDB_europe")
write.csv(itrdb.out, "~/Desktop/ITRDB_europe/ITRDB_Europe_metadata.csv", row.names=F)
# ----------------------


itrdb.sp <- SpatialPointsDataFrame(coords=itrdb.out[,c("Longitude", "Latitude")], itrdb.out, proj4string = CRS(projection(europe)))

plot(europe)
plot(itrdb.sp, add=T, pch=20, col="red", cex=0.5)
