path.out <- "."
path.dat <- "/home/data"
state.files <- dir(file.path(path.dat, "FIA_CSV_DATA"), "_TREE.csv")
spp.cd <- 621

state.files=state.files[c(3, 6,7)]

dat.blank <- data.frame()
pb <- txtProgressBar(min=0, max=length(state.files), style=3)
pb.ind = 1
for(STATE in state.files){
	setTxtProgressBar(pb, pb.ind)
	pb.ind = pb.ind+1

	df.tmp <- read.csv(file.path(path.dat, "FIA_CSV_DATA", STATE))
	
	# Subset just for SPP of interest
	df.tmp <- df.tmp[df.tmp$SPCD==spp.cd,]
	
	if(nrow(df.tmp)==0) next
	dat.blank <- rbind(dat.blank, df.tmp)
	
	rm(df.tmp)	
}

write.csv(dat.blank, file.path(path.out, paste0(spp.cd, "_FIA.csv")), row.names=F)