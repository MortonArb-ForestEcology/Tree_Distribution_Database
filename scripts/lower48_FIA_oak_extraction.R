############ 1.
########### 2.23.18 Elizabeth Tokarz
############# can't download TREE CSV, so I'll create one for the lower 48 for now
############# 
##############

############### INPUT: lower 48 state TREE csv files from FIA Datamart
############### OUTPUT: smaller csv file (removing all dead trees and containing
############### only our rare oaks)
###### lower48_Quercus.csv 

###### *initially skip Georgia and Minnesota because files are so large

# working directory cannot be changed on the server, so simply specify the route whenever uploading a CSV file
#setwd("C:/Users/Elizabeth/Desktop/2017_CTS_fellowship/FIA_unzipped_postgres_data/FIADB_PG/CSV_DATA")
# read in our rare oak vector
rare_oak <- c(6768, 8429, 811, 6782, 851, 6785, 8514, 821, 844, 8492, 836, 8455, 8457)

# Trial a function:

# read in tree data, which lists all species and the plots in which they were found
# this one will take time to read in
# treeAL <- read.csv("AL_TREE.csv")
treeAL <- read.csv("../data/CSV_DATA/AL_TREE.csv")

# first we want to ensure that all the trees in this sample are live
treeAL <- treeAL[treeAL$STATUSCD == 1, ]

# and make a new Oak data frame

IUCN_oak <- data.frame()
  
# Now we can cycle through our vector of rare oak species codes and extract those 
# rows from Alabama

for (sp in 1:length(rare_oak)){
  oak <- treeAL[which(treeAL$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
  }

summary(IUCN_oak$SPCD)
unique(IUCN_oak$SPCD)
# Looks good!!
rm(treeAL)
############### New state
# treeAZ <- read.csv("AZ_TREE.csv")
treeAZ <- read.csv("../data/CSV_DATA/AZ_TREE.csv")
treeAZ <- treeAZ[treeAZ$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeAZ[which(treeAZ$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeAZ)

# treeAR <- read.csv("AR_TREE.csv")
treeAR <- read.csv("../data/CSV_DATA/AR_TREE.csv")
treeAR <- treeAR[treeAR$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeAR[which(treeAR$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeAR)

# treeCA <- read.csv("CA_TREE.csv")
treeCA <- read.csv("../data/CSV_DATA/CA_TREE.csv")
treeCA <- treeCA[treeCA$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeCA[which(treeCA$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeCA)

# treeCO <- read.csv("CO_TREE.csv")
treeCO <- read.csv("../data/CSV_DATA/CO_TREE.csv")
treeCO <- treeCO[treeCO$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeCO[which(treeCO$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeCO)

# treeCT <- read.csv("CT_TREE.csv")
treeCT <- read.csv("../data/CSV_DATA/CT_TREE.csv")
treeCT <- treeCT[treeCT$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeCT[which(treeCT$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeCT)

# treeDE <- read.csv("DE_TREE.csv")
treeDE <- read.csv("../data/CSV_DATA/DE_TREE.csv")
treeDE <- treeDE[treeDE$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeDE[which(treeDE$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeDE)

# treeFL <- read.csv("FL_TREE.csv")
treeFL <- read.csv("../data/CSV_DATA/FL_TREE.csv")
treeFL <- treeFL[treeFL$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeFL[which(treeFL$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeFL)

# treeGA <- read.csv("GA_TREE.csv")
treeGA <- read.csv("../data/CSV_DATA/GA_TREE.csv")
treeGA <- treeGA[treeGA$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeGA[which(treeGA$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeGA)

# treeID <- read.csv("ID_TREE.csv")
treeID <- read.csv("../data/CSV_DATA/ID_TREE.csv")
treeID <- treeID[treeID$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeID[which(treeID$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeID)

# treeIL <- read.csv("IL_TREE.csv")
treeIL <- read.csv("../data/CSV_DATA/IL_TREE.csv")
treeIL <- treeIL[treeIL$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeIL[which(treeIL$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeIL)

# treeIN <- read.csv("IN_TREE.csv")
treeIN <- read.csv("../data/CSV_DATA/IN_TREE.csv")
treeIN <- treeIN[treeIN$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeIN[which(treeIN$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeIN)

# treeIA <- read.csv("IA_TREE.csv")
treeIA <- read.csv("../data/CSV_DATA/IA_TREE.csv")
treeIA <- treeIA[treeIA$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeIA[which(treeIA$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeIA)

# treeKS <- read.csv("KS_TREE.csv")
treeKS <- read.csv("../data/CSV_DATA/KS_TREE.csv")
treeKS <- treeKS[treeKS$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeKS[which(treeKS$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeKS)

# treeKY <- read.csv("KY_TREE.csv")
treeKY <- read.csv("../data/CSV_DATA/KY_TREE.csv")
treeKY <- treeKY[treeKY$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeKY[which(treeKY$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeKY)

# treeLA <- read.csv("LA_TREE.csv")
treeLA <- read.csv("../data/CSV_DATA/LA_TREE.csv")
treeLA <- treeLA[treeLA$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeLA[which(treeLA$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeLA)

# treeME <- read.csv("ME_TREE.csv")
treeME <- read.csv("../data/CSV_DATA/ME_TREE.csv")
treeME <- treeME[treeME$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeME[which(treeME$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeME)

# treeMD <- read.csv("MD_TREE.csv")
treeMD <- read.csv("../data/CSV_DATA/MD_TREE.csv")
treeMD <- treeMD[treeMD$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeMD[which(treeMD$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeMD)

# treeMA <- read.csv("MA_TREE.csv")
treeMA <- read.csv("../data/CSV_DATA/MA_TREE.csv")
treeMA <- treeMA[treeMA$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeMA[which(treeMA$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeMA)

# treeMI <- read.csv("MI_TREE.csv")
treeMI <- read.csv("../data/CSV_DATA/MI_TREE.csv")
treeMI <- treeMI[treeMI$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeMI[which(treeMI$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeMI)

# treeMN <- read.csv("MN_TREE.csv")
treeMN <- read.csv("../data/CSV_DATA/MN_TREE.csv")
treeMN <- treeMN[treeMN$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeMN[which(treeMN$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeMN)

# treeMS <- read.csv("MS_TREE.csv")
treeMS <- read.csv("../data/CSV_DATA/MS_TREE.csv")
treeMS <- treeMS[treeMS$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeMS[which(treeMS$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeMS)

# treeMO <- read.csv("MO_TREE.csv")
treeMO <- read.csv("../data/CSV_DATA/MO_TREE.csv")
treeMO <- treeMO[treeMO$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeMO[which(treeMO$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeMO)

# treeMT <- read.csv("MT_TREE.csv")
treeMT <- read.csv("../data/CSV_DATA/MT_TREE.csv")
treeMT <- treeMT[treeMT$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeMT[which(treeMT$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeMT)

# treeNE <- read.csv("NE_TREE.csv")
treeNE <- read.csv("../data/CSV_DATA/NE_TREE.csv")
treeNE <- treeNE[treeNE$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeNE[which(treeNE$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeNE)

# treeNV <- read.csv("NV_TREE.csv")
treeNV <- read.csv("../data/CSV_DATA/NV_TREE.csv")
treeNV <- treeNV[treeNV$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeNV[which(treeNV$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeNV)

# treeNH <- read.csv("NH_TREE.csv")
treeNH <- read.csv("../data/CSV_DATA/NH_TREE.csv")
treeNH <- treeNH[treeNH$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeNH[which(treeNH$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeNH)

# treeNJ <- read.csv("NJ_TREE.csv")
treeNJ <- read.csv("../data/CSV_DATA/NJ_TREE.csv")
treeNJ <- treeNJ[treeNJ$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeNJ[which(treeNJ$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeNJ)

# treeNM <- read.csv("NM_TREE.csv")
treeNM <- read.csv("../data/CSV_DATA/NM_TREE.csv")
treeNM <- treeNM[treeNM$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeNM[which(treeNM$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeNM)

# treeNY <- read.csv("NY_TREE.csv")
treeNY <- read.csv("../data/CSV_DATA/NY_TREE.csv")
treeNY <- treeNY[treeNY$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeNY[which(treeNY$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeNY)

# treeNC <- read.csv("NC_TREE.csv")
treeNC <- read.csv("../data/CSV_DATA/NC_TREE.csv")
treeNC <- treeNC[treeNC$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeNC[which(treeNC$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeNC)

# treeND <- read.csv("ND_TREE.csv")
treeND <- read.csv("../data/CSV_DATA/ND_TREE.csv")
treeND <- treeND[treeND$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeND[which(treeND$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeND)

# treeOH <- read.csv("OH_TREE.csv")
treeOH <- read.csv("../data/CSV_DATA/OH_TREE.csv")
treeOH <- treeOH[treeOH$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeOH[which(treeOH$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeOH)

# treeOK <- read.csv("OK_TREE.csv")
treeOK <- read.csv("../data/CSV_DATA/OK_TREE.csv")
treeOK <- treeOK[treeOK$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeOK[which(treeOK$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeOK)

# treeOR <- read.csv("OR_TREE.csv")
treeOR <- read.csv("../data/CSV_DATA/OR_TREE.csv")
treeOR <- treeOR[treeOR$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeOR[which(treeOR$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeOR)

# treePA <- read.csv("PA_TREE.csv")
treePA <- read.csv("../data/CSV_DATA/PA_TREE.csv")
treePA <- treePA[treePA$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treePA[which(treePA$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treePA)

# treeRI <- read.csv("RI_TREE.csv")
treeRI <- read.csv("../data/CSV_DATA/RI_TREE.csv")
treeRI <- treeRI[treeRI$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeRI[which(treeRI$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeRI)

# skip SC
# treeSC <- read.csv("SC_TREE.csv")
treeSC <- read.csv("../data/CSV_DATA/SC_TREE.csv")
treeSC <- treeSC[treeSC$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeSC[which(treeSC$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeSC)

# treeSD <- read.csv("SD_TREE.csv")
treeSD <- read.csv("../data/CSV_DATA/SD_TREE.csv")
treeSD <- treeSD[treeSD$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeSD[which(treeSD$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeSD)

# error TN
# treeTN <- read.csv("TN_TREE.csv")
treeTN <- read.csv("../data/CSV_DATA/TN_TREE.csv")
treeTN <- treeTN[treeTN$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeTN[which(treeTN$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeTN)

# treeTX <- read.csv("TX_TREE.csv")
treeTX <- read.csv("../data/CSV_DATA/TX_TREE.csv")
treeTX <- treeTX[treeTX$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeTX[which(treeTX$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeTX)
# no more observations after this?

# treeUT <- read.csv("UT_TREE.csv")
treeUT <- read.csv("../data/CSV_DATA/UT_TREE.csv")
treeUT <- treeUT[treeUT$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeUT[which(treeUT$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeUT)

# treeVT <- read.csv("VT_TREE.csv")
treeVT <- read.csv("../data/CSV_DATA/VT_TREE.csv")
treeVT <- treeVT[treeVT$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeVT[which(treeVT$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeVT)

# treeVA <- read.csv("VA_TREE.csv")
treeVA <- read.csv("../data/CSV_DATA/VA_TREE.csv")
treeVA <- treeVA[treeVA$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeVA[which(treeVA$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeVA)

# treeWA <- read.csv("WA_TREE.csv")
treeWA <- read.csv("../data/CSV_DATA/WA_TREE.csv")
treeWA <- treeWA[treeWA$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeWA[which(treeWA$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeWA)

# treeWV <- read.csv("WV_TREE.csv")
treeWV <- read.csv("../data/CSV_DATA/WV_TREE.csv")
treeWV <- treeWV[treeWV$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeWV[which(treeWV$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeWV)

# error?
# treeWI <- read.csv("WI_TREE.csv")
treeWI <- read.csv("../data/CSV_DATA/WI_TREE.csv")
treeWI <- treeWI[treeWI$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeWI[which(treeWI$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeWI)

# treeWY <- read.csv("WY_TREE.csv")
treeWY <- read.csv("../data/CSV_DATA/WY_TREE.csv")
treeWY <- treeWY[treeWY$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeWY[which(treeWY$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeWY)

write.csv(x = IUCN_oak, file = "lower_48_Quercus.csv")


# Now try making this a function for all states at once
# have vector of state data frame names for d.f and a vector of rare_oak for sp
fia_extract <- function(d.f, sp){
  
  d.f <- read.csv("../data/CSV_DATA/df.csv")
  d.f <- d.f[d.f$STATUSCD == 1, ]
  
  
}