############ 1.1 lower48_oak_extraction
########### 2.23.18 Elizabeth Tokarz
############# 
##############

############### INPUT: lower 48 state TREE csv files from FIA Datamart
############### OUTPUT: smaller csv file (removing all dead trees and containing
############### only our rare oaks, but lacking their coordinates, which will be added in step 2)
###### fia_tree_raw.csv 

source("scripts/set_workingdirectory.R")
#setwd("C:/Users/Elizabeth/Desktop/2017_CTS_fellowship/FIA_unzipped_postgres_data/FIADB_PG/CSV_DATA")

######################################################################################
# Note: a lot of memory is needed to run the function through all at once, so 
# if function does not work, skip and go state by state.
# Estimated time to run: 3 hours

# read in our rare oak vector
rare_oak <- c(6768, 8429, 811, 6782, 851, 6785, 8514, 821, 844, 8492, 836, 8455, 8457)

# for more oak species counts
#rare_oak <- c(801, 802, 803, 804, 8513, 805, 806, 807, 809, 810, 812, 8438, 814, 
#              815, 8441, 8511, 846, 8449, 816, 817, 842, 818, 819, 820, 822, 823, 840,
#              824, 825, 841, 832, 826, 6791, 827, 829, 813, 830, 831, 8512, 845, 8453,
#              833, 847, 834, 8487, 6799, 808, 835, 828, 8459, 8461, 837, 838, 839, 843)

# carya for emily
rare_oak <- c(400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 531, 5983)

# make state vector
lower_48 <- c("AL_TREE.csv", "AZ_TREE.csv", "AR_TREE.csv", "CA_TREE.csv", "CO_TREE.csv",
              "CT_TREE.csv", "DE_TREE.csv", "FL_TREE.csv", "GA_TREE.csv", "ID_TREE.csv",
              "IL_TREE.csv", "IN_TREE.csv", "IA_TREE.csv", "KS_TREE.csv", "KY_TREE.csv",
              "LA_TREE.csv", "ME_TREE.csv", "MD_TREE.csv", "MA_TREE.csv", "MI_TREE.csv",
              "MN_TREE.csv", "MS_TREE.csv", "MO_TREE.csv", "MT_TREE.csv", "NE_TREE.csv",
              "NV_TREE.csv", "NH_TREE.csv", "NJ_TREE.csv", "NM_TREE.csv", "NY_TREE.csv",
              "NC_TREE.csv", "ND_TREE.csv", "OH_TREE.csv", "OK_TREE.csv", "OR_TREE.csv",
              "PA_TREE.csv", "RI_TREE.csv", "SC_TREE.csv", "SD_TREE.csv", "TN_TREE.csv",
              "TX_TREE.csv", "UT_TREE.csv", "VT_TREE.csv", "VA_TREE.csv", "WA_TREE.csv",
              "WV_TREE.csv", "WI_TREE.csv", "WY_TREE.csv")

# make state vector for server
lower_48 <- c("/home/data/FIA_CSV_DATA/AL_TREE.csv", "/home/data/FIA_CSV_DATA/AZ_TREE.csv", 
              "/home/data/FIA_CSV_DATA/AR_TREE.csv", 
              "/home/data/FIA_CSV_DATA/CA_TREE.csv", "/home/data/FIA_CSV_DATA/CO_TREE.csv",
              "/home/data/FIA_CSV_DATA/CT_TREE.csv", "/home/data/FIA_CSV_DATA/DE_TREE.csv", 
              "/home/data/FIA_CSV_DATA/FL_TREE.csv",
              "/home/data/FIA_CSV_DATA/GA_TREE.csv", "/home/data/FIA_CSV_DATA/ID_TREE.csv",
              "/home/data/FIA_CSV_DATA/IL_TREE.csv", "/home/data/FIA_CSV_DATA/IN_TREE.csv", 
              "/home/data/FIA_CSV_DATA/IA_TREE.csv",
              "/home/data/FIA_CSV_DATA/KS_TREE.csv", "/home/data/FIA_CSV_DATA/KY_TREE.csv",
              "/home/data/FIA_CSV_DATA/LA_TREE.csv", "/home/data/FIA_CSV_DATA/ME_TREE.csv",
              "/home/data/FIA_CSV_DATA/MD_TREE.csv",
              "/home/data/FIA_CSV_DATA/MA_TREE.csv", "/home/data/FIA_CSV_DATA/MI_TREE.csv",
              "/home/data/FIA_CSV_DATA/MN_TREE.csv", "/home/data/FIA_CSV_DATA/MS_TREE.csv", 
              "/home/data/FIA_CSV_DATA/MO_TREE.csv",
              "/home/data/FIA_CSV_DATA/MT_TREE.csv", "/home/data/FIA_CSV_DATA/NE_TREE.csv",
              "/home/data/FIA_CSV_DATA/NV_TREE.csv", "/home/data/FIA_CSV_DATA/NH_TREE.csv",
              "/home/data/FIA_CSV_DATA/NJ_TREE.csv",
              "/home/data/FIA_CSV_DATA/NM_TREE.csv", "/home/data/FIA_CSV_DATA/NY_TREE.csv",
              "/home/data/FIA_CSV_DATA/NC_TREE.csv", "/home/data/FIA_CSV_DATA/ND_TREE.csv",
              "/home/data/FIA_CSV_DATA/OH_TREE.csv", 
              "/home/data/FIA_CSV_DATA/OK_TREE.csv", "/home/data/FIA_CSV_DATA/OR_TREE.csv",
              "/home/data/FIA_CSV_DATA/PA_TREE.csv", "/home/data/FIA_CSV_DATA/RI_TREE.csv",
              "/home/data/FIA_CSV_DATA/SC_TREE.csv",
              "/home/data/FIA_CSV_DATA/SD_TREE.csv", "/home/data/FIA_CSV_DATA/TN_TREE.csv",
              "/home/data/FIA_CSV_DATA/TX_TREE.csv", "/home/data/FIA_CSV_DATA/UT_TREE.csv",
              "/home/data/FIA_CSV_DATA/VT_TREE.csv",
              "/home/data/FIA_CSV_DATA/VA_TREE.csv", "/home/data/FIA_CSV_DATA/WA_TREE.csv",
              "/home/data/FIA_CSV_DATA/WV_TREE.csv", "/home/data/FIA_CSV_DATA/WI_TREE.csv", 
              "/home/data/FIA_CSV_DATA/WY_TREE.csv")

IUCN_oak_test <- data.frame()

# this function will load a tree file one state at a time, and run through the csv
# looking for species code matches with our species of interest. All rows with matching
# species codes will be saved and stored to the end product file.
fia_extract <- function(df, sp){
  
  for (i in 1:length(df)){
    onedf <- read.csv(df[i])
    onedf <- onedf[onedf$STATUSCD == 1, ]
    print(head(onedf[, 4:16]))
    
    for (sp in 1:length(sp)){
      oak <- onedf[which(onedf$SPCD==rare_oak[sp]),]
      IUCN_oak_test <- rbind(IUCN_oak_test, oak)
    }
    # print the tail so progress can be seen at the conclusion of each state
    print(tail(IUCN_oak_test[, 4:16]))
  }
  
}

trial <- fia_extract(lower_48, rare_oak)

# decide where to place this file in server
write.csv(x = trial, file = "FIA_tree_raw.csv")

######################################################################################

# read in tree data, which lists all species and the plots in which they were found
# this one will take time to read in
# treeAL <- read.csv("AL_TREE.csv")
treeAL <- read.csv("/home/data/FIA_CSV_DATA/AL_TREE.csv")
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
treeAZ <- read.csv("/home/data/FIA_CSV_DATA/AZ_TREE.csv")
treeAZ <- treeAZ[treeAZ$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeAZ[which(treeAZ$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeAZ)

# treeAR <- read.csv("AR_TREE.csv")
treeAR <- read.csv("/home/data/FIA_CSV_DATA/AR_TREE.csv")
treeAR <- treeAR[treeAR$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeAR[which(treeAR$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeAR)

# treeCA <- read.csv("CA_TREE.csv")
treeCA <- read.csv("/home/data/FIA_CSV_DATA/CA_TREE.csv")
treeCA <- treeCA[treeCA$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeCA[which(treeCA$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeCA)

# treeCO <- read.csv("CO_TREE.csv")
treeCO <- read.csv("/home/data/FIA_CSV_DATA/CO_TREE.csv")
treeCO <- treeCO[treeCO$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeCO[which(treeCO$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeCO)

# treeCT <- read.csv("CT_TREE.csv")
treeCT <- read.csv("/home/data/FIA_CSV_DATA/CT_TREE.csv")
treeCT <- treeCT[treeCT$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeCT[which(treeCT$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeCT)

# treeDE <- read.csv("DE_TREE.csv")
treeDE <- read.csv("/home/data/FIA_CSV_DATA/DE_TREE.csv")
treeDE <- treeDE[treeDE$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeDE[which(treeDE$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeDE)

# treeFL <- read.csv("FL_TREE.csv")
treeFL <- read.csv("/home/data/FIA_CSV_DATA/FL_TREE.csv")
treeFL <- treeFL[treeFL$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeFL[which(treeFL$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeFL)

# treeGA <- read.csv("GA_TREE.csv")
treeGA <- read.csv("/home/data/FIA_CSV_DATA/GA_TREE.csv")
treeGA <- treeGA[treeGA$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeGA[which(treeGA$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeGA)

# treeID <- read.csv("ID_TREE.csv")
treeID <- read.csv("/home/data/FIA_CSV_DATA/ID_TREE.csv")
treeID <- treeID[treeID$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeID[which(treeID$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeID)

# treeIL <- read.csv("IL_TREE.csv")
treeIL <- read.csv("/home/data/FIA_CSV_DATA/IL_TREE.csv")
treeIL <- treeIL[treeIL$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeIL[which(treeIL$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeIL)

# treeIN <- read.csv("IN_TREE.csv")
treeIN <- read.csv("/home/data/FIA_CSV_DATA/IN_TREE.csv")
treeIN <- treeIN[treeIN$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeIN[which(treeIN$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeIN)

# treeIA <- read.csv("IA_TREE.csv")
treeIA <- read.csv("/home/data/FIA_CSV_DATA/IA_TREE.csv")
treeIA <- treeIA[treeIA$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeIA[which(treeIA$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeIA)

# treeKS <- read.csv("KS_TREE.csv")
treeKS <- read.csv("/home/data/FIA_CSV_DATA/KS_TREE.csv")
treeKS <- treeKS[treeKS$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeKS[which(treeKS$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeKS)

# treeKY <- read.csv("KY_TREE.csv")
treeKY <- read.csv("/home/data/FIA_CSV_DATA/KY_TREE.csv")
treeKY <- treeKY[treeKY$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeKY[which(treeKY$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeKY)

# treeLA <- read.csv("LA_TREE.csv")
treeLA <- read.csv("/home/data/FIA_CSV_DATA/LA_TREE.csv")
treeLA <- treeLA[treeLA$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeLA[which(treeLA$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeLA)

# treeME <- read.csv("ME_TREE.csv")
treeME <- read.csv("/home/data/FIA_CSV_DATA/ME_TREE.csv")
treeME <- treeME[treeME$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeME[which(treeME$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeME)

# treeMD <- read.csv("MD_TREE.csv")
treeMD <- read.csv("/home/data/FIA_CSV_DATA/MD_TREE.csv")
treeMD <- treeMD[treeMD$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeMD[which(treeMD$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeMD)

# treeMA <- read.csv("MA_TREE.csv")
treeMA <- read.csv("/home/data/FIA_CSV_DATA/MA_TREE.csv")
treeMA <- treeMA[treeMA$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeMA[which(treeMA$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeMA)

# treeMI <- read.csv("MI_TREE.csv")
treeMI <- read.csv("/home/data/FIA_CSV_DATA/MI_TREE.csv")
treeMI <- treeMI[treeMI$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeMI[which(treeMI$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeMI)

# treeMN <- read.csv("MN_TREE.csv")
treeMN <- read.csv("/home/data/FIA_CSV_DATA/MN_TREE.csv")
treeMN <- treeMN[treeMN$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeMN[which(treeMN$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeMN)

# treeMS <- read.csv("MS_TREE.csv")
treeMS <- read.csv("/home/data/FIA_CSV_DATA/MS_TREE.csv")
treeMS <- treeMS[treeMS$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeMS[which(treeMS$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeMS)

# treeMO <- read.csv("MO_TREE.csv")
treeMO <- read.csv("/home/data/FIA_CSV_DATA/MO_TREE.csv")
treeMO <- treeMO[treeMO$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeMO[which(treeMO$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeMO)

# treeMT <- read.csv("MT_TREE.csv")
treeMT <- read.csv("/home/data/FIA_CSV_DATA/MT_TREE.csv")
treeMT <- treeMT[treeMT$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeMT[which(treeMT$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeMT)

# treeNE <- read.csv("NE_TREE.csv")
treeNE <- read.csv("/home/data/FIA_CSV_DATA/NE_TREE.csv")
treeNE <- treeNE[treeNE$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeNE[which(treeNE$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeNE)

# treeNV <- read.csv("NV_TREE.csv")
treeNV <- read.csv("/home/data/FIA_CSV_DATA/NV_TREE.csv")
treeNV <- treeNV[treeNV$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeNV[which(treeNV$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeNV)

# treeNH <- read.csv("NH_TREE.csv")
treeNH <- read.csv("/home/data/FIA_CSV_DATA/NH_TREE.csv")
treeNH <- treeNH[treeNH$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeNH[which(treeNH$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeNH)

# treeNJ <- read.csv("NJ_TREE.csv")
treeNJ <- read.csv("/home/data/FIA_CSV_DATA/NJ_TREE.csv")
treeNJ <- treeNJ[treeNJ$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeNJ[which(treeNJ$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeNJ)

# treeNM <- read.csv("NM_TREE.csv")
treeNM <- read.csv("/home/data/FIA_CSV_DATA/NM_TREE.csv")
treeNM <- treeNM[treeNM$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeNM[which(treeNM$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeNM)

# treeNY <- read.csv("NY_TREE.csv")
treeNY <- read.csv("/home/data/FIA_CSV_DATA/NY_TREE.csv")
treeNY <- treeNY[treeNY$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeNY[which(treeNY$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeNY)

# treeNC <- read.csv("NC_TREE.csv")
treeNC <- read.csv("/home/data/FIA_CSV_DATA/NC_TREE.csv")
treeNC <- treeNC[treeNC$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeNC[which(treeNC$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeNC)

# treeND <- read.csv("ND_TREE.csv")
treeND <- read.csv("/home/data/FIA_CSV_DATA/ND_TREE.csv")
treeND <- treeND[treeND$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeND[which(treeND$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeND)

# treeOH <- read.csv("OH_TREE.csv")
treeOH <- read.csv("/home/data/FIA_CSV_DATA/OH_TREE.csv")
treeOH <- treeOH[treeOH$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeOH[which(treeOH$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeOH)

# treeOK <- read.csv("OK_TREE.csv")
treeOK <- read.csv("/home/data/FIA_CSV_DATA/OK_TREE.csv")
treeOK <- treeOK[treeOK$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeOK[which(treeOK$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeOK)

# treeOR <- read.csv("OR_TREE.csv")
treeOR <- read.csv("/home/data/FIA_CSV_DATA/OR_TREE.csv")
treeOR <- treeOR[treeOR$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeOR[which(treeOR$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeOR)

# treePA <- read.csv("PA_TREE.csv")
treePA <- read.csv("/home/data/FIA_CSV_DATA/PA_TREE.csv")
treePA <- treePA[treePA$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treePA[which(treePA$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treePA)

# treeRI <- read.csv("RI_TREE.csv")
treeRI <- read.csv("/home/data/FIA_CSV_DATA/RI_TREE.csv")
treeRI <- treeRI[treeRI$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeRI[which(treeRI$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeRI)

# treeSC <- read.csv("SC_TREE.csv")
treeSC <- read.csv("/home/data/FIA_CSV_DATA/SC_TREE.csv")
treeSC <- treeSC[treeSC$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeSC[which(treeSC$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeSC)

# treeSD <- read.csv("SD_TREE.csv")
treeSD <- read.csv("/home/data/FIA_CSV_DATA/SD_TREE.csv")
treeSD <- treeSD[treeSD$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeSD[which(treeSD$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeSD)

# treeTN <- read.csv("TN_TREE.csv")
treeTN <- read.csv("/home/data/FIA_CSV_DATA/TN_TREE.csv")
treeTN <- treeTN[treeTN$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeTN[which(treeTN$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeTN)

# treeTX <- read.csv("TX_TREE.csv")
treeTX <- read.csv("/home/data/FIA_CSV_DATA/TX_TREE.csv")
treeTX <- treeTX[treeTX$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeTX[which(treeTX$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeTX)

# treeUT <- read.csv("UT_TREE.csv")
treeUT <- read.csv("/home/data/FIA_CSV_DATA/UT_TREE.csv")
treeUT <- treeUT[treeUT$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeUT[which(treeUT$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeUT)

# treeVT <- read.csv("VT_TREE.csv")
treeVT <- read.csv("/home/data/FIA_CSV_DATA/VT_TREE.csv")
treeVT <- treeVT[treeVT$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeVT[which(treeVT$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeVT)

# treeVA <- read.csv("VA_TREE.csv")
treeVA <- read.csv("/home/data/FIA_CSV_DATA/VA_TREE.csv")
treeVA <- treeVA[treeVA$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeVA[which(treeVA$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeVA)

# treeWA <- read.csv("WA_TREE.csv")
treeWA <- read.csv("/home/data/FIA_CSV_DATA/WA_TREE.csv")
treeWA <- treeWA[treeWA$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeWA[which(treeWA$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeWA)

# treeWV <- read.csv("WV_TREE.csv")
treeWV <- read.csv("/home/data/FIA_CSV_DATA/WV_TREE.csv")
treeWV <- treeWV[treeWV$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeWV[which(treeWV$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeWV)

# treeWI <- read.csv("WI_TREE.csv")
treeWI <- read.csv("/home/data/FIA_CSV_DATA/WI_TREE.csv")
treeWI <- treeWI[treeWI$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeWI[which(treeWI$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeWI)

# treeWY <- read.csv("WY_TREE.csv")
treeWY <- read.csv("/home/data/FIA_CSV_DATA/WY_TREE.csv")
treeWY <- treeWY[treeWY$STATUSCD == 1, ]

for (sp in 1:length(rare_oak)){
  oak <- treeWY[which(treeWY$SPCD==rare_oak[sp]),]
  IUCN_oak <- rbind(IUCN_oak, oak)
}
rm(treeWY)

write.csv(x = IUCN_oak, file = "fia_tree_raw.csv")
