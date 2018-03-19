##1_set_sdm_run
####################################################################################################
##set the working environment depending on the computer on which working
####################################################################################################
# rm(list=ls())
if (Sys.info()[1] == "Windows"){
  if (Sys.info()[4] == "CBG002565") { ##my office desktop
#     setwd("E:/Still_GIS/R_working_directory/SDM"); print(getwd())
      f_out <- "SDM_output"

#     f_out <- "Z:/Project_Folders/17 spp/"
#     print(paste0("Working from my office desktop ", Sys.info()[4],"."))
  } else if (Sys.info()[4] == "CBG002806") {   ##new GIS lab desktop (center, window)
#     setwd("Y:/Y_SDM"); print(getwd())
    f_out <- "SDM_output"
    #     print(paste0("Working from the GIS lab desktop ", Sys.info()[4],", (center, window)."))
  } else if (Sys.info()[4] == "CBG002793") {   ##new GIS lab desktop (center, window)
    #     setwd("Y:/Y_SDM"); print(getwd())
    f_out <- "SDM_output"
    #     print(paste0("Working from the GIS lab desktop ", Sys.info()[4],", (center, window)."))
  } else if (Sys.info()[4] == "CBG002807") {   ##new GIS lab desktop (center, window)
#     setwd("C:/Users/sstill/Desktop/Sagebrush_SDM"); print(getwd())
    f_out <- "SDM_output"
#     setwd("C:/Users/sstill/Desktop/Sagebrush_SDM"); print(getwd())
#     f_out <- "C:/Users/sstill/Desktop/SDM_output"
    print(paste0("Working from the GIS lab desktop ", Sys.info()[4],", (center, door)."))
  } 
} else {
  setwd(getwd()) ##my work MacBook Pro
  f_out <- "SDM_output"
  #   pypath <- "/Library/Frameworks/GDAL.framework/Versions/current/Programs/gdal_polygonize.py"
}

# print(getwd()) 
