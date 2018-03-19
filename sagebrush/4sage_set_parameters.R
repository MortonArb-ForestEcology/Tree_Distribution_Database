# 4sage_set_parameters.R
# ####################################################################################################
# ##load necessary libraries
# ####################################################################################################
# library(rgdal)
# library(rgeos)
# library(dismo)
####################################################################################################
##set the column names for the data output
####################################################################################################
#set up output files for stats
cnms <- c("species", "contr_import", "variable", "percentage", "vars_incl", 
          "num_var", "occ_incl", "neg_occs", "QC", "area", 
          "soil_elev", "train_occ", "test_occ", "test_absences", "MaxKappa", "min_train_presence",
          "train_absences", "total_area", "reps", "replicate", "AUC", 
          "nAUC", "cAUC", "avg_dist_train_km", "avg_dist_null_km", "occ_test_pwd", 
          "bg_test_pwd", "test_per", "var_incl_perc", "current_model", "projected_model", 
          "model_algorithm", "date_run", "iterations", "parameter_no", "AICc")

out.cols1 <- matrix(cnms, nrow=1, ncol=length(cnms))
####################################################################################################
##create the data output file, if it was not already created
####################################################################################################
if (!file.exists(file.path(f_out, "sage_model_reps.csv"))){
  write.table(out.cols1, file.path(f_out, "sage_model_reps.csv"), sep=",", row.names=FALSE, col.names=FALSE)
} 
  rm(cnms)

####################################################################################################
##create the list names for the fit files
####################################################################################################
ls.nms <- c("id", "occ.all", "occ", "occ.xy", "occtrain", "occtest", "backgr", "backgr2", "mod.curr", 
            "me.importance", "me.contribution", "out.table.imp", "out.table.cont", "reps", 
            "e.mod.curr", "p.mod.curr", "thresh1", "thresh2", "back_train", "back_test", "back_test_pwd", "occtest_pwd", "ms", "par_no", "aicc")
####################################################################################################
#set parameters (eventually through the variable_sets.txt dataframe)
# v.sets <- read.delim("variable_sets.txt", sep="\t", header=TRUE, as.is=TRUE)
#   ms <- v.sets[v.sets$vars.incl == vars.incl,]
m.sets <- read.delim("model_sets.txt", sep="\t", header=TRUE, as.is=TRUE)
  ms <- m.sets[m.sets$mset == mset,]

##bring in variable sets
#   v.sets <- read.delim("variable_sets_new.txt", sep="\t", header=TRUE, as.is=TRUE)
#   vs <- v.sets[v.sets$vars.incl == vars.incl,]
####################################################################################################
if (model_algorithm == "MaxEnt") {
      ##args for maxent()
      maxiter <- ms$maxiter
      convergeth <- ms$convergeth
      betamult <- ms$betamult
      # meoutfiles="F:/SDM_output/trial_output"
      # ncores=4
      # maxiter=1500
      # convergeth=0.00001
      #maxbg=10000
  
  } else if (model_algorithm == "RandomForest") {
      #
      #
      #
      maxiter    <- NA
      convergeth <- NA
      betamult   <- NA
    }
####################################################################################
##set variables for the SDM runs (run_cur & run_fut) and various other SDM fxns and scripts
ms$model_algorithm <- model_algorithm
if(model_algorithm == "MaxEnt"){ms$ma <- "me"} else if(model_algorithm == "Bioclim"){ms$ma <- "bc"} else if(model_algorithm == "GLM"){ms$ma <- "glm"} else if(model_algorithm == "RandomForest"){ms$ma <- "rf"}

#"pmod" is projected model code to pull in files; "pnm" is corresponding projected model in .RData file
clim <- clims[clims$fullmod == model.climate,]
# vs$clim.folder <- strsplit(clim$zipfile, "_bio_30s_no_tile_asc.zip")
# ms$pmod <- strsplit(clim$zipfile, "_bio_30s_no_tile_asc.zip")
ms$pmod2 <- clim$pmod
ms$pmod <- clim$fullmod
ms$pnm <- clim$pnm
ms$yr <- clim$yrs

rm(clim)

###################################
f_loc <- file.path(f_out, "out", ms$pmod)
model.outdir <- file.path(f_out, "model_output")

# if (!file.exists(paste0(m_loc))){dir.create(m_loc, recursive=TRUE)}
if (!file.exists(model.outdir)){dir.create(model.outdir, recursive=TRUE)}
if (!file.exists(f_loc)){dir.create(f_loc, recursive=TRUE)}
if (!file.exists(model.outdir)){dir.create(model.outdir, recursive=TRUE)}

###################################
# #set the filenames for the rasters for both current and future projections  
# f_loc <- paste(f_out, "out/", ms$pmod, "/", sep="")
r_loc  <- file.path(f_out, "raster_models", ms$pmod)
f_locc <- file.path(f_out, "out", "current")
r_locc <- file.path(f_out, "raster_models", "current")
s_loc  <- file.path(f_out, "polygon_models", ms$pmod)
s_locc <- file.path(f_out, "polygon_models", "current")

###################################
### set path for Python
  pypath <- '/Library/Frameworks/GDAL.framework/Versions/1.9/Programs/gdal_polygonize.py'
