Updated: 8 May 2018

# Tree_Distribution_Database
Repository for curating occurrence and predictor data of tree distributions

All scripts are located in folder "scripts"
Examples and attempts to use similar scripts with data that have not been refined are in folder "trials"
The "sagebrush" folder contains scripts used in Shannon's sagebrush distribution modeling project, which can serve as an example for our project.
We will describe the scripts folder first because it is most relevant.

# 2. scripts folder
Within folder "scripts", files have been numbered in the order they are to be used in the workflow.
Data files used in these scripts are on the Google Drive "Distributions_TreeSpecies" and/or server and the scripts 
should contain references to their proper locations

 Occurrence data for our oak species of interest has been downloaded prior to its use in our scripts. 
   None of the data comes from API currently, but we hope to update these file paths when API becomes available, we hope.
   
# Workflow   
## 	0.calculate_met by Christy
   Purpose: To calculate yearly statistics from daily mean temperature values
   
   Input: PRISM data
   
   Output: Climate normal raster
   
   Packages: raster
   
## 	1.0.set_workingdirectory by Shannon
  Purpose: Customize the file paths depending on who is running the scripts and which computers they are using.
  
  Input: Run if-else script on the computer you will be using. The script will read your Sys.info()
  
  Output: File path abbreviations stored as global values in your R console
  
  Packages: None
  
## 1.1.lower48_FIA_oak_extraction by Elizabeth
  Purpose: FIA data comes in a unique form, where individual trees are not directly linked with their
  coordinates. Here we first cycle through the tree occurrence data from 48 states and extract all
  occurrences labeled as one of our species of interest. The trees are not linked with their coordinates until step 2.
  
  Input: TREE.csv files for lower 48 states from FIA datamart
         Found in fia_translation_data_raw/FIA TREES--January 2018 folder in Google Drive
  
  Output: fia_tree_raw.csv; single csv containing FIA occurrence data for 28 species of interest, without coordinates; 
          Found in in-use_occurrence_raw folder in Google Drive
          
  Packages: none
  
  Functions: fia_extract (This function cycles through each state TREE.csv, one at a time, removes all trees marked as not living, and then   
  goes through all the remaining occurrences, checking for each of our 13 species of interest with FIA codes, one at a time. If it finds a 
  match, it pulls the occurrence and saves it to a new data frame. As it goes through each state and finds more occurrences, the data frame 
  grows until it reaches its final size.) Note: This function may take a while to run and may stop if there is not enough memory to run it.
  The file can also be built state by state with the long-hand version of the function, included immediately following the function.
  
  Notes: 1) This script may need to be updated if FIA API becomes available.
  2) Keep in mind that the coordinates used for these FIA occurrences may differ from the actual location of 
  an occurrence, up to a mile, due to "data fuzzing". 3) Only six of our 28 species of interest were 
  observed in the most recent FIA surveys, though 13 of the 28 had been assigned species codes, 
  which suggests that the other seven species have been observed during previous survey years. Several of 
  the 28 species are shrubs and would not be expected to be recorded in the surveys anyway.
  Species represented by positive occurrences in FIA: Quercus engelmannii, Q. graciliformis, 
  Q. laceyi, Q. lobata, Q. oglethorpensis, Q. similis.

## 1.2.prep_GBIF_for_GeoLocate by Elizabeth
  Purpose: STEP ONE--After downloading GBIF occurrence data from the website in Darwin Core format, several occurrences
  have locality information, such as state, county and a description of where it was found, but lack coordinates. 
  Tulane's GEOLocate online application finds coordinates of places based on a description of state, county 
  and other locality information. In an effort to augment our occurrence datasets, we edited the Darwin Core 
  columns to fit the format necessary to use the GEOLocate application, creating a CSV to load into the web application.
  The CSV we download from the application after it has reassigned coordinates to the occurrences (gbif_DC_post-georef.csv)
  is used in step 2 of this script. STEP TWO--Further revision was done to the geo-referenced coordinates to reinstate
  coordinates that had previously lacked "issues". After labelling county-specific occurrences that lack coordinates, all remaining
  occurreces lacking coordinates were removed. Finally, the remaining occurrences were re-ordered according to year and precision.
  
  Input: STEP ONE--gbif_DarwinCore_edit.csv
         STEP TWO--gbif_DC_post-georef.csv
                   Found in in-use_occurrence_raw folder on Google Drive
  
  Output: STEP ONE--gbif_DC_cleaned.csv; gbif_DC_georef.csv
          STEP TWO--gbif_DC_post-georef_revised.csv
                    Found in in-use_occurrence_raw folder on Google Drive
  
  Packages: dplyr, rgbif, tidyr, data.table
  
  Functions: extract_state_new (Search in the state column, locality and verbatim locality columns for a state name or abbreviation. Write 
  in that state's name in the new state column.)
  
  extract_county_new_v2 (Search in the state and county columns for a paired match to a state and county as found in the FIA county 
  reference file. Write in appropriate matches in the new county column.)
  
  Notes: After running part one of this script, the resulting CSV must be uploaded into a separate online application and further changed. 
  Running our csv through GEOLocate takes a significant amount of time and the completed result, which can be saved with a 
  code, must be redownloaded into a new CSV that will be renamed as gbif_DC_post-georef.csv. Some of the existing coordinates 
  may also be affected by running GEOLocate, so round two of this script (1.2) will help check for errors.
  
## 2.1.compile_occurrence_point_datasets by Emily and Elizabeth
  Purpose: Using the FIA and GBIF data compiled in step 1.1 and 1.2, as well as occurrence data 
  from other sources (gbif, consortium, idigbio, bonap, A. Hipp, exsitu, natureserve, redlist, 
  usdaplants, other), we will make a larger stacked data frame containing all the occurrence data 
  we will need to make our model. Column names will be made uniform.
  
  Input: all csv files from in_use_occurrence_raw folder and fia_translation_data_raw folder (*) on Google Drive
         gbif_DC_post-georef_revised.csv (step 3)
         consortium_raw.csv (step 4)
         idigbio_raw.csv (step 5)
         fia_tree_raw.csv (step 6)
         fia_plot_raw.csv* (step 6)
         fia_species_raw.csv* (step 6)
         fia_county_raw.csv* (step 6)
         target_species_list.csv (step 1: to add missing columns to each dataset)
         /cb_2016_us_county_5m/cb_2016_us_county_5m.shp (step 7)
  
  Output: standardized_col_compiled.csv (step 2)
          gbif_compiled.csv (step 3)
          consortium_compiled.csv (step 4)
          idigbio_compiled.csv (step 5)
          fia_compiled.csv (step 6)
          fia_absence_compiled.csv (absence data) (step 6 part 2)
          occurrence_compiled_dec2.csv containing uniform occurrence data for 28 species of 
          interest from several sources. To be used in script 2.2 below. (step 7)
          Found in in-use_occurrence_compiled folder on Google Drive
  
  Output Column names: dataset, genus, species, decimalLongitude, decimalLatitude, basisOfRecord, year, locality, 
                institutionCode, gps_determ, occurrenceRemarks, coordinateUncertaintyInMeters, stateProvince,
                county, speciesKey, synonyms, fia_codes, order, family, specificEpithet, intraspecificEpithet,
                scientificName, collectionCode, datasetName, catalogNumber, recordNumber, precision, georeferenceSources,
                individualCount, countryCode, municipality, locationRemarks, habitat, fieldNotes, issue, 
                scientificNameAuthorship, geodeticDatum, country, dataQualityScore
  
  Packages: dplyr, plyr, rgbif, ridigbio, data.table, tidyr, stringr, lubridate, rgdal, geosphere
  
  Functions: gen_subset ()
             rbind.all.columns ()
  
  Notes: *FIA Absence Data also compiled here (step 6)* After the coordinates of the positive occurrences 
          are assigned to the FIA occurrences, the full dataset of the plots that are surveyed are compared
          to the positive occurrences for each species. Plots in which each of the 13 species that could
          possibly have been found are absent are saved in a new dataframe.

## 2.2.subset_occurrence_point_data by Emily
  Purpose: After compiling all the occurrences into one dataset in the above script, we must remove 
  duplicate occurrences, first overall, and then county centroids for counties which are already represented.
  
  Input: occurrence_compiled_dec2.csv in in-use_occurrence_compiled folder on Google Drive (from script 2.1)
         /cb_2016_us_county_5m/cb_2016_us_county_5m.shp (county shapefile used to create county centroid data frame)
         Found in in-use_occurrence_compiled folder and cb_2016_us_county_5m folder on Google Drive
  
  Output: occurrence_compiled_dec2_unique.csv (no duplicates)
          occurrence_compiled_dec2_unique_countyDupMarked.csv (county centroids occurring in counties with other occurrences are marked)
          occurrence_compiled_dec2_unique_countyDupRemoved.csv (marked counties from above are removed)
          occurrence_compiled_dec2_unique_countyDupRemoved_acceptedDistMarked.csv (above file with additional column marking remaining 
                                                                                   counties that are not in the "accepted" range)
          Found in in-use_occurrence_compiled folder on Google Drive
  
  Column Names: X, dataset, genus, species, decimalLongitude, decimalLatitude, basisOfRecord, year, 
                locality, institutionCode, gps_determ, occurrenceRemarks, coordinatePrecisionInMeters, 
                stateProvince, county, speciesKey, synonyms, fia_codes, order, family, specificEpithet,
                infraspecificEpithet, scientificName, collectionCode, datasetName, catalogNumber, 
                recordNumber, precision, georeferenceSources, individualCount, countryCode, municipality, 
                locationRemarks, habitat, fieldNotes, issue, scientificName, geodeticDatum, country,
                dataQualityScore, lat_round, long_round, obs_no
  
  Packages: sp, rgdal, spatialEco, geosphere, mapview, data.table, dplyr, plyr
  
  Functions: gen_subset()
  
  Notes: In some cases, two different species may occur at the same coordinate, so it
  is important to consider each subset of coordinates by species before removing any points.  

## 3.add_climate_predictors_to_occurrence_df by Elizabeth--script works, but may change to fit needs of model
  Purpose: Add columns for climatic data to the large dataset of occurrences compiled for our species. Also, add 
  these columns with climate data to the absence dataset.
  
  Input: occurrence_compiled_dec2_unique_countyDupRemoved.csv
         fia_absence_compiled.csv
         climate rasterLayers from PRISM 
         (30-year normals for annual precipitation, mean annual temperature, maximum annual 
         temperature, minimum annual temperature) Additional rasterLayers can be added without much difficulty.
         Found in folder in-use_occurrence_compiled/ and PRISM data on server in /home/data/PRISM/
  
  Output: occur_prism.csv 
          (updated csv with both species occurrence data and climate data for those coordinates)
          absent_prism.csv
          (both will be placed in the in-use_occurrence_compiled folder in the Google Drive)
          Found in folder in-use_occurrence_compiled/
  
  Packages: dplyr, raster, rgdal, sp, plyr
  
  Functions: extract_PRISM (This single function will extract the appropriate climatic 
  statistics from each layer of interest. After running it, a separate step is needed to append it to the existing data frame)
  
## 4.extract_species_from_raw_df by Elizabeth--script works, but may change to fit needs of model
  Purpose: Subset by species from large occurrence pool first 
  and then at different spatial scales second
  *THIS will likely be revised; some of this now covered with removal of duplicates*
  
  Input: occurrence_compiled_dec2_unique_countyDupRemoved.csv (The the script is currently set up to read this file, but 
         this excludes any climatic values added in new columns in step 3.)
         Found in folder in-use_occurrence_compiled/
  
  Output: shapefiles of occurrences for all species of interest and of absences for the species listed in FIA
          occurrence shapefiles: "Quercus_(species epithet)"
          absence shapefiles:    "(species epithet)_ absence"
          Found in folder in-use_occurrence_compiled/species_shapefiles
  
  Packages: dplyr
  
  Functions: create_species_shp (input the species name and make a shapefile of all occurrences in a dataset)
             create_absence_shp (the absences needed a slightly different function to input the species name and 
             make a shapefile of all coordinates in which a species was absent because the coordinates were set up
             slightly differently--they hadn't undergone the same round of edits as the occurrence data.)
             
  Notes: 1) One of the species, Quercus graciliformis, underwent a slightly different process to make the
        shapefile because it needed to be grouped with Quercus canbyi. 2) The occurrence shapefiles can be 
        made fairly quickly, but the absence shapefiles can take up to an hour each. 3) Remember that absence data
        is only available for species surveyed by FIA.

## Compile_occurrencePt_datasets.R
   Purpose: outdated version of step 2?
   
## PRIMS_unzip_daily.sh
   Purpose: early version of script 0?

## extract_ITRDB.R
   Purpose: loop example from Shannon, consider removing to sagebrush folder


# 1. sagebrush folder
Within folder "sagebrush", files are numbered in order of workflow

Data used in these trials varies with the file, but should come from Shannon's OakCons/data folder on box: https://app.box.com/folder/46543046640

# Files
## 1sage_set_sdm_model_run.R
## 2sage_load_climate_data.R
## 3sage_functions.R
## 4sage_set_parameters.R
## 5sage_load_occurrance_data.R
## 6sage_prep_predictor_variables_and_run.R
## 7sage_load_proj_climate_data.R
## ITRDB_extract_workflow.R
## clim_codes.xlsx
## model_sets.xlsx
## sagebrush_sdm_models_MC.R
## sagebrush_sdm_prj_MC.R
## variable_sets_sage.xlsx

# 3. trials folder
Within folder "trials", files are usually not related, so are not meant to be run in any particular order

Data used in these trials varies with the file, but should come from the Google Drive
# Files   
## FIA_extraction_loop_example
Purpose: Early version of making a loop to extract FIA occurrence data state by state

## map_FIA_data
Purpose: Plot the FIA occurrence data for each of the species included and zoom in on the region of the country in which they are found so we can visualize the spread of FIA.

## trial_Q_similis
Purpose: Compares FIA an GBIF occurrence points where FIA data comes from most recent survey data across the lower 48 and the GBIF data comes from a hand-checked sample where occurences were run through GEOLocate to obtain more coordinates.

## trial_gbif_3x_Q_lobata.R
Purpose: Map GBIF occurrences of Quercus lobata, comparing points from different combinations of geolocated and given coordinates. The maps created here showed me that the solely GeoLocated points were more condensed, the solely given points were more dispersed and a combination of them represented a decent middling range.
