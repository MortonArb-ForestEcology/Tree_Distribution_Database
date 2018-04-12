Updated: 12 April 2018

# Tree_Distribution_Database
Repository for curating occurrence and predictor data of tree distributions

All scripts are located in folder "scripts"
Examples and attempts to use similar scripts with data that have not been refined are in folder "trials"
The "sagebrush" folder contains scripts used in Shannon's sagebrush distribution modeling project, which can serve as an example for our project.
We will describe the scripts folder first because it is most relevant.

# 2. scripts folder
Within folder "scripts", files have been numbered in the order they are to be used in the workflow.
Data files used in these scripts are on the Google Drive "Distributions_TreeSpecies" and the scripts 
should contain references to their proper locations

 Occurrence data for our oak species of interest has been downloaded prior to its use in our scripts. 
   None of the data comes from API currently, but we hope to update these file paths when API becomes available, we hope.
   
# Workflow   
## 	1.0.set_workingdirectory
  Purpose: Customize the file paths depending on who is running the scripts and which computers they are using.
  
  Input: Run if-else script on the computer you will be using. The script will read your Sys.info()
  
  Output: File path abbreviations stored as global values in your R console
  
  Packages: None
  
## 1.1.lower48_FIA_oak_extraction
  Purpose: FIA data comes in a unique form, where individual trees are not directly linked with their
  coordinates. Here we first cycle through the tree occurrence data from 48 states and extract all
  occurrences labeled as a species of interest. The trees are not linked with their coordinates until step 2.
  
  Input: TREE.csv files for lower 48 states from FIA datamart
  
  Output: single csv containing FIA occurrence data for 28 species of interest, without coordinates; fia_tree_raw.csv
  
  Packages: none
  
  Functions: fia_extract (This function cycles through each state TREE.csv, one at a time, removes all trees marked as not living, and then   goes through all the remaining occurrences, checking for each of our 13 species of interest with FIA codes, one at a time. If it finds a 
  match, it pulls the occurrence and saves it to a new data frame. As it goes through each state and finds more occurrences, the data frame 
  grows until it reaches its final size.)
  
  Notes: 1) This output will not lead to the later inclusion the coordinates of surveyed plots 
  that did not contain the species of interest. Consider altering this script by also finding 
  coordinates in step 1.1 if decide to use empty plots as absence data. 2) This script may need 
  to be updated if FIA API becomes available.
  3) Keep in mind that the coordinates used here by FIA may differ from the actual location of 
  occurrence, up to a mile, due to "data fuzzing". 4) Only six of our 28 species of interest were 
  observed in the most recent FIA surveys, though 13 of the 28 have been assigned species codes, 
  which suggests that other species have been observed during different survey years. Several of 
  the 28 species are shrubs and would not be expected to be recorded in the surveys anyway.
  Species represented by positive occurrences in FIA: Quercus engelmannii, Q. graciliformis, 
  Q. laceyi, Q. lobata, Q. oglethorpensis, Q. similis

## 1.2.prep_GBIF_for_GeoLocate
  Purpose: STEP ONE--After downloading GBIF occurrence data from website in Darwin Core format, several occurrences
  have locality information, such as state, county and a description of where it was found, but lack coordinates. 
  Tulane's GEOLocate online application finds coordinates of places based on a description of state, county 
  and other locality information. In an effort to augment our occurrence datasets, we edited the Darwin Core 
  columns to fit the format necessary to use the GEOLocate application, creating a CSV to load into the web application.
  The CSV we download from the application after it has reassigned coordinates to the occurrences (gbif_DC_post-georef.csv)
  will be used in step 2 of this script. STEP TWO--Further revision was done to the geo-referenced coordinates to reinstate
  coordinates that were issueless
  
  Input: STEP ONE--gbif_DarwinCore_edit.csv
         STEP TWO--gbif_DC_post-georef.csv
  
  Output: STEP ONE--gbif_DC_georef.csv
          STEP TWO--gbif_DC_post-georef_revised.csv
  
  Packages: dplyr, rgbif, tidyr, data.table
  
  Functions: extract_state (Search in the state column for NAs and in the locality column for a state name or abbreviation. If a row has an    NA for state and a state listed in the locality, then write in that state's name in the state column.)
  
   extract_county (Search in the county column for NAs and in the locality column for the words "County", "county" or the abbreviation "Co." If any of the above are found, then create a new data frame "find_cou" where the locality column is split by that word. If a row has an NA for county and corresponds to a split column in find_cou, then write in the characters from the first part of the splut column into the original county column.)
  
  Notes: After running this script, the resulting CSV must be uploaded into a separate online application and further changed. 
  Running our csv through GEOLocate takes a significant amount of time and the completed result, which can be saved with a 
  code must be redownloaded into a new CSV that will be renamed as gbif_DC_post-georef.csv. Some of the existing coordinates 
  may also be affected by running GEOLocate, so a second supplementary script (1.3) will help check for errors.
  
## 2.1.compile_occurrence_point_datasets
  Purpose: Using the FIA and GBIF data compiled in step 1.1 and 1.2, as well as occurrence data 
  from other sources (gbif, consortium, idigbio, bonap, A. Hipp, exsitu, natureserve, redlist, 
  usdaplants, other), we will make a larger stacked data frame containing all the occurrence data 
  we will need to make our model. Column names will be made uniform.
  
  Input: all csv files from in_use_occurrence_raw folder in Google Drive, plus FIA translation files*
         gbif_DC_post-georef_revised.csv
         consortium_raw.csv
         idigbio_raw.csv
         fia_tree_raw.csv
         fia_plot_raw.csv*
         fia_species_raw.csv*
         fia_county_raw.csv*
         target_species_list.csv (to add missing columns to each dataset)
  
  Output: standardized_col_compiled.csv
          gbif_compiled.csv
          consortium_compiled.csv
          idigbio_compiled.csv
          fia_compiled.csv
          fia_absence_compiled.csv (absence data)
          occurrence_compiled_dec2.csv in in-use_occurrence_compiled folder on Google Drive
              containing uniform occurrence data for 28 species of interest from several sources
              To be used in script 2.2 below.
  
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

## 2.2.subset_occurrence_point_data
  Purpose: After compiling all the occurrences into one dataset in the above script, we must remove 
  duplicate occurrences, first overall, and then including county centroids.
  
  Input: occurrence_compiled_dec2.csv in in-use_occurrence_compiled folder on Google Drive
  
  Output: occurrence_compiled_dec2_unique.csv (no duplicates)
          occurrence_compiled_dec2_unique_countyDupMarked.csv (county centroids occurring in counties with other occurrences are marked)
          occurrence_compiled_dec2_unique_countyDupRemoved.csv (marked counties from above are removed)
          occurrence_compiled_dec2_unique_countyDupRemoved_acceptedDistMarked.csv (exlcuding counties that are not in the "accepted" range)
  
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

## 3.add_climate_predictors_to_occurrence_df
  Purpose: Add columns for climatic data to the occurrences compiled for our species
  
  Input: occurrence_compiled_dec2_unique_countyDupRemoved.csv
         fia_absence_compiled_first12.csv
         fia_absence_compiled_thirteenth.csv
         climate rasterLayers from PRISM 
         (30-year normals for annual precipitation, mean annual temperature, maximum annual 
         temperature, minimum annual temperature) Additional rasterLayers can be added without much difficulty.
  
  Output: updated csv with species occurrence data and climate data for those areas
  
  Packages: dplyr, raster, rgdal, sp
  
  Functions: extract_PRISM (This single function will extract the appropriate climatic 
  statistics from each layer of interest and append it to the existing data frame)
  
## 4.extract_species_from_raw_df
  Purpose: Subset by species from large occurrence pool first 
  and then at different spatial scales second
  *THIS will likely be revised; some of this now covered with removal of duplicates*
  
  Input: large occurrence csv from step 2
  
  Output: several smaller csvs, depending on preference of modeller
  
  Packages: dplyr
  
  Functions: extract_county_one ()
             extract_localized_two ()
             write_loc2_csv ()             
 
  Notes: 1) Subsetting by species is made especially easy with the use of the species_no code.
  2) Subsetting can be done further at three different levels of spatial scale. 
  ONE: county level--this large-scale picture is useful for mapping an upper limit of a 
  species distribution. A comparison between this map and a federal species occurrence by
  county map, from USDA plants, BONAP or NatureServe, can quickly highlight where occurrence 
  points may have been mistaken. Some occurence data lack coordinates, but include county.
  TWO: locality level--this subset removes occurrences that are only specified to the county 
  level and includes all occurrence labeled with coordinates, including those found using 
  GeoReference in step 2.
  THREE: revised locality level--this subset starts with the subset from level two and further 
  edits out points in which we have a lower confidence. For example, some coordinates that are 
  given are associated with certain issues in the 'issue' column which may indicate an error. 
  It is recommended to use this subset of occurrences for the final model.

## Compile_occurrencePt_datasets.R
   Purpose: outdated version of step 2?

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
