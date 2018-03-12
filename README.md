# Tree_Distribution_Database
Repository for curating occurrence and predictor data of tree distributions

All scripts are located in folder scripts

# scripts folder
Within folder "scripts", files have been numbered in the order they are to be used in the workflow.

 Occurrence data for our oak species of interest has been downloaded prior to its use in our scripts. 
   None of the data comes from API currently, but we hope to update these file paths when API becomes available, we hope.
   
# Workflow   
## 1.lower48_FIA_oak_extraction
  Purpose: FIA data comes in unique form, where individual trees are not directly linked with their
  coordinates. Here we first cycle through the tree occurrence data from 48 states and extract all
  occurrences labeled as a species of interest.
  Input: TREE.csv files for lower 48 states from FIA datamart, PLOT.csv file from FIA datamart
  Output: single csv containing FIA occurrence data for 28 species of interest
  Packages:
  Functions:
  Notes: 1) This output does not include the coordinates of surveyed plots that did not 
  contain the species of interest. Consider altering this script if decide to use 
  empty plots as absence data. 2) This script may need to be updated if FIA API becomes available.
  3) Keep in mind that the coordinates used here by FIA may differ from the actual location of 
  occurrence, up to a mile, due to "data fuzzing". 4) Only six of our 28 species of interest were 
  observed in the most recent FIA surveys, though 13 of the 28 have been assigned species codes, 
  which suggests that other species have been observed during different survey years. Several of 
  the 28 species are shrubs and would not be expected to be recorded in the surveys anyway.
  Species represented in FIA: Quercus engelmannii, Q. graciliformis, Q. laceyi, Q. lobata, 
  Q. oglethorpensis, Q. similis

## 2.compile_occurrence_point_datasets
  Purpose: Using the FIA data compiled in step 1, and occurrence data from other sources 
  (gbif, consortium, idigbio, bonap, A. Hipp, exsitu, natureserve, redlist, usdaplants, other),
  we will make a larger stacked data frame containing all the occurrence data we will need to make
  our model. Column names will be made uniform.
  Input: all csv files from in_use_occurrence_raw folder in Google Drive
  Output: single csv containing uniform occurrence data for 28 species of interest from several sources
  Column names: dataset, basis, source, genus, species, year, state, county, locality, lat, long,
  uncert_m, issue, species_no
  Packages: dplyr, rgbif, ridigbio, data.table, tidyr, lubridate
  Functions: gen_subset ()
  Notes: 


## 3.extract_species_from_raw_df
  Purpose: Subset by species from large occurrence pool first 
  and then at different spatial scales second
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

## 4.add_climate_predictors_to_species_df
  Purpose: Using any of the species-specific csvs created in step 3, columns for climatic data
  of the location of the occurrence will be appended, thereby creating a new file in which 
  coordinates will be associated with local climatic statistics.
  Input: species-specific csv from step 3 & climate rasterLayers from PRISM 
  (30-year normals for annual precipitation, mean annual temperature, maximum annual 
  temperature, minimum annual temperature) Additional rasterLayers can be added without much difficulty.
  Output: updated csv with species occurrence data and climate data for those areas
  Packages: dplyr, raster, rgdal, sp
  Functions: extract_PRISM (This single function will extract the appropriate climatic 
  statistics from each layer of interest and append it to the existing data frame)
