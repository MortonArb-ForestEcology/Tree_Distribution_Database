############ 4. add climate predictors to species data frames
########### 2.27.18 Elizabeth Tokarz
###########
###########


############### INPUT: species data csv in any of the three levels of certainty
#               (from script #3)
#
#               first three letters of species epithet + number representing level 
#               of certainty + _coord.csv
#               ex. Quercus arkansana at county level: ark1_coord.csv 
#                 Quercus arkansana at localized level: ark2_coord.csv
#                 Quercus arkansana at edited localized level: ark3_coord.csv
#
#
#     package: dplyr
#
#     ############### OUTPUT: above files with extra columns holding climate and
#                     environmental predictor data
#
#                     ex. ark1_cp.csv
#                         ark2_cp.csv
#                         ark3_cp.csv
