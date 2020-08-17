##################################################
## Project: SFV CPT Retrospective Analysis
## Script purpose: Data Intake & Processing
## Date: 7/28/2020
## Author: Alex Shirley, axshirl
##################################################

#### Package Loads ####
package_list = c("dplyr", "tidyr", "jsonlite")
packages_missing = package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(packages_missing) > 0) install.packages(packages_missing)
loaded_pkgs = lapply(package_list, require, character.only = TRUE)

####Reading in json files####
players_json <- fromJSON(file="data/players.json", simplify=TRUE) %>%
  as.data.frame()

#turn json into tournament level- each record is a tournament tibble- have a column that is a list column of player involved, characters, placements
#function takes in a nested structure