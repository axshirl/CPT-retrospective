##################################################
## Project: SFV CPT Retrospective Analysis
## Script purpose: Data Intake & Processing
## Date: 7/28/2020
## Author: Alex Shirley, axshirl
##################################################

#### Package Loads ####
package_list = c("dplyr", "tidyr", "jsonlite", 'purrr', )
packages_missing = package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(packages_missing) > 0) install.packages(packages_missing)
loaded_pkgs = lapply(package_list, require, character.only = TRUE)

####Reading in json files####
tournaments_json <- fromJSON("data/tournaments.json") %>%
  as.data.frame()

#turn json into tournament level- each record is a tournament tibble-
#have a column that is a list column of player involved, characters, placements
#function takes in a nested structure

#date read in as character
sfv_tourneys <- tournaments_json %>% 
  filter(version == 'SF5') %>%
  mutate(date = as.Date(date, format = '%d-%m-%Y')) %>%
  select(-c(version, videos, challonge, creator))
  #unnest(players)

#data before a certain date (2019? very recent) is entirely listed as UNRANKED for column 'type'
#most likely going to have to go thru ranking criteria for 2016 -> 2019 seasons & 
#basically need to make a key- How much were places worth for Type = 'premier' in 2016? 2017? etc

####Classifying tourneys####
#scraping the capcom site for tourney names + types.
#scraping is very minimal luckily

cpt_2016 <- read_html('https://capcomprotour.com/schedule/?season=2016&list_view=&lang=en-us')

df_2016 <- data.frame(
  event_year = 2016, 
  event_title = cpt_2016 %>% 
    html_nodes('.aga-list-title') %>%
    html_text(), 
  event_type = cpt_2016 %>%
    html_nodes('.tag-event') %>%
    html_text() %>%
    tail(-4) #first 4 elements are part of their filters. ez remove.
  )
View(df_2016)



