##################################################
## Project: SFV CPT Retrospective Analysis
## Script purpose: Data Intake & Processing
## Date: 7/28/2020
## Author: Alex Shirley, axshirl
##################################################

#### Package Loads ####
package_list = c("dplyr", "tidyr", "jsonlite", 'purrr', 'rvest' )
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

event_list = list()
for (i in 1:5) {
  loop_year = 2015+i
  cpt_page <- read_html(
    paste0('https://capcomprotour.com/schedule/?season=',
    loop_year, 
    '&list_view=&lang=en-us')
    )
  
  titles = cpt_page %>% 
    html_nodes('.aga-list-title') %>%
    html_text()
  cut_buttons <- cpt_page %>%
    html_nodes('.tag-event') %>%
    html_name() %>%
    `!=`('button') #lol this is gross
  
  types = cpt_page %>%
    html_nodes('.tag-event') %>%
    html_text() %>%
    .[cut_buttons]
  
  dat <- data.frame(
    event_year = loop_year, 
    event_type = types,
    event_title = titles 
  ) 
  event_list[[i]] <- dat
}

sfv_cpt = bind_rows(event_list) %>%
  nest(data = c(event_title))


#ooook. not a lot of actual code coming- gonna write up some quick findings.
#2016-
#-EVO: 1-1024/2-512/3-256/4-128/5-64/7-32/9-16/13-8
#-Premier: 1-256/2-128/3-64/4-32/5-16/7-8/9-4/13-2
#-Ranking: 1-128/2-64/3-32/4-16/5-8/7-4/9-2/13-1
#Trend- EVO = 4x Premier, Premier = 2x Ranking.

#2017-
#-EVO: 1-1000/2-700/3-500/4-400/5-320/7-250/9-200/13-160/(decreases by 30 each time until)/49-40/65-20/97-10/129-5/193-1/
#-Premier: 1-400/2-250/3-200/4-160/5-130/7-100/9-70/13-40/17-20/25-10/33-5/49-1
#-Ranking: 1-160/2-100/3-70/4-40/5-20/7-10/9-5/13-1
#sheesh. these get annoying. I think it might actually be smart to level up my scraping a bit
#what I'm thinking is essentially
# - we're already scraping each tourney name/class
# - what if we scraped each results page (for possible tourneys?)
# - would pull players AND characters AND results! this is huge...
# - NOTE- gotta be careful about how hard we hit the cpt site. 
# - I want to get data not DDOS capcom lol


