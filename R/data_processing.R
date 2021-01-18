##################################################
## Project: SFV CPT Retrospective Analysis
## Script purpose: Data Intake & Processing
## Date: 7/28/2020
## Author: Alex Shirley, axshirl
##################################################

#### Package Loads ####
package_list = c("dplyr", "tidyr", "jsonlite", 'purrr', 'rvest', 'stringr')
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

event_list = list()
#we'll need somewhere to keep our dataframes as we build them
for (i in 1:5) {
  loop_year = 2015+i
  cpt_page <- read_html(
    paste0('https://capcomprotour.com/schedule/?season=',
    loop_year, 
    '&list_view=&lang=en-us')
    )
  #extracting titles, event types (i.e. 'Premier',) and links to results
  titles <- cpt_page %>% 
    html_nodes('.aga-list-title') %>%
    html_text()
  cut_buttons <- cpt_page %>%
    html_nodes('.tag-event') %>%
    html_name() %>%
    `!=`('button') #lol this is gross. "everything that happens in R is a function call" 
  #treating != as `!=`() 
  
  types <- cpt_page %>%
    html_nodes('.tag-event') %>%
    html_text() %>%
    .[cut_buttons]
  
  result_links <- cpt_page %>%
    html_nodes('.btn:nth-child(1)') %>%
    html_attr('href') 
  if(loop_year==2018){
    result_links <- result_links %>%
      append('https://capcomprotour.com/twfighter-major-2018-results/', after=29)
  } #This sucks. In 2018 there's a unique case
  #of a tournament whose results page isn't actually linked on the overall page
  #and while I spent so much time searching for an elegant solution
  #to make this just give me an NA in place of the link...
  #the link actually exists. it's just not referenced on the main page
  #so the only way to add it is either referencing thru another page or manually.
  dat <- data.frame(
    event_year = loop_year, 
    event_type = types,
    event_title = titles, 
    event_results = result_links
  ) #wrapping it all in a df
  event_list[[i]] <- dat
  #store that in a list, so that we have list
  #elements for each year of tourneys
  
}

sfv_cpt = bind_rows(event_list) #%>%
  #nest(data = c(event_title, event_results))

#read in each results page and then start cleaning
#lotta regex coming up. Need to separate sponsor tag from player tag
#clean up Placing and change from character "1st" to numeric/int "1" 
#clean up Characters columns ("Dhalsim/Kolin") maybe make it wider?
#ie character1 "Dhalsim" character2 "Kolin" 
#which would mean NAs for single main players
result_page <- read_html(sfv_cpt$event_results[87])
result_table <- result_page %>% html_node('.easy-table-default') %>% html_table()
result_table$tag <- ifelse(str_detect(result_table$Handle, "\\|"), 
                           str_extract(result_table$Handle, 
                                       "\\|.*") %>% str_remove("\\|"), 
                           result_table$Handle)
result_table$sponsor <- str_extract(result_table$Handle, 
                                    "^.[^|]*\\|") %>% 
  str_remove("\\|")
result_table$Placing <- str_extract(result_table$Placing, 
                                    "[:digit:]*") %>% as.numeric()
tourney_results <- result_table %>% select('placing' = Placing, 
                                           sponsor,
                                           tag, 
                                           "characters" = Characters, 
                                           'points' = Points
                                           ) 


#what I'm thinking is essentially
# - we're already scraping each tourney name/class
# - what if we scraped each results page (for possible tourneys?)
# - would pull players AND characters AND results! this is huge...
# - NOTE- gotta be careful about how hard we hit the cpt site. 
# - I want to get data not DDOS capcom lol