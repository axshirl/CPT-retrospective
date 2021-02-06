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

#binding together the lists we just created 
#& replacing the broken link for SCR 2016
#(results page exists, but link referenced on the CPT Schedule page has a typo)
#same problem exists for Dreamhack summer 2016... please let this be all!
sfv_cpt = bind_rows(event_list) %>%
  mutate(event_results = replace(event_results, 
                                 event_year == 2016 & event_title == 'SoCal Regionals', 
                                 'https://capcomprotour.com/premier-event-socal-regionals-2016-results/'
                                 )
         ) %>%
  mutate(event_results = replace(event_results, 
                                 event_year == 2016 & event_title == 'Dreamhack Summer', 
                                 'https://capcomprotour.com/premier-tournament-dreamhack-summer-2016-results/'
                                 )
         )
#read in each results page and then start cleaning
#lotta regex coming up. Need to separate sponsor tag from player tag
#clean up Placing and change from character "1st" to numeric/int "1" 
#clean up Characters columns ("Dhalsim/Kolin") 

read_Results <- function(event_results, ...) {
  cat(paste0(event_results, '\n'))
  result_page <- read_html(event_results)
  result_table <- result_page %>% html_node('.easy-table-default') %>% html_table(fill=TRUE)
  result_table$tag <- ifelse(str_detect(result_table$Handle, "\\|"), 
                             str_extract(result_table$Handle, 
                                         "\\|.*") %>% str_remove("\\|"), 
                             result_table$Handle)
  result_table$sponsor <- str_extract(result_table$Handle, 
                                      "^.[^|]*\\|") %>% 
    str_remove("\\|")
  result_table$placement <- str_extract(result_table$Placing, 
                                        "[:digit:]*") %>% as.numeric()
  result_table$characters <- result_table$Characters %>% str_split("\\/") 
  #splitting up the string for characters, i.e.
  #"Dhalsim/Kolin" should become c("Dhalsim", "kolin")
  #this makes this column a list, but as of rn I don't mind that
  #if worst comes to worst I guess we make it wide but I don't know
  #that i want to do that
  
  tourney_results <- result_table %>% 
    mutate(Points = {if("Points" %in% names(.)) Points else NA}) %>%
    select(placement, 
           sponsor,
           tag, 
           characters,
           'points' = Points
           )
  Sys.sleep(5) #adding a long pause between scrapes
  cat(paste(event_results, 'exists and was read in.\n'))
  return(tourney_results)
}

#testing rn: some CPT final events have an empty points column 
#and this makes html_table() sad
#404 issue in first 25?
test_output <- sfv_cpt %>% 
  dplyr::mutate(tourney_results = pmap(., .f = read_Results))

#issue links:
#SC2016
#https://capcomprotour.com/premier-tournament-dreamhack-summer-2016-2/?lang=en-us
#What if this -2 is actually somehow an issue- it's supposed to externally be -results?

position_results <- sfv_cpt$event_results %>% str_detect(., '-2/')
sfv_cpt[position_results,]
#no, that's not the case. Maybe just see which ones say 'results' in them?
results_strings <- !sfv_cpt$event_results %>% str_detect(., 'results')
sfv_cpt[results_strings,]
#Still not the case! I'm going to write up a patch for Dreamhack & see if our issue persists.

#ANOTHER ONE.
#https://capcomprotour.com/ranking-tournament-fight-2016-results/?lang=en-us
#this is very annoying. I think the move is gonna be to find -all- of the broken links
#and then maybe like, make a dataframe for just those & 
#hotswap the broken links out w/ manually replaced working links? 
#I doubt this problem ends in 2016.
#https://capcomprotour.com/ranking-tournament-the-fight-2016-results/
#This one they just missed a "the" 


