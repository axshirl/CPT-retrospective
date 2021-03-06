##################################################
## Project: SFV CPT Retrospective Analysis
## Script Purpose: Data Intake & Processing
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

#date reads in as character
sfv_tourneys <- tournaments_json %>% 
  filter(version == 'SF5') %>%
  mutate(date = as.Date(date, format = '%d-%m-%Y')) %>%
  select(-c(version, videos, challonge, creator))

#data before a certain date (2019? very recent) is entirely listed as UNRANKED for column 'type'
#meaning we need to create a system to attach points (which vary by tourney type)
#to tourneys. or full results with points

####Classifying tourneys####
#scraping the capcom site for tourney names + types.

#preallocating list for loop purposes
#normally would've written a function & used a functional 
#(we'll do that later) but this just fit with the way I was
#exploring the data at the time. 
event_list = vector(mode = 'list', length = 5)
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
sfv_cpt = bind_rows(event_list) 


#read in each results page and then start cleaning
#lotta regex coming up. Need to separate sponsor tag from player tag
#clean up Placing and change from character "1st" to numeric/int "1" 
#clean up Characters columns ("Dhalsim/Kolin") 
#note that there's a handful (7 out of ~290) results page links that
#lead to a 404- the results exist, the links are just bunk.
#we'll be dealing with these in a second

#### Appending results ####

read_Results <- function(event_results, ...) {
  #read in page, grab the table. start cleanin'
  result_page <- read_html(event_results)
  result_table <- result_page %>% html_node('.easy-table-default') %>% html_table(fill=TRUE)
  #split handle into Sponsor name & Player name columns.
  #If no sponsor, make it NA
  result_table$tag <- ifelse(str_detect(result_table$Handle, "\\|"), 
                             str_extract(result_table$Handle, 
                                         "\\|.*") %>% str_remove("\\|"), 
                             result_table$Handle)
  result_table$sponsor <- str_extract(result_table$Handle, 
                                      "^.[^|]*\\|") %>% 
    str_remove("\\|")
  #making placement into numeric instead of string (1 instead of '1st')
  result_table$placement <- str_extract(result_table$Placing, 
                                        "[:digit:]*") %>% as.numeric()
  if(event_results == 'https://capcomprotour.com/furia-tica-2017-results/'){
    #Furia Tica 2017 has a missing field in the table. this is mentioned
    #a little later in the script when we start patching up the 404s
    #but as a result, I had to go find footage of top 8 & manually enter chars
    #note that the table is top 16, but footage & results only exist for top 8
    result_table <- result_table %>% head(8) %>% select(-3) 
    result_table$characters <- list('FANG', 
                                    c('Urien', 'Rashid'), 
                                    'Vega', 
                                    'Ken', 
                                    'Laura', 
                                    'Balrog', 
                                    'Guile', 
                                    c('Guile', 'M.Bison','Ken')
    )
  } else {
    #splitting up the string for characters, i.e.
    #"Dhalsim/Kolin" should become c("Dhalsim", "kolin")
    #means this'll be a list column
    #note that we don't want to do this for that stupid furia tica
    #edge case bc there's no characters column by default
    result_table$characters <- result_table$Characters %>% str_split("\\/") 
  }
  #Add Points column if it doesn't exist. rename some cols and shrink what we return
  tourney_results <- result_table %>% 
    mutate(Points = {if("Points" %in% names(.)) Points else NA}) %>%
    select(placement, 
           sponsor,
           tag, 
           characters,
           'points' = Points
           )
  Sys.sleep(5) #adding a pause between scrapes
  return(tourney_results)
}

#There are 7 tournaments w/ result links that 404
#the results pages exist, the links are just incorrect
#so we'll replace those 7
#and of those 7, one (furia tica 2017) is missing info from 
#the results table. 

#### Fixing 404 page errors ####
#possibly() gives us whatever output runs, & if it doesn't run because
#of a 404 error, it'll give us NA. after that, we grab all the NA results
attempt_Results <- possibly(read_Results, otherwise = NA)
tourneys_with_results <- sfv_cpt %>% 
  dplyr::mutate(tourney_results = pmap(., .f = attempt_Results))
broken_link_refs <- test_output %>% filter(is.na(tourney_results)) 

#and once we have the NA result tourneys (the 7 with 404 errors)
#we just find the actual working results pages (why are these different??)
#and patch em up
manual_links <- c('https://capcomprotour.com/premier-event-socal-regionals-2016-results/', 
                  'https://capcomprotour.com/premier-tournament-dreamhack-summer-2016-results/', 
                  'https://capcomprotour.com/ranking-tournament-the-fight-2016-results/', 
                  'https://capcomprotour.com/toryuken-v-unleashed-results/', 
                  'https://capcomprotour.com/ranking-tournament-lima-salty3-at-mgtfix-results/', 
                  'https://capcomprotour.com/furia-tica-2017-results/', 
                  'https://capcomprotour.com/europe-west-2-results-mildom-bstinfexious-from-the-uk-wins/'
                  )

#replacing the broken links in the subsetted df and grabbing those results
broken_results_patch <- broken_link_refs %>%
  mutate(event_results = manual_links) %>%
  dplyr::mutate(tourney_results = pmap(., .f = attempt_Results))

#and now we use our 7 row dataframe to patch up the main dataframe-
#joining into the original dataframe and then taking the new column (from the patch)
#and using that wherever the original results column has NA
full_tourneys_with_results <- tourneys_with_results %>%
  left_join(broken_results_patch, by = c('event_year', 'event_type', 'event_title')) %>%
  mutate(tourney_results = coalesce(tourney_results.x, tourney_results.y)) %>%
  select(-tourney_results.x, -tourney_results.y, 
         -event_results.x, -event_results.y)
#WOOOOO BOY i thought i'd never see the day

#### Saving Output ####
#rds for personal use and json for the sake of having it
saveRDS(full_tourneys_with_results, 'data/full_tourneys_with_results.rds')
write(toJSON(full_tourneys_with_results, encoding = "UTF-8"), 'data/full_tourneys_with_results.json')
