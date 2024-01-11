##############################################################
# Intro ----

## Scrapping the Internet for text data 
### John Karuitha
### Target website: University of Amsterdam, Amsterdam Center for the Study of the 
### Golden Age
### Web Address: 
### https://www.vondel.humanities.uva.nl/ecartico/persons/index.php?subtask=browse

###############################################################
# Load required packages ----
## Install pacman for package management if not already installed.
## Ensure you have internet connection to install

if(!require(pacman)){
        install.packages("pacman")
}

## Use pacman to install absent packages and load them for use 

pacman::p_load(tidyverse, rvest)

###############################################################

# Get the required addresses ---- 
## The pages required span multiple pages, 23 pages. We have to scrap 1117 columns of data. 
## By the time you are reading this, the number of entries may have increased.
## The web addresses take this format.
## address{pagenumber}, for example. 
## https://www.vondel.humanities.uva.nl/ecartico/persons/index.php?subtask=browse&field=surname&strtchar=A&page=3

addresses <- paste0("https://www.vondel.humanities.uva.nl/ecartico/persons/index.php?subtask=browse&field=surname&strtchar=A&page=",1:23)

################################################################
# Write a function to scrap data ----

 scrapper <- function(url){
         
         ## load the required rvest library
         library(rvest)
         
         ## delay the scrapper 2 seconds after each page
         Sys.sleep(2)
         
         ## Scrap the text data 
         read_html(url) %>%
                 
                 ## capture the nodes- use selectorGadget to see node name
                 html_nodes("#setwidth li a") %>% 
                 
                 ## extraxt the text
                 html_text()
         
 }

################################################################
# Scrap the data ----

## Loop the scrapper over the many pages
amsterdam_data <- sapply(addresses, scrapper)

## View the scraped data
amsterdam_data

## Convert the scrapped data into a dataframe

amsterdam_data_tibble <- amsterdam_data %>% 
        
        ## Convert to tibble
        tibble() %>% 
        
        ## Unnest the columns
        unnest(col = c(.)) %>% 
        
        ## Set the column names
        set_names("person") %>% 
        
        ## Separate years from persons
        mutate(year = str_extract(person, "\\(.*\\s+-\\s+.*\\)$")) %>% 
        
        ## Remove years from the names 
        mutate(person = str_remove_all(person, "\\(.*\\s+-\\s+.*\\)$")) %>% 
        
        ## Remove brackets from years 
        mutate(year = str_remove_all(year, "\\(|\\)$"))

###############################################THE END

