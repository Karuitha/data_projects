## JOHN KARUITHA

## WEB SCRAPING PROJECT
########################################

## IN THIS PROJECT I ILLUSTRATE HOW TO SCRAP MULTIPLE WEB PAGES

## Specifically, I scrap data for the Spanish la Liga for 2009-2021 seasons.

## Note that for web scraping, you must have an active internet connection. 

#########################################
## @knitr install_packages
#########################################
## Again, ensure you are CONNECTED to the internet to install packages

if(!require(pacman)){
        
        install.packages("pacman")
}

## Load required packages

pacman::p_load(tidyverse, rvest, janitor, png, magick)

#########################################
## @knitr scrap_one_wikipedia
#########################################
## PART 1: SCRAPPING ONE WEBPAGE

## La Liga top 3 clubs and top scorers 1929-2021.

## Data is from the following url

url2 <- "https://en.wikipedia.org/wiki/List_of_Spanish_football_champions"

## We now scrap the data

## Read the html

full_league_1929_2020 <- read_html(url2) %>% 
        
        ## Capture the nodes for tables
        html_nodes("table") %>% 
        
        ## Capture the tables
        html_table() %>% 
        
        ## Capture the third table in the series
        .[[3]] %>% 
        
        ## Remove square brackets in names
        set_names(names(.) %>% str_remove_all("\\[|\\]|\\d")) %>% 
        
        ## Clean names by removing spaces
        janitor::clean_names() %>%
        
        ## Remove redundant columns
        select(-starts_with("x")) %>% 
        
        ## Clean the team names
        mutate(winners = str_remove_all(winners, "\\(\\d+\\)|\\*")) %>% 
        
        ## make a nice table
        knitr::kable()


##########################
## @knitr top1019292021    
##########################

full_league_1929_2020 %>% 

## Get first ten rows
head(10)

#########################################
## @knitr scrap_one_sky
#########################################
## PART 2: SCRAPPING ONE WEB PAGE

## Initial trial with one url for 2020/2021 season

## Define the url

url <- "https://www.skysports.com/la-liga-table/2020"

## Read the html
read_html(url) %>% 
        
        ## Capture the nodes for tables
        html_nodes("table") %>% 
        
        ## Capture the tables
        html_table() %>% 
        
        ## Capture the first table in the series
        .[[1]] %>%
        
        ## rename the position column
        rename(pos = `#`) %>% 
        
        ## Clean the column names 
        janitor::clean_names() %>% 
        
        ## Remove redundant last 6 column
        select(-last_6) %>% 
        
        ## make a nice table
        knitr::kable(caption = "La Liga Standings 2020-2021 Season")

#########################################
## @knitr scrap_many_sky_function
#########################################
## PART 3: SCRAP MULTIPLE WEB PAGES

# Scrap the 12 links of the results from 2009 to 2021.

# NB: Every page starts with "https://www.skysports.com/la-liga-table/"

# Then for every respective year, the year is appended.

# For instance for 2020, the address is "https://www.skysports.com/la-liga-table/2020"

## Write a scrapping function

scrapper <- function(url){
        
        ## Add delay after scrapping each page
        Sys.sleep(2)
        
        ## Read the html
        read_html(url) %>% 
                
                ## Capture the nodes for tables
                html_nodes("table") %>% 
                
                ## Capture the tables
                html_table() %>% 
                
                ## Capture the first table in the series
                .[[1]] %>%
                
                ## rename the position column
                rename(pos = `#`) %>% 
                
                ## Clean the column names 
                janitor::clean_names() %>% 
        
                ## Remove redundant last 6 column
                select(-last_6)
}

##########################################
## @knitr get_many_address
##########################################
## Capture all the urls for years 2009-2021

many_urls <- paste0("https://www.skysports.com/la-liga-table/", 2020:2009)

many_urls


########################################
## @knitr scrap_many_sky
#######################################
## Run a loop over all the web pages 

la_liga_09_2020 <- lapply(many_urls, scrapper)

## Give each table a name corresponding to the year

names(la_liga_09_2020) <- paste0("year", 2020:2009)

##########################################
## @knitr top_teams_2009_2020
##########################################
## Get the top team in each year

top_teams_2009_20 <- sapply(la_liga_09_2020, "[", 1, "team") %>% 
        
        ## Unlist to make one table
        unlist() %>% 
        
        ## Coerce to tibble
        tibble() %>% 
        
        ## Rename column
        rename(team = ".")

################################################################################
## @knitr graph_top_teams_2009_20
################################################################################
## Capture the top teams
top_teams_2009_20 %>% 
        
        ## make a table
        
        table() %>% 
        
        ## Convert table into tibble
        
        tibble() %>% 
        
        ## Rename the tibble column
        
        rename(no = ".") %>% 
        
        ## Add team names
        
        mutate(team = c("Atletico Madrid", "Barcelona", "Real madrid")) %>% 
        
        ## Convert teams to factors
        
        mutate(team = factor(team)) %>% 
        
        ## Relocate team column to first position
        
        relocate(team) %>% 
        
        ## Plot the data
        
        ggplot(aes(x = fct_reorder(team, no), y = no, fill = team)) + 
        
        ## Add a geom
        
        geom_col(col = "black", show.legend = FALSE) + 
        
        ## Add text labels
        
        geom_text(aes(label = no), nudge_y = 0.3) +
        
        ## Add x and y labels and a title, subtitle
        
        labs(x = "Teams", y = "", title = "LA LIGA ANALYSIS", 
             
             subtitle = "La Liga Winners, 2009-2021", 
             
             caption = "Developed by John Karuitha using R and the ggplot2 package") + 
        
        ## Select bar colors
        
        scale_fill_manual(values = c("red", "blue", "white")) + 
        
        ## Add a pleasant theme
        
        ggthemes::theme_clean()

############################################
## @knitr full_listing
############################################

## Appendix: The full list of data scrapped

la_liga_09_2020


############################################
## @knitr full_listing_1929_2021
############################################

## Appendix: The full list of data scrapped

full_league_1929_2020

??monochromeR::generate_palette
monochromeR::generate_palette(
        "blue",
        modification = 'blend',
        blend_colour = 'black',
        n_colours = 3,
        view_palette = TRUE
)
