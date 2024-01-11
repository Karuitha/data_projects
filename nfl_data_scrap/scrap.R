## Load required packages ----
if(!require(pacman)){
  install.packages("pacman")
}

pacman::p_load(tidyverse, glue, rvest, janitor)

## Create file paths ----
positions <- c("Offence", "Defense")
years <- 2022:2010

source_data <- expand.grid(positions, years) %>% 
  
  tibble() %>% 
  
  set_names(c("position", "year"))

## Build a scrapper ----
scrapper <- function(position, year){
  
  read_html(glue('https://www.spotrac.com/nfl/contracts/sort-value/{position}/{year}')) %>% 
    
    html_nodes("table") %>% 
    
    html_table() %>% 
    
    .[[1]] %>% 
    
    clean_names() %>% 
    
    mutate(year = year)
  
}

## Scrap from 2010
nfl_data_raw <- map2_dfr(source_data$position, source_data$year, scrapper)

## Explore and clean the data ----
nfl_data_clean <- nfl_data_raw %>% 
  
  mutate(player = str_remove_all(player, "\\|")) %>% 
  
  separate(player, into = c("last_name", "name", "position", "contracted"), sep = "\\s{2,}") %>% 
  
  select(-last_name) %>% 
  
  mutate(across(.cols = value:practical_gteed, .fns = parse_number))



