## Load required packages ----
if(!require(pacman)){
  install.packages("pacman")
}

pacman::p_load(tidyverse, cronR, rvest)

## Scrap ----
read_html("https://en.wikipedia.org/wiki/Mike_Tyson") %>% 
  
  html_nodes("table") %>% 
  
  html_table() %>% 
  
  .[[4]]
