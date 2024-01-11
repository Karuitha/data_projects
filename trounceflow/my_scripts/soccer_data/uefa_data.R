## Load required libraries ----

if(!require(pacman)){
  
  install.packages("pacman")
  
}

pacman::p_load(gapminder, tidyverse, rvest, ggthemes, 
               
               janitor, data.table, tidyquant, patchwork)


## Scrap champions league data ----
my_scrapped_data <- read_html("https://en.wikipedia.org/wiki/UEFA_Champions_League") %>% 
  
  html_nodes("table") %>% 
  
  html_table(fill = TRUE) %>% 
  
  .[[4]] %>% 
  
  tibble() %>% 
  
  janitor::clean_names() 


## Write the data to excel ----
fwrite(my_scrapped_data, "soccer_data.csv")

## Explore the data ----
head(my_scrapped_data)

## Plots

(
my_scrapped_data %>% 
  
  filter(title_s > 0) %>% 
  
  ggplot(mapping = aes(x = fct_reorder(club, title_s, min), y = title_s)) + 
  
  geom_col() +
  
  coord_flip() + 
  
  tidyquant::theme_tq() + 
  
  labs(y = "Titles", x = "", 
       
       title = "No of Wins in UEFA Champions League",
       
       caption = "Data source: Wkipedia") +



my_scrapped_data %>% 
  
  filter(runners_up > 0) %>% 
  
  ggplot(mapping = aes(x = fct_reorder(club, runners_up, min), y = runners_up)) + 
  
  geom_col() +
  
  coord_flip() + 
  
  tidyquant::theme_tq() + 
  
  labs(y = "Runners Up Position", x = "", 
       
       title = "No of Runners Up Position in UEFA Champions League",
       
       caption = "John Karuitha, 2021")
)


## Scrap wins by country ----

my_scrapped_data_country <- read_html("https://en.wikipedia.org/wiki/UEFA_Champions_League") %>% 
  
  html_nodes("table") %>% 
  
  html_table(fill = TRUE) %>% 
  
  .[[5]] %>% 
  
  tibble() %>% 
  
  janitor::clean_names() %>% 
  
  pivot_longer(-nation, names_to = "position", values_to = "number") %>% 
  
  mutate(nation =str_remove_all(nation, "\\[.*\\]"))



## Explore the data
my_scrapped_data_country %>% 
  
  mutate(position = factor(position, levels = c("titles", "runners_up", "total"))) %>% 
  
  ggplot(mapping = aes(x = fct_reorder(nation, number, min), y = number)) + 
  
  geom_col() + 
  
  facet_wrap(~ position) + 
  
  coord_flip() +
  
  tidyquant::theme_tq() + 
  
  labs(y = "Number", x = "", 
       
       title = "UEFA Champions League Wins by Country",
       
       caption = "Data Source: Wikipedia")


## afcon

afcon_winners <- read_html("https://en.wikipedia.org/wiki/Africa_Cup_of_Nations") %>% 
  
  html_nodes("table") %>% 
  
  html_table() %>% 
  
  .[[3]] %>% 
  
  set_names(c("edition", "year", "hosts", "blank", "champions", 
              
              "score", "runners_up", "blank_1", "third_place", 
              
              "score_ru", "fourth_place", "blank_2", "no_of_teams")) %>% 
  
  select(!starts_with("blank")) %>% 
  
  filter(edition != "Edition")

