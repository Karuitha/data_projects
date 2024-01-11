##THE BEER DEBATE PROJECT ----
# Objective- to get and clean data on beer ratings ----
# Set working directory ----
# Load required packages ----
library(tidyverse)
library(rvest)
library(ggthemes)
library(plotly)
library(GGally)

##################################################################
## Scrapper functions ----
##################################################################
scraper_1 <- function(url, n_rows){
  
  beer_data_main <- read_html(url) %>% 
    
    html_nodes(".hr_bottom_light b") %>% 
    
    html_text() %>% 
    
    tibble() %>% 
    
    mutate(key = rep(1:3, n_rows))
  
  beer <- data.frame(beer = filter(beer_data_main, key == 1))
  votes <- data.frame(votes = filter(beer_data_main, key == 2))
  avg <- data.frame(avg = filter(beer_data_main, key == 3))
  
  bind_cols(beer, votes, avg) %>% 
    
    select(-ends_with("key")) %>% 
    
    set_names(c("beer", "votes", "average"))
}

## Scrapper 2

scrapper_2 <- function(url){
  
  read_html(url) %>% 
    
    html_nodes("table") %>% 
    
    html_table() %>% 
    
    .[[1]] %>% 
    
    filter(!is.na(X1)) %>% 
    
    select(-X1, -X3, -X4, -X5)
}



##################################################################
## Full scrapping function ----




##################################################################
## The scrapping process
## Top beer companies
##################################################################
top_250 <- scraper_1("https://www.beeradvocate.com/beer/top-rated/", 
          
          n_rows = 250)

####################################
beer_category <- read_html("https://www.beeradvocate.com/beer/top-rated/") %>% 
  
  html_nodes("#ba-content a~ a") %>% 
  
  html_text() %>% 
  
  tibble() %>% 
  
  set_names("type")

####################################
full_table <- scrapper_2("https://www.beeradvocate.com/beer/top-rated/")

full_table_top_250 <- top_250 %>% bind_cols(full_table) %>% 
  
  bind_cols(beer_category) %>% 
  
  mutate(X2 = str_remove_all(X2, beer)) %>% 
  
  mutate(X2 = str_remove_all(X2, type)) %>% 
  
  mutate(alcohol_perc = str_extract(X2, "\\d{1,2}\\.\\d{2}%$"), 
         
         X2 = str_remove_all(X2, "\\|\\s?\\d{1,2}\\.\\d{2}%$"),
         
         alcohol_perc = parse_number(alcohol_perc),
         
         category = category) %>% 
  
  rename(brewer = X2)

###################################################################################

my_comprehensive_scrapper <- function(url, n_rows, category){
  
## get the Beers, number of votes and average rating ----
  
  beer_data_main <- read_html(url) %>% 
    
    html_nodes(".hr_bottom_light b") %>% 
    
    html_text() %>% 
    
    tibble() %>% 
    
    mutate(key = rep(1:3, n_rows))
  
  beer <- data.frame(beer = filter(beer_data_main, key == 1))
  
  votes <- data.frame(votes = filter(beer_data_main, key == 2))
  
  avg <- data.frame(avg = filter(beer_data_main, key == 3))
  
  long_beers <- bind_cols(beer, votes, avg) %>% 
    
    select(-ends_with("key")) %>% 
    
    set_names(c("beer", "votes", "average"))
  
  
## Get the beer category- like stout ----
  
  beer_category <- read_html(url) %>% 
    
    html_nodes("#ba-content a~ a") %>% 
    
    html_text() %>% 
    
    tibble() %>% 
    
    set_names("type")
  
## Get the full table  ----
  
  full_table <- read_html(url) %>% 
    
    html_nodes("table") %>% 
    
    html_table() %>% 
    
    .[[1]] %>% 
    
    filter(!is.na(X1)) %>% 
    
    select(-X1, -X3, -X4, -X5)
  
## Combine the three tables to form one table ----
  
long_beers %>% bind_cols(full_table) %>% 
    
    bind_cols(beer_category) %>% 
    
    mutate(X2 = str_remove_all(X2, beer)) %>% 
    
    mutate(X2 = str_remove_all(X2, type)) %>% 
    
    mutate(alcohol_perc = str_extract(X2, "\\d{1,2}\\.\\d{2}%$"), 
           
           X2 = str_remove_all(X2, "\\|\\s?\\d{1,2}\\.\\d{2}%$"),
           
           alcohol_perc = parse_number(alcohol_perc),
           
           category = "top_250") %>% 
    
    rename(brewer = X2)
  
}

###################################################################################
## Add beer category light, medium, strong----
full_beer_data$category <- case_when(
  full_beer_data$alcohol_percent <= 8 ~ "lite",
  full_beer_data$alcohol_percent > 8 & 
    full_beer_data$alcohol_percent <= 16 ~ "medium",
  full_beer_data$alcohol_percent > 16 ~ "strong")

ggplotly(full_beer_data %>% ggplot(aes(y = average_ratings, 
                              x = category, fill = category)) + geom_boxplot())

######################################################################################
## Add column for subtype and clean dataset ----
full_beer_data$subtype <- 
  ifelse(str_detect(full_beer_data$beer, 
  "\\s*\\-\\s*\\W*"), str_extract_all(full_beer_data$beer, 
  "\\s*\\-\\s*\\w*\\s*\\w*\\s*\\w*"), NA)


########################################################################################
## Feature engineer moren----
# Types ----
full_beer_data$type <- factor(full_beer_data$type) 
                              #levels = names(sort(table(full_beer_data$type))))

class(full_beer_data$type)



##########################################################################################
##Visualize the data ----
median_na <- function(x){median(x, na.rm = TRUE)}

ggplotly(full_table_top_250 %>% 
           
           group_by(type) %>% 
           
           filter(n() >= 7) %>% 
           
           ungroup %>% 
           
  ggplot(aes(x = reorder(type, alcohol_perc, median), 
             
  y = alcohol_perc, fill = type)) + 
    
    geom_boxplot() + 
    
  theme_hc() + theme(legend.position = "none") + 
    
  theme(axis.text.x = element_text(angle = 90)) + 
    
  labs(y = "Alcohol Percentage", x = "Type of Beer", 
       title = "THE BEER DEBATE",
       subtitle = "A Visual Guide to Choosing Your Poison", 
       caption = "John Karuitha (2020),
       Data Source: Beer Advocate- Your Go-To Resource for Beer
       Website: https://www.beeradvocate.com, 
       **Respect Beer"))


##Alcohol content vs ratings ----
ggplotly(full_table_top_250 %>% group_by(type) %>% 
           
           ggplot(aes(x = alcohol_perc, 
                      
                              y = average, 
                      
                              color = type)) + 
           
                              geom_point(alpha = 0.5))


full_table_top_250 %>% 
  
  group_by(type) %>% 
  
  summarise(n(), mean(alcohol_perc, na.rm = TRUE), 
            
            mean(average, na.rm = TRUE)) %>% 
  
  arrange(`n()`)



full_table_top_250 %>% 
  
  ggplot(aes(x = fct_reorder(type, alcohol_perc, max), y = alcohol_perc, 
             
             fill = type)) + 
  
  geom_boxplot(show.legend = FALSE) + 
  
  ggthemes::theme_clean() +
  
  theme(axis.text.x = element_text(angle = 90)) +
  
  labs(y = "Alcohol Percentage", x = "Type of Beer", 
       title = "THE BEER DEBATE",
       subtitle = "A Visual Guide to Choosing Your Poison", 
       caption = "John Karuitha (2020),
       Data Source: Beer Advocate- Your Go-To Resource for Beer
       Website: https://www.beeradvocate.com, 
       **Respect Beer")



                    