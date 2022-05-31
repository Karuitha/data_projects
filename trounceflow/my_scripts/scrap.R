library(quantmod)
library(tidyverse)


google <- getSymbols("GOOG", auto.assign = FALSE)

head(google)
tail(google)

### My data analysis
google %>% 
        
        coredata() %>% 
        
        data.frame() %>% 
        
        ggplot(mapping = aes(x = GOOG.Volume, y = GOOG.Adjusted)) +
        
        geom_point() + 
        
        ggthemes::theme_fivethirtyeight()

## My thesis
Ad(google)
Hi(google)
Lo(google)
Cl(google)

## 
my_dt_data <- data.table::data.table(lab = sample(letters, 10000, TRUE),
                                     
                                     x = sample(10000, 10000, TRUE),
                                     
                                     y = sample(LETTERS, 10000, TRUE))


library(data.table)
my_dt_data[x >= 1]

my_dt_data[x >= 1 & y %chin% c("L", "X")]


sample(10000, 10^6, TRUE)

head(my_dt_data)
my_dt_data[1:10, .(x)]
my_dt_data[1:10, x]
my_dt_data[, "x"]


my_dt_data[lab == "a", mean(x)]
my_dt_data[lab == "d", .N]
my_dt_data[1:(.N - 5), .N]


my_dt_data[y == "L", list(mean_x = mean(x))]

my_dt_data[y == "L", .(mean_x = mean(x))]

my_dt_data[y =="L", list(mean = mean(x), median = median(x), 
                         
                         count = .N), by = lab]


## AFCON  in data.table
library(rvest)

afcon_winners <- read_html("https://www.britannica.com/sports/African-Cup-of-Nations") %>% 
        
        html_nodes("table") %>% 
        
        html_table() %>% 
        
        .[[1]] %>% 
        
        filter(!str_detect(year, "^\\*F*")) %>% 
        
        mutate(year = str_remove_all(year, "\\*")) %>% 
        
        mutate(year = as.numeric(year)) %>% 
        
        pivot_longer(-year, names_to = "position", 
                     
                     values_to = "country") %>% 
        
        mutate(country = case_when(country == "Zaire" ~ "DRC",
                                   
                                   str_detect(country, "Kinshasa") ~ "DRC",
                                   
                                   TRUE ~ country))


afcon_winners %>% 
        
        filter(position == "winner") %>% 
        
        count(country, name = "no_of_wins") %>% 
        
        arrange(desc(no_of_wins)) %>% 
        
        ggplot(aes(x = fct_reorder(country, no_of_wins, max), y = no_of_wins, fill = country)) + 
        
        geom_col(show.legend = FALSE) + 
        
        tidyquant::theme_tq() + 
        
        coord_flip()
        
        

??dplyr::count


## Scrapping job 
## Scrap all addresses from address below

read_html("https://www.chamberorganizer.com/members/directory/search_bootstrap.php?org_id=CARR") %>% 
        
        html_element(".name-plate")

??html_attrs


