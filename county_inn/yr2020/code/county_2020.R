## A hotel in town data analysis 
### Load packages 
library(tidyverse)
library(kableExtra)
library(readxl)
library(lubridate)

## Set theme
theme_set(ggthemes::theme_clean())

## Load packages
path.expand("data")
my_files <- list.files(path = ".", pattern = "*.xlsx")

my_county_data <- map_dfr(my_files, readxl::read_xlsx) %>% 
        
        janitor::clean_names() %>% 
        
        filter(!is.na(item_code)) %>% 
        
        mutate(start = ymd(start),
               
               end = ymd(end),
               
               trans_month = month(start),
               
               trans_date = day(start)) %>% 
        
        mutate(item_name = case_when(item_name == "TUSKER LARGER" ~ "TUSKER LAGER",
                                     
                                     TRUE ~ item_name)) %>% 
        
        mutate(week_day = weekdays(start))

########################################################################
View(my_county_data)

########################################################################
my_county_data %>% 
        
        filter(item_group == "BEERS") %>% 
        
        group_by(item_name) %>% 
        
        summarise(total_units_sold = sum(qnty)) %>% 
        
        arrange(desc(total_units_sold))
#######################################################################
my_county_data %>% 
        
        filter(item_group == "WHISKY") %>% 
        
        group_by(item_name) %>% 
        
        summarise(total_units_sold = sum(qnty)) %>% 
        
        arrange(desc(total_units_sold))

#######################################################################
my_county_data %>% 
        
        filter(item_group == "MAIN DISHES") %>% 
        
        group_by(item_name) %>% 
        
        summarise(total_units_sold = sum(qnty)) %>% 
        
        arrange(desc(total_units_sold))

#######################################################################
my_county_data %>% 
        
        filter(item_name %in% c("TUSKER LAGER", "WHITE CAP LAGER", 
                                
                                "GUINESS BIG", "BALOZI")) %>% 
        
        ggplot(mapping = aes(x = start, y = qnty, col = item_name)) + 
        
        geom_line(lwd = 3)
######################################################################
my_county_data %>% 
        
        ggplot(mapping = aes(x = item_source, y = total, 
                             
                             fill = item_group, col = item_group)) + 
        
        geom_col()



#########################################################################
## Sales by day of week

day_week_sales <- my_county_data %>% 
        
        group_by(item_group, week_day, item_source) %>% 
        
        summarise(total_sales = sum(total)) 

day_week_sales %>% arrange(desc(total_sales))
        
day_week_sales %>%  
        
        filter(item_group == "MAIN DISHES") %>% 
        
        ggplot(mapping = aes(x = fct_reorder(week_day, total_sales, max), 
                                         
                y = total_sales, fill = week_day)) +
        
        geom_col(show.legend = FALSE)



