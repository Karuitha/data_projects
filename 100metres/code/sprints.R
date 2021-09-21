## 100 metres all records ----
library(tidyverse)
library(rvest)
library(janitor)
library(lubridate)
library(skimr)
library(countrycode)

pages <- 1:224


url <- "https://www.worldathletics.org/records/all-time-toplists/sprints/100-metres/outdoor/men/senior?regionType=world&timing=electronic&windReading=regular&page="

url_2 <- "&bestResultsOnly=false&firstDay=1900-01-01&lastDay=2021-09-20"

## Full url example
full_url <- paste0(url, "1", url_2)

## Scrapping function 
scrapper <- function(x){
        
        Sys.sleep(4)
        
        read_html(paste0(url, x, url_2)) %>% 
                
                html_nodes("table") %>% 
                
                html_table()
        
}



# my_100_dash_data <- pages %>% map_dfr(~ scrapper(.x))

# write_csv(my_100_dash_data, "my_100_dash_data.csv")

my_100_dash_data <- read_csv("data/my_100_dash_data.csv") %>% 
        
        clean_names() %>% 
        
        select(-x8) %>% 
        
        mutate(dob = lubridate::dmy(dob), 
               
               date = lubridate::dmy(date), 
               
               age_days = (date - dob),
               
               age_years = as.numeric(age_days / 365.25),
               
               venue_country_code = str_extract_all(venue, "\\([A-Z]*\\)"), 
               
               venue_country_code = str_remove_all(venue_country_code, "\\(|\\)")) %>% 
        
        mutate(venue_country_name = countrycode(venue_country_code, 
                                                
                                                origin = "ioc", 
                                                
                                                destination = "country.name"))



## Check out NAs

subset(my_100_dash_data, subset = is.na(dob))

my_100_dash_data[is.na(my_100_dash_data$dob), ]  

## athletes by country
my_100_dash_data %>% 
        
        count(nat, sort = TRUE)


## Check out age structure of the fastest athletes. 

my_100_dash_data %>% 
        
        skim(age_years)

## Youngest athlete 
my_100_dash_data[which.min(my_100_dash_data$age_years), ]

## Oldest athlete 
my_100_dash_data[which.max(my_100_dash_data$age_years), ]

## Graph of the age structure

my_100_dash_data %>% 
        
        ggplot(mapping = aes(x = age_years)) + 
        
        geom_histogram(col = "black", fill = "skyblue", binwidth = 1) + 
        
        ggthemes::theme_economist() + 
        
        labs(x = "Age in Years", y = "Count", 
             
             title = "Histogram of Age of Top 100 Metres Male Sprinters")

## Graph of best times 
my_100_dash_data %>% 
        
        ggplot(mapping = aes(x = mark)) + 
        
        geom_histogram(col = "black", fill = "skyblue", binwidth = 0.01) + 
        
        ggthemes::theme_economist() + 
        
        labs(x = "Mark", y = "Count", 
             
             title = "Histogram of Best Times of Top 100 Metres Male Sprinters")

## Evoluation of Usain Bolt

my_100_dash_data %>% 
        
        filter(competitor == "Usain BOLT") %>% 
        
        ggplot(mapping = aes(x = factor(year(date)), y = mark)) + 
        
        geom_boxplot(mapping = aes(fill = factor(year(date)))) + 
        
        geom_point() + 
        
        ggthemes::theme_economist() +
        
        theme(legend.position = "none") 


## Times by venues
my_100_dash_data %>%
        
        na.omit() %>% 
        
        group_by(venue_country) %>% 
        
        skim_without_charts(mark) %>% 
        
        select(-complete_rate, -n_missing) %>% 
        
        arrange(numeric.mean)
        
