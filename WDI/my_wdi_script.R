## Load libraries
library(tidyverse)
library(data.table)
library(janitor)
library(countrycode)

## Load data ----
my_data <- read_csv("73087209-3838-4d29-b77c-46d6dc001466_Data.csv", na = "..") %>% 
        
        clean_names() %>% 
        
        mutate(continent = countrycode(sourcevar = country_code,
                                       
                                       origin = "iso3c", 
                                       
                                       destination = "continent")) %>% 
        
        mutate(continent = case_when(
                
                country_code == "CHI" ~ "Europe", 
                
                country_code == "XKX" ~ "Europe",
                
                TRUE ~ continent
                
                
        ))


head(my_data)


my_data %>% 
        
        filter(time %in% c(2002, 2007, 2012, 2017)) %>% 
        
        na.omit() %>% 
        
        ggplot(mapping = aes(x = gdp_per_capita_constant_2015_us_ny_gdp_pcap_kd, 
                             
                             y = life_expectancy_at_birth_total_years_sp_dyn_le00_in,
                             
                             color = continent)) + 
        
        geom_point(shape = 1, show.legend = FALSE, 
                   
                   size = 5, stroke = 2, alpha = 0.5) + 
        
        facet_wrap(~ time) + 
        
        scale_color_manual(values = c("red", "blue", "green", 
                                     
                                     "purple", "black")) 




my_data %>% 
        
        filter(time %in% c(2002, 2007, 2012, 2017)) %>% 
        
        filter(!complete.cases(.)) %>% 
        
        pull(country_name) %>% 
        
        unique()


my_data %>% 
        
        filter(str_detect(country_code, "XKX"))
