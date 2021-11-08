## 100 metres all records ----

library(tufte)
library(knitr)
library(tidyverse)
library(rvest)
library(janitor)
library(lubridate)
library(skimr)
library(countrycode)
library(kableExtra)
library(Amelia)
library(mgcv)
library(stargazer)

##########################################################################################
## @knitr webscrap

pages <- 1:225

url <- "https://www.worldathletics.org/records/all-time-toplists/sprints/100-metres/outdoor/men/senior?regionType=world&timing=electronic&windReading=regular&page="

url_2 <- "&bestResultsOnly=false&firstDay=1900-01-01&lastDay=2021-09-20"

##########################################################################################
## Scrapping function 

scrapper <- function(x){
        
        Sys.sleep(2)
        
        read_html(paste0(url, x, url_2)) %>% 
                
                html_nodes("table") %>% 
                
                html_table()
        
}

##########################################################################################
#my_100_dash_data <- pages %>% map_dfr(~ scrapper(.x))

#write_csv(my_100_dash_data, "my_100_dash_data.csv")

##########################################################################################
## @knitr data_cleaning

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
                                                
                                                destination = "country.name")) %>% 
        
        mutate(venue_country_name = case_when(venue_country_code == "AHO" ~ "Netherlands Antilles",
                                              
                                              venue_country_code == "FRG" ~ "Germany",
                                              
                                              venue_country_code == "GDR" ~ "Germany",
                                              
                                              venue_country_code == "MAC" ~ "Macau",
                                              
                                              venue_country_code == "TCH" ~ "Czechia",
                                              
                                              venue_country_code == "TKS" ~ "Turks and Caicos Islands",
                                              
                                              venue_country_code == "URS" ~ "Russia",
                                              
                                              TRUE ~ venue_country_name
                                              
                                              ))

##########################################################################################
## @knitr nas

Amelia::missmap(my_100_dash_data)

sapply(my_100_dash_data, is.na) %>% 
        
        colSums() %>% 
        
        tibble(variables = names(my_100_dash_data), missing = .) %>% 
        
        arrange(desc(missing)) %>% 
        
        mutate(prop_percent = missing / nrow(my_100_dash_data) * 100) %>% 
        
        head(10) %>% 
        
        kbl(., booktabs = TRUE, caption = "Missing Data") %>% 
        
        kable_classic(full_width = FALSE, latex_option = "hold_position")

##########################################################################################
## @knitr most_athletes_country

my_100_dash_data %>% 
        
        count(nat, sort = TRUE) %>% 
        
        head(10) %>% 
        
        kbl(., booktabs = TRUE, caption = "NUmber of Athletes by Nationality") %>% 
        
        kable_classic(full_width = FALSE, latex_option = "hold_position")

my_100_dash_data %>% 
        
        select(competitor, nat) %>% 
        
        filter(!duplicated(.)) %>% 
        
        count(nat) %>% 
        
        arrange(desc(n)) %>% 
        
        head(10) %>% 
        
        kbl(., booktabs = TRUE, caption = "Number of Distict Atletes by Nationality") %>% 
        
        kable_classic(full_width = FALSE, latex_option = "hold_position")

##########################################################################################
## @knitr top_athletes_best_times

my_100_dash_data %>% 
        
        select(competitor, mark) %>% 
        
        arrange(mark) %>% 
        
        head(10) %>% 
        
        kbl(., booktabs = TRUE, caption = "Top 10 Best Times in 100 Meters Dash") %>% 
        
        kable_classic(full_width = FALSE, latex_option = "hold_position")

##########################################################################################
## @knitr top_all_time_best

my_100_dash_data %>% 
        
        select(competitor, mark) %>% 
        
        group_by(competitor) %>% 
        
        arrange(mark) %>% 
        
        slice(1) %>% 
        
        ungroup() %>% 
        
        arrange(mark) %>% 
        
        head(10) %>% 
        
        kbl(., booktabs = TRUE, caption = "Top 10 100 Meters Athletes") %>% 
        
        kable_classic(full_width = FALSE, latex_option = "hold_position")

##########################################################################################
## @knitr top_athlete_appearances

my_100_dash_data %>% 
        
        count(competitor, sort = TRUE) %>% 
        
        head(10) %>% 
        
        kbl(., booktabs = TRUE, caption = "Most Appearances (Races) in the Top Sprinters List") %>% 
        
        kable_classic(full_width = FALSE, latex_option = "hold_position")

##########################################################################################
## @knitr age_structure_times

my_100_dash_data %>% 
        
        skim_without_charts(age_years, mark) %>% 
        
        kbl(., booktabs = TRUE, caption = "Summary Statistics for Athletes Age and Mark") %>% 
        
        kable_classic(full_width = FALSE, latex_option = "hold_position")

##########################################################################################
## Youngest athlete 

my_100_dash_data[which.min(my_100_dash_data$age_years), ] %>% 
        
        kbl(., booktabs = TRUE, caption = "Youngest Athlete in the Dataset") %>% 
        
        kable_classic(full_width = FALSE, latex_option = "hold_position")

##########################################################################################
## Oldest athlete 

my_100_dash_data[which.max(my_100_dash_data$age_years), ] %>% 
        
        kbl(., booktabs = TRUE, caption = "Oldest Athlete in the Dataset") %>% 
        
        kable_classic(full_width = FALSE, latex_option = "hold_position")

##########################################################################################
## @knitr data_vis
## Graph of the age structure

my_100_dash_data %>% 
        
        ggplot(mapping = aes(x = age_years)) + 
        
        geom_histogram(col = "black", fill = "skyblue", binwidth = 1) + 
        
        ggthemes::theme_economist() + 
        
        labs(x = "Age in Years", y = "Count", 
             
             title = "Histogram of Age of Top 100 Metres Male Sprinters")

###########################################
## Graph of best times

my_100_dash_data %>% 
        
        ggplot(mapping = aes(x = mark)) + 
        
        geom_histogram(col = "black", fill = "skyblue", binwidth = 0.01) + 
        
        ggthemes::theme_economist() + 
        
        labs(x = "Mark", y = "Count", 
             
             title = "Histogram of Best Times of Top 100 Metres Male Sprinters")

###############################################
## Evoluation of Usain Bolt

my_100_dash_data %>% 
        
        filter(competitor == "Usain BOLT") %>% 
        
        ggplot(mapping = aes(x = factor(year(date)), y = mark)) + 
        
        geom_boxplot(mapping = aes(fill = factor(year(date)))) + 
        
        geom_point() + 
        
        ggthemes::theme_economist() +
        
        theme(legend.position = "none") +
        
        labs(x = "Year", y = "Mark/Time in Seconds", 
             
             title = "Usain  Bolt 100 Meters Races History 2007-2017")

#####################################################
## @knitr time_by_venues
## Minimum Times by venues

my_100_dash_data %>%
        
        na.omit() %>% 
        
        group_by(venue_country_name) %>% 
        
        skim_without_charts(mark) %>% 
        
        select(-complete_rate, -n_missing) %>% 
        
        arrange(numeric.p0) %>% 
        
        head(10)


#######################################################
## Improvements over time- best times per year

my_100_dash_data %>% 
        
        group_by(year(date)) %>% 
        
        summarise(year_record = min(mark)) %>% 
        
        rename(year = `year(date)`) %>% 
        
        ggplot(mapping = aes(x = year, y = year_record)) +
        
        geom_line(col = "blue") +
        
        labs(x = "Year", y = "Mark/Best Time", 
             
             title = "Trend in 100 Meters Men Best Times") + 
        
        ggthemes::theme_economist()

################################################################
## Mean and Median age of athletes over time

my_100_dash_data %>% 
        
        group_by(year(date)) %>% 
        
        summarise(mean_age = mean(age_years, na.rm = TRUE),
                  
                  median_age = median(age_years, na.rm = TRUE)) %>% 
        
        rename(year = `year(date)`) %>% 
        
        pivot_longer(-year, names_to = "metric", values_to = "age") %>% 
        
        ggplot(mapping = aes(x = year, y = age, col = metric)) + 
        
        geom_line() + 
        
        ggthemes::theme_economist() +
        
        scale_colour_manual(values = c("red", "blue"))


##########################################################################################
## Number of races versus times posted

races_vs_time <- my_100_dash_data %>% 
        
        group_by(competitor, year(date)) %>% 
        
        rename(year = `year(date)`) %>% 
        
        summarise(races = n(),
                  
                  age = age_years,
                  
                  best_time = min(mark),
                  
                  median_time = median(mark),
                  
                  mean_time = mean(mark), 
                  
                  max_time = max(mark))

head(races_vs_time)

###################################################################

races_vs_time %>% 
        
        pivot_longer(-c("competitor", "year", "races", "age"),
                     
                     names_to = "perf", values_to = "time") %>% 
        
        ggplot(mapping = aes(x = races, y = time)) + 
        
        geom_hex(alpha = 0.5) +
        
        scale_fill_gradient(low = "grey", high = "red") +
        
        geom_point(shape = ".") + 
        
        geom_density_2d() + 
        
        geom_smooth(col = "green", lty = "dashed") + 
        
        labs(x = "Races", y = "Best Time", 
             
             title = "No of Races per Year vs Worst/ Max Times",
             
             caption = "John Karuitha, 2021") + 
        
        facet_wrap(~ perf) + 
        
        ggthemes::theme_clean()

###################################################################

races_lm <- lm(best_time ~ age + races, 
               
               data = races_vs_time)

summary(races_lm)

races_gam <- gam(best_time ~ s(age) + s(races), 
                 
                 data = races_vs_time,
                 
                 family = gaussian)

summary(races_gam)


stargazer::stargazer(races_lm, races_gam)
