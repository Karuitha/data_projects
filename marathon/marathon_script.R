## Load required libraries ----

if(!require(pacman)){
        
        install.packages("pacman")
        
}

pacman::p_load(tidyverse, rvest, countrycode, 
               
               data.table, glue, lubridate,
               
               readxl, tidyquant, kableExtra)

## Set theme for plots
theme_set(theme_tq())
## Scraping functions ----
scrapper <- function(x){
        
        Sys.sleep(2)
        
        read_html(x) %>% 
                
                html_nodes("table") %>% 
                
                html_table() %>% 
                
                .[[1]]
        
}

## pages ----
read_html("https://worldathletics.org/records/all-time-toplists/road-running/marathon/outdoor/men/senior?regionType=world&drop=regular&fiftyPercentRule=regular&page=1&bestResultsOnly=true&firstDay=1900-01-01&lastDay=2023-05-05") |> 
        html_nodes('.toplist-pagination:nth-child(1) .pag--show') |>
        html_text() |>
        as.numeric()

## Where is the data ----
url <- "https://www.worldathletics.org/records/all-time-toplists/road-running/marathon/outdoor/men/senior?page="
pages <- 1:48


all_web_pages <- glue::glue("{url}{pages}")
###############################################
## THIS SECTION ALLOWS YOU TO SCRAPE DATA DIRECTLY
## JUST UNCOMMENT TO REDO SCRAPPING
## Scrape the data ----
marathon_data <- lapply(all_web_pages, scrapper)

### Clean the data
final_marathon_data <- rbindlist(marathon_data) %>%

        janitor::clean_names() %>%

        select(-v1)

## I WRITE THE DATA TO EXCEL TO AVOID SCRAPPING AGAIN
## Write excel files to avoid scrapping again
# final_marathon_data %>%
# 
#         fwrite("marathon.xlsx")

########################################################       
## Read in the data to avoid scrapping again
final_marathon_data <- fread("marathon.xlsx")

head(final_marathon_data)

## Clean the data 
final_marathon_data <- final_marathon_data %>% 
        
       mutate(dob = if_else(str_detect(dob, "^\\d{4}$"), 
                            
                            paste0("1 JAN", " ", 
                                   
                            str_extract_all(dob, "^\\d{4}$")), dob)) %>% 
        
        mutate(dob = dmy(dob),
               
               date = dmy(date),
               ) %>% 
        
        mutate(age_at_race = difftime(date, dob, units = "days"),
               
               age_at_race = age_at_race / 365.25,
               
               age_at_race = str_remove(age_at_race, "days"),
               
               age_at_race = parse_number(age_at_race)) %>% 
        
        mutate(venue_country_code = str_extract_all(venue, "\\(.*\\)"),
               
               venue_country_code = str_remove_all(venue_country_code, "\\(|\\)")) %>% 
        
        mutate(venue_country_name = countrycode(venue_country_code,
                                                
                                                origin = "ioc",
                                                
                                                destination = "country.name"),
               
               venue = str_remove_all(venue, "\\(.*\\)"),
               
               date_of_race = year(date)) %>% 
        
        mutate(nat_country_name = countrycode(nat,
                                                
                                                origin = "ioc",
                                                
                                                destination = "country.name")) %>% 
        
        relocate(nat_country_name, .after = nat) %>% 
        
        mutate(mark = hms(mark)) %>% 
        
        mutate(time_posted_seconds = hour(mark) * 3600 + 
                       
                       minute(mark) * 60 + second(mark))
        


## Explore the data
## How many nationalities have won
final_marathon_data %>% 
        
        count(nat, sort = TRUE, name = "count") %>% 
        
        mutate(prop = count / sum(count) * 100) %>% 
        
        kbl(., booktabs = TRUE, caption = "Marathoner Nationality") %>% 
        
        kable_classic(full_width = FALSE)

## Venues countries where most marathons held
final_marathon_data %>% 
        
        count(venue_country_name, sort = TRUE, 
              
              name = "count") %>% 
        
        mutate(prop = count / sum(count) * 100)

## Venue cities where most marathons held
final_marathon_data %>% 
        
        count(venue, sort = TRUE, name = "count") %>% 
        
        mutate(prop = count / sum(count) * 100)

## Marathoner with the most wins 

final_marathon_data %>% 
        
        count(competitor) %>% 
        
        arrange(desc(n))

## Marathoners with best times
final_marathon_data %>% 
        
        arrange(time_posted_seconds) %>% 
        
        select(competitor, nat, venue, time_posted_seconds) %>% 
        
        mutate(time = time_posted_seconds / 3600) %>% 
        
        select(-time_posted_seconds) %>% 
        
        slice(1:10)

## Age distributions ----
## Which age group is represented the most in the data 
final_marathon_data$age_group <- cut(final_marathon_data$age_at_race, 
                                     
                                     breaks = 6)

final_marathon_data %>% 
        
        na.omit() %>% 
        
        ggplot(mapping = aes(x = age_group, 
                             
                             y = time_posted_seconds)) + 
        
        geom_col()

## Age group with the most winners
final_marathon_data %>% 
        
        filter(pos == 1) %>% 
        
        na.omit() %>% 
        
        ggplot(mapping = aes(x = age_group, 
                             
                             y = time_posted_seconds)) + 
        
        geom_col()

## DATA VISUALIZATION ----
## Winners by country
final_marathon_data %>% 
        
        count(nat_country_name, sort = TRUE, 
              
              name = "count") %>% 
        
        filter(!is.na(nat_country_name)) %>% 
        
        slice(1:10) %>% 
        
        ggplot(mapping = aes(x = fct_reorder(nat_country_name, count, max), 
                             
                             y = count)) + 
        
        geom_col() + 
        
        labs(x = "Country", y = "", title = "Marathon Winners by Country") + 
        
        theme(plot.title = element_text(face = "bold", 
                                        
                                        size = 20))


## Age distribution 
final_marathon_data %>% 
        
        ggplot(mapping = aes(x = age_at_race)) +
        
        geom_histogram(col = "black", binwidth = 1)


## Times posted distribution 
final_marathon_data %>% 
        
        ggplot(mapping = aes(x = time_posted_seconds)) + 
        
        geom_density(fill = "skyblue")

## Distribution of times posted
final_marathon_data %>% 
        
        mutate(time = time_posted_seconds / 3600) %>% 
        
        ggplot(mapping = aes(x = time)) + 
        
        geom_histogram(col = "black")

## Distiribution of times posted
final_marathon_data %>% 
        
        mutate(time = time_posted_seconds / 3600,
               
               
               continent = countrycode(sourcevar = nat_country_name,
                        
                                       origin = "country.name",
                                       
                                       destination = "continent"
                                       
                                       )
               ) %>% 
        
        ggplot(mapping = aes(x = time)) + 
        
        geom_density(col = "black", alpha = 0.5)

## Times posted versus age
final_marathon_data %>% 
        
        ggplot(mapping = aes(x = age_at_race %>% round(2), 
                             
                             y = time_posted_seconds)) + 
        
        geom_hex(show.legend = FALSE, alpha = 0.5) + 
        
        geom_density2d() + 
        
        geom_point(shape = ".") +
        
        scale_fill_gradient(low = "green", high = "red")

## Visualizing marathon maps
map_data("world") %>% 
        
        mutate(code = countrycode(sourcevar = region, origin = "country.name",
                                  
                                  destination = "ioc")) %>% 
        
        left_join(final_marathon_data %>% 
                          
                          filter(pos %in% c(1, 2, 3)) %>% 
                          
                          count(nat) %>% 
                          
                          tibble(), by = c("code" = "nat")) %>% 
        
        ggplot(mapping = aes(x = long, y = lat, group = group, fill = n)) + 
        
        geom_polygon(show.legend = FALSE) +
        
        scale_fill_gradient(low = "green", high = "red") + 
        
        labs(title = "Top 3 Positions by Country in Olympics Marathons",
             
             caption = "Countries with Grey Highlight Have No data")

