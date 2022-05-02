##################################################
## IMPORTANT
## Challenges that make code to break
# Inconsistent formats xls, xlsx(see year 2017)
# Some broken excel files in the data folder.
# Inconsistent file naming. eg march.xlsx, march2021.xlsx (see year 2021 relative to other years)
# Data stacked up in one column that we sorted yesterday.

## Load required packages ----
if(!require(pacman)){
        
        install.packages("pacman")
}


pacman::p_load(tidyverse, readxl, data.table)

## List files recursively, that is, including files in sub-folders
my_files <- list.files(path = "Data", 
                       
                       pattern = "*.xlsx", 
                       
                       recursive = TRUE, 
                       
                       full.names = TRUE)
## Print all files
my_files

## Allow for parallel computing ----
doParallel::registerDoParallel()

## Uncomment this code to see number of variables in each sheet
## Check the number of variables in each excel workbook 
# my_vars <- map_dfr(my_files, function(x){
#         
#         readxl::read_xlsx(x, col_names = FALSE) %>% 
#                 
#                 ncol() %>% 
#                 
#                 tibble() %>% 
#                 
#                 set_names("cols") %>% 
#                 
#                 mutate(month = x) %>% 
#                 
#                 relocate(month) %>% 
#                 
#                 arrange(desc(month))
# })


## Write function to read in the data ----
## The function takes in file name (x) and country as inputs

get_data <- function(x, country){
        
        readxl::read_xlsx(x, col_names = FALSE) %>% 
                
                ## Convert all columns to character to allow binding rows
                mutate(across(.cols = everything(), as.character)) %>% 
                
                ## Get rows for date from folder and country
                mutate(date = x, country = country) %>% 
                
                ## Select relevant columns, including F
                select(date, country, `...6`) %>% 
                
                ## Rename the column F with the values to a proper name
                rename(values = `...6`) %>% 
                
                ## Extract only columns with 5 letters
                mutate(figs = str_extract(values, "^[0-9]{5}$")) %>% 
                
                ## Drop columns with NAs in column F
                drop_na(figs) %>% 
                
                select(-figs) %>% 
                
                ## Convert values to numeric
                mutate(values = parse_number(values)) %>% 
                
                ## Extract month anf year from file names
                mutate(year = str_extract(date, '[0-9]{4}'),
                       
                       month = str_extract(date, '[a-zA-Z]*\\.xlsx$'),
                       
                       month = str_remove(month, '\\.xlsx$')
                       
                       ) %>% 
                
                ## Remove the redundant column containing file name
                ## Given we have extracted year and month from it.
                select(-date) %>% 
                
                relocate(month, year) %>% 
                
                extract(values, into = c("clouds", "wind_direction", 
                                         
                                         "speed"), 
                        
                        regex = "(\\d{1})(\\d{2})(\\d{2})",
                        
                        remove = FALSE) %>% 
                
                ## Extract values for cloud, wind direction and speed
                mutate(##Convert the extracted variables to numerics
                       clouds = parse_number(clouds),
                       
                       speed = parse_number(speed),
                       
                       wind_direction = parse_number(wind_direction),
                       
                       ## Convert knots to m/s
                       wind_speed = speed * 0.514) %>% 
                
                ## Arrange data well
                relocate(wind_direction, .after = clouds)
                                              
                                              
}


## Loop the function above over all files in the datqset
final_joe_data <- map_dfr(my_files, get_data, country = "Zambia")

## I am wrtiting the excel file to avoid running the
## files reading process as it takes a lot of time
fwrite(final_joe_data, "final_joe_data.csv")

## Finally I average the wind speed data by year, month 
## Them make it wide 
## I read in the data
final_joe_wide_data <- fread("final_joe_data.csv") %>% 
        
        ## Select relevant variables
        select(month, year, wind_speed) %>% 
        
        ## Group data by month and year for making summaries
        group_by(month, year) %>% 
        
        ## Take average of wind speed
        summarise(avg_wind_speed = mean(wind_speed, na.rm = TRUE)) %>% 
        
        ## Convert months to a standard format
        mutate(month = str_to_sentence(month),
               
               month = case_when(
                       
                       str_detect(month, '^[Aa]p.*') ~ "April",
                       
                       str_detect(month, '^[Jj]ul.*') ~ "July",
                       
                       str_detect(month, '^[Jj]un.*') ~ "June",
                       
                       str_detect(month, '^[Mm]ar.*') ~ "Mar",
                       
                       str_detect(month, '^[Nn]ov.*') ~ "Nov",
                       
                       str_detect(month, '^[Ss]ep.*') ~ "Sep",
                       
                       TRUE ~ month
               )
               
               ) %>% 
        
        ## make the data wide
        pivot_wider(names_from = month, 
                    
                    values_from = avg_wind_speed) %>% 
        
        ## Arrange data by year
        arrange(year)

## Write wide data in a csv file containg wind speed only
fwrite(final_joe_wide_data, "final_joe_wide_data.csv")


## I repeat the same exercise to get long data ----
final_joe_long_data <- fread("final_joe_data.csv") %>% 
        
        ## Select relevant variables
        select(month, year, wind_speed) %>% 
        
        ## Group data by month and year for making summaries
        group_by(month, year) %>% 
        
        ## Take average of wind speed
        summarise(avg_wind_speed = mean(wind_speed, na.rm = TRUE)) %>% 
        
        ## Convert months to a standard format
        mutate(month = str_to_sentence(month),
               
               month = case_when(
                       
                       str_detect(month, '^[Aa]p.*') ~ "April",
                       
                       str_detect(month, '^[Jj]ul.*') ~ "July",
                       
                       str_detect(month, '^[Jj]un.*') ~ "June",
                       
                       str_detect(month, '^[Mm]ar.*') ~ "Mar",
                       
                       str_detect(month, '^[Nn]ov.*') ~ "Nov",
                       
                       str_detect(month, '^[Ss]ep.*') ~ "Sep",
                       
                       TRUE ~ month
               )
               
        )



fwrite(final_joe_long_data, "final_joe_long_data.csv")

# get_data("Data/2019/Sep/Sept.xlsx", country = "Zambia")
        
        