## Load the libraries ----
library(tidyverse)
library(data.table)
library(tabulizer)

##############################################################
## Read in the data ----
my_files <- list.files(path = ".", pattern = "*.xlsx")

## Function for reading in the data ----
my_chile_function <- function(x, my_sheet){
        
        readxl::read_xlsx(x, sheet = my_sheet) %>% 
                
                janitor::clean_names() %>% 
                
                pivot_longer(cols = -c("source", "type"),
                             
                             names_to = "period", 
                             
                             values_to = "amount") %>% 
                
                mutate(my_month = str_extract(period, "[A-Za-z]*"),
                       
                       
                       my_month = str_to_sentence(my_month),
                       
                       
                       year = str_extract(period, "[0-9]+")) %>% 
                
                select(-period) %>% 
                
                relocate(my_month, year) %>% 
                
                mutate(my_month = case_when(
                        
                        my_month %in% c("Dec","Dic") ~ "December",
                        
                        my_month %in% c("Jun", "Junio") ~ "June",
                        
                        my_month %in% c("Mar", "Marzo") ~ "March",
                        
                        my_month %in% c("Sep", "Sept") ~ "September",
                        
                        TRUE ~ my_month
                        
                        
                )) %>% 
                
                filter(!duplicated(.)) %>% 
                
                filter(!str_detect(source, "Deuda*")) %>% 
                
                mutate(type = case_when(
                        
                        str_detect(type, "Total$") ~ "Deuda Total",
                        
                        str_detect(type, "[Ii]nterna$") ~ "Deuda Interna",
                        
                        TRUE ~ "Deuda Externa"
                        
                )) %>% 
                
                filter(source != "Fuente: Dipres.") %>% 
                
                mutate(amount = amount * 1000000)
        
}

#############################################################
my_raw_data_chile <- map_dfr(my_files, 
                             
                             my_chile_function, my_sheet = "mone") %>% 
        
        arrange(year, my_month)

my_raw_data_chile_2 <- map_dfr(my_files, my_chile_function, my_sheet = "acre")

############################################################
readxl::read_xlsx("march2019.xlsx", sheet = 2) %>% 
        
        janitor::clean_names() %>% 
        
        pivot_longer(cols = -c("source", "type"),
                     
                     names_to = "period", 
                     
                     values_to = "amount") %>% 
        
        mutate(my_month = str_extract(period, "[A-Za-z]*"),
               
               
               my_month = str_to_sentence(my_month),
               
               
               year = str_extract(period, "[0-9]+")) %>% 
        
        select(-period) %>% 
        
        relocate(year, my_month)%>% 
        
        mutate(my_month = case_when(
                
                my_month %in% c("Dec","Dic") ~ "December",
                
                my_month %in% c("Jun", "Junio") ~ "June",
                
                my_month %in% c("Mar", "Marzo") ~ "March",
                
                my_month %in% c("Sep", "Sept") ~ "September",
                
                TRUE ~ my_month
                
                
        )) %>% 
        
        filter(!duplicated(.)) %>% 
        
        filter(!str_detect(source, "Deuda*")) %>% 
        
        mutate(type = case_when(
                
                str_detect(type, "Total$") ~ "Deuda Total",
                
                str_detect(type, "[Ii]nterna$") ~ "Deuda Interna",
                
                TRUE ~ "Deuda Externa"
                
        )) %>% 
        
        pivot_wider(names_from = "type", values_from = "amount") %>% 
        
        janitor::clean_names()

## make the data wide for totals ----

my_raw_data_chile %>% 
        
        filter(type == "Deuda Total") %>% 
        
        group_by(year, my_month, source, type) %>% 
        
        summarise(amount = sum(amount))

## Check duplicated  ----   

# my_raw_data_chile %>% 
#         
# dplyr::group_by(year, my_month, type) %>%
#         
#         dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#         
#         dplyr::filter(n > 1L) 


## my_sheets
sapply(my_files, readxl::excel_sheets)

## attempt to extract tables
tabulizer::extract_tables("june2014.pdf")

## The data ----
my_raw_data_chile %>%
        
        distinct() %>% 
        
        pivot_wider(names_from = "type",
                    
                    values_from = "amount",
                    
                    values_fn = ~ min(.x, na.rm = TRUE)) %>% 

        janitor::clean_names() %>% 
        
        mutate(year = as.numeric(year)) %>% 
        
        mutate(year = if_else(is.na(year), 2017, year)) %>% 
        
        mutate(deuda_total = if_else(is.na(deuda_total), 
                       
                       deuda_interna + deuda_externa, deuda_total)) %>% 
        
        mutate(period = glue::glue("01 {my_month}, {year}")) %>% 
        
        relocate(period) %>% 
#####################################
        
        fwrite(., "final_output/final_data.csv")
        
        
        
## The acre data

my_acre_fn <- function(x, my_sheet){
        
readxl::read_xlsx(x, sheet = my_sheet) %>% 
        
        janitor::clean_names() %>% 
                
        pivot_longer(-c("source", "type"),
                     
                     names_to = "period",
                     
                     values_to = "amount") %>% 
                
                mutate(amount = amount * 1000000) %>% 
        
        pivot_wider(names_from = "type",
                    
                    values_from = "amount",
                    
                    values_fn = ~ median(.x, na.rm = TRUE)) %>% 
        
        janitor::clean_names() %>% 
        
        separate(period, into = c("my_month", "year"), sep = "_")  %>% 
                
                filter(!str_detect(source, "[Dd]euda.*")) %>% 
                
                mutate(my_month = case_when(
                        
                        my_month %in% c("dec","dic", "december") ~ "December",
                        
                        my_month %in% c("jun", "junio", "june") ~ "June",
                        
                        my_month %in% c("mar", "marzo", "march") ~ "March",
                        
                        my_month %in% c("sep", "sept") ~ "September",
                        
                        TRUE ~ my_month
                        
                        
                )) %>% 
                
                filter(!duplicated(.))
        
}       


final_acre_data <- map_dfr(my_files, my_acre_fn, my_sheet = "acre") %>% 
        
        fwrite(., "final_output/final_acre_data.xlsx")



## Refine the acre data by removing duplicates ----
read_csv("final_output/final_acre_data_to_send.csv") %>% 
        
        distinct() %>% 
        
        fwrite(., "final_output/final_acre_data_to_send.xlsx")

###############################
my_chile_function("september2013.xlsx", 
                  
                  my_sheet = "mone") %>% 
        
        filter(year == 2013, 
               
               my_month == "September")
