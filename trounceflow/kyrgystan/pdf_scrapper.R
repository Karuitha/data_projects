library(tidyverse)
library(tabulizer)
library(readxl)
library(data.table)
library(janitor)


tabulizer::extract_tables("pdf_2021.pdf") %>% 
        
        as.data.frame() %>% 
        
        filter(X1 == "ДВУСТОРОННИЕ ЛЬГОТНЫЕ") %>% 
        
        set_names(c("creditor", "January", "February", "March", 
                    
                    "April", "May", "June", "July", "August", 
                    
                    "September", "October", "November", "December")) %>% 
        
        mutate(creditor = if_else(
                
                creditor == "ДВУСТОРОННИЕ ЛЬГОТНЫЕ", 
                
                "Bilateral Preferential",
                
                creditor
                
        )) %>% 
        
        mutate(year = 2021) %>% 
        
        relocate(year, .after = creditor)



## Make a scrapping function ----
bilateral_pref_scrapper_pdf <- function(file, year){
        
        library(tidyverse)
        library(tabulizer)
        library(readxl)
        
        tabulizer::extract_tables(file) %>% 
                
                as.data.frame() %>% 
                
                filter(X1 %in% c("ДВУСТОРОННИЕ ЛЬГОТНЫЕ", 
                                 
                                 "ДВУСТОРОННИЕ НЕЛЬГОТНЫЕ", 
                                 
                                 "МНОГОСТОРОННИЕ ЛЬГОТНЫЕ",
                                 
                                 "МНОГОСТОРОННИЕ НЕЛЬГОТНЫЕ", 
                                 
                                 "ИТОГО:")) %>% 
                
                set_names(c("creditor", "January", "February", "March", 
                            
                            "April", "May", "June", "July", "August", 
                            
                            "September", "October", "November", "December")) %>% 
                
                mutate(creditor = case_when(
                        
                        creditor == "ДВУСТОРОННИЕ ЛЬГОТНЫЕ" ~ "Bilateral Preferential", 
                        
                        creditor == "ДВУСТОРОННИЕ НЕЛЬГОТНЫЕ" ~ "Bilateral Non-light", 
                        
                        creditor == "МНОГОСТОРОННИЕ ЛЬГОТНЫЕ" ~ "Multilateral Preferential",
                        
                        creditor == "МНОГОСТОРОННИЕ НЕЛЬГОТНЫЕ" ~ "Non Relief Multilateral", 
                        
                        TRUE ~ "Total"
                        
                )) %>% 
                
                mutate(year = year) %>% 
                
                relocate(year, .after = creditor) %>% 
                
                pivot_longer(-c(creditor, year), names_to = "type", 
                             
                             values_to = "amount") %>% 
                
                mutate(amount = str_replace_all(amount, ",", "."),
                       
                       amount = str_trim(amount),
                       
                       amount = str_replace_all(amount, " ", ""), 
                       
                       amount = parse_number(amount)) %>% 
                
                mutate(creditor = case_when(
                        
                        str_detect(creditor, "[Bb]ilateral") ~ "Bilateral",
                        
                        str_detect(creditor, "[Mm]ultilateral") ~ "Multilateral",
                        
                        TRUE ~ "Total"
                        
                )) %>% 
                
                mutate(no = row_number()) %>% 
                
                group_by(year, type, creditor) %>% 
                
                summarise(amount = sum(amount))
}


my_data <- bilateral_pref_scrapper_pdf("pdf_2021.pdf", year = 2021) %>% 
        
        mutate(amount = amount * 1000000)

my_data22 <- bilateral_pref_scrapper_pdf("pdf_2022.pdf", year = 2022) %>% 
        
        mutate(amount = amount * 1000000)

## Scrap for 2020 ----
## December 2020 ----

bilateral_pref_scrapper_xlsx <- function(file, year){
        
        library(tidyverse)
        library(tabulizer)
        library(readxl)
        
        readxl::read_xls(file) %>% 
                
                as.data.frame() %>% 
                
                set_names(c("creditor", "January", "February", "March", 
                            
                            "April", "May", "June", "July", "August", 
                            
                            "September", "October", "November", "December")) %>% 
                
                mutate(across(.cols = -creditor, .fns = as.character)) %>% 
                
                filter(creditor %in% c("ДВУСТОРОННИЕ ЛЬГОТНЫЕ", 
                                 
                                 "ДВУСТОРОННИЕ НЕЛЬГОТНЫЕ", 
                                 
                                 "МНОГОСТОРОННИЕ ЛЬГОТНЫЕ",
                                 
                                 "МНОГОСТОРОННИЕ НЕЛЬГОТНЫЕ", 
                                 
                                 "ИТОГО:")) %>% 
                
                mutate(creditor = case_when(
                        
                        creditor == "ДВУСТОРОННИЕ ЛЬГОТНЫЕ" ~ "Bilateral Preferential", 
                        
                        creditor == "ДВУСТОРОННИЕ НЕЛЬГОТНЫЕ" ~ "Bilateral Non-light", 
                        
                        creditor == "МНОГОСТОРОННИЕ ЛЬГОТНЫЕ" ~ "Multilateral Preferential",
                        
                        creditor == "МНОГОСТОРОННИЕ НЕЛЬГОТНЫЕ" ~ "Non Relief Multilateral", 
                        
                        TRUE ~ "Total"
                        
                )) %>% 
                
                mutate(year = year) %>% 
                
                relocate(year, .after = creditor) %>% 
                
                pivot_longer(-c(creditor, year), names_to = "type", 
                             
                             values_to = "amount") %>% 
                
                mutate(amount = str_replace_all(amount, ",", "."),
                       
                       amount = str_trim(amount),
                       
                       amount = str_replace_all(amount, " ", ""), 
                       
                       amount = parse_number(amount)) %>% 
                
                mutate(creditor = case_when(
                        
                        str_detect(creditor, "[Bb]ilateral") ~ "Bilateral",
                        
                        str_detect(creditor, "[Mm]ultilateral") ~ "Multilateral",
                        
                        TRUE ~ "Total"
                        
                )) %>% 
                
                mutate(no = row_number()) %>% 
                
                group_by(year, type, creditor) %>% 
                
                summarise(amount = sum(amount)) %>% 
                
                mutate(amount = amount * 1000000)
}


data_2020 <- bilateral_pref_scrapper_xlsx("fy2020.xls", year = 2020)
data_2019 <- bilateral_pref_scrapper_xlsx("fy2019.xls", year = 2019)



final_data <- rbind(my_data22, my_data, data_2020, data_2019)
head(final_data)
final_data <- final_data %>% 
        
        pivot_wider(names_from = creditor, values_from = amount)

final_data %>% 
        
        fwrite("kyrgyz_external_debt.xlsx")


################################################################################
## Domestic debt 
my_years <- list.files(path = ".", pattern = "domestic.*.xlsx")

domestic_debt_kyrgyz_fun <- function(x, year){
        
        library(tidyverse)
        library(tabulizer)
        library(readxl)
        library(janitor)

        read_xlsx(x) %>% 
        
        set_names(c("name", "January", "February", "March", 
                    
                    "April", "May", "June", "July", "August", 
                    
                    "September", "October", "November", "December")) %>% 
        
        filter(name %in% c("КРАТКОСРОЧНЫЕ ЦЕННЫЕ БУМАГИ", 
                           
                           "ДОЛГОСРОЧНЫЕ ЦЕННЫЕ БУМАГИ",
                           
                           "ИНДЕКСАЦИЯ СБЕРЕЖЕНИЙ НАСЕЛЕНИЯ")) %>% 
        
        mutate(name = case_when(
                
                name == "КРАТКОСРОЧНЫЕ ЦЕННЫЕ БУМАГИ" ~ "Short-term securities",
                
                name == "ДОЛГОСРОЧНЫЕ ЦЕННЫЕ БУМАГИ" ~ "Long-term securities",
                
                TRUE ~ "Indexation of savings of the population"
        )) %>% 
                
                pivot_longer(cols = -name, names_to = "month", values_to = "amount") %>% 
                
                mutate(year = x %>% str_extract('[0-9]{4}') %>% parse_number()) %>% 
                
                mutate(amount = amount * 1000000) %>% 
                
                relocate(year, .after = month) %>% 
                
                pivot_wider(names_from = name, values_from = amount) %>% 
                
                clean_names() %>% 
                
                mutate(total_domestic = short_term_securities + 
                               
                               long_term_securities + 
                               
                               indexation_of_savings_of_the_population
                               
                               )
        
}


domestic_jan_22 <- tabulizer::extract_tables("domestic_jan22") %>% 
        
        as.data.frame() %>% 
        
        set_names(c("name", "January", "February", "March", 
                    
                    "April", "May", "June", "July", "August", 
                    
                    "September", "October", "November", "December")) %>% 
        
        select(name, January) %>% 
        
        filter(name %in% c("КРАТКОСРОЧНЫЕ ЦЕННЫЕ БУМАГИ", 
                           
                           "ДОЛГОСРОЧНЫЕ ЦЕННЫЕ БУМАГИ",
                           
                           "ИНДЕКСАЦИЯ СБЕРЕЖЕНИЙ НАСЕЛЕНИЯ")) %>% 
        
        mutate(name = case_when(
                
                name == "КРАТКОСРОЧНЫЕ ЦЕННЫЕ БУМАГИ" ~ "Short-term securities",
                
                name == "ДОЛГОСРОЧНЫЕ ЦЕННЫЕ БУМАГИ" ~ "Long-term securities",
                
                TRUE ~ "Indexation of savings of the population"
        )) %>% 
        
        mutate(year = 2022) %>% 
        
        pivot_longer(cols = -c(name, year), names_to = "month", values_to = "amount") %>% 
        
        mutate(amount = str_trim(amount),
               
               amount = str_replace_all(amount, ",", "."),
               
               amount = parse_number(amount)) %>% 
        
        mutate(amount = amount * 1000000) %>% 
        
        relocate(year, .after = month) %>% 
        
        pivot_wider(names_from = name, values_from = amount) %>% 
        
        clean_names() %>% 
        
        mutate(total_domestic = short_term_securities + 
                       
                       long_term_securities + 
                       
                       indexation_of_savings_of_the_population
               
        )



map_dfr(my_years, domestic_debt_kyrgyz_fun) %>% 
        
        rbind(domestic_jan_22) %>% 
        
        fwrite("kyrgyz_domestic_debt.xlsx")


data_2019
data_2020
final_data
my_data
