## Load required packages ----
if(!require(pacman)){
        
        install.packages("pacman")
}

## Install from Github -----
##remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), 
                        
                        ##INSTALL_opts = "--no-multiarch")

pacman::p_load(tidyverse, tabulizer, here, data.table, gdata)



#####################################
###############################################################################
my_excel_scrapper <- function(x){
  
        readxl::read_xlsx(x, col_names = FALSE) %>% 
                
                set_names(c("source", "outstanding_debt_start", "disbursements",
                            
                            "principal_payments", "other_payments", 
                            
                            "parity_charge",
                            
                            "outstanding_debt_end", "borrower"
                            
                )) %>% 
                
                mutate(period = x) %>% 
                
                filter(!str_detect(source, "Table A*|Disburs*|Debt")) %>% 
                
                mutate(
                      
                        outstanding_debt_start = as.numeric(outstanding_debt_start),
                        
                        disbursements = as.numeric(disbursements),
                            
                        principal_payments = as.numeric(principal_payments), 
                        
                        other_payments = is.numeric(other_payments), 
                            
                        parity_charge = as.numeric(parity_charge),
                            
                        outstanding_debt_end = as.numeric(outstanding_debt_end)  
                        
                ) %>% 
                
                select(period, source, outstanding_debt_start,
                       
                       outstanding_debt_end, borrower)
}


###############################################
final_data <- map(list.files(".", pattern = ".*\\.xlsx"), my_excel_scrapper) %>% 
        
        bind_rows()


###############################################
## Pattern to capture months
month_pattern = "[Jj]anuary|[Ff]ebruary|[Mm]arch|[Aa]pril|[Mm]ay|[Jj]une|[Jj]uly|[Aa]ugust|[Ss]eptember|[Oo]ctober|[Nn]ovember|[Dd]ecember"

## Clean the data
final_data <- final_data %>% 
        
        filter(!source %in% c("CENTRAL GOVERNMENT", 
                             
                             "NON-FINANCIAL PUBLIC SECTOR",
                             
                             "FINANCIAL PUBLIC SECTOR")) %>% 
        
        ## Extract month
        mutate(month = str_extract(period, month_pattern)) %>% 
        
        ## Extract year
        mutate(year = str_extract(period, "\\d{4}")) %>% 
        
        ## Update values by multiplying by 1000
        mutate(across(.cols = starts_with("out"), ~ .x * 1000)) %>% 
        
        mutate(source = str_to_title(source),
               
               source = str_remove_all(source, "\\s*\\(.*\\)$")) %>% 
        
        ## Combine the similar lender
        mutate(
                
                source = case_when(
                        
                        str_detect(source, "Bank [Oo]f New York.*") ~ "Bank of New York",
                        
                        str_detect(source, "^Bear.*") ~ "Bear Stearns & Company",
                        
                        str_detect(source, "^Belize Mortgage Company") ~ "Belize Mortgage Company",
                        
                        str_detect(source, "Caribbean Community Climate Change") ~ 
                                
                                "Caribbean Community Climate Change Center",
                        
                        str_detect(source, "Caribbean Development") ~ "Caribbean Development Bank",
                        
                        str_detect(source, "British Caribbean Bank") ~ "British Caribbean Bank Limited",
                        
                        str_detect(source, "Central American Bank For Ec.*") ~ "Central American Bank For Economic Integration",
                        
                        str_detect(source, "European Inv.*") ~ "European Investment Bank",
                        
                        str_detect(source, "Government.*United States") ~ "Government of the United States",
                        
                        str_detect(source, "Government.*Venezuela") ~ "Government of Venezuela",
                        
                        str_detect(source, "[Rr]econstruction") ~ 
                                
                                "International Bank for Reconstruction and Development",
                        
                        str_detect(source, "[Ii]nternational [Ff]und [Ff]or [Aa]gric.*") ~ 
                                
                                "International Fund for Agricultural Development",
                        
                        str_detect(source, "International Cooperation.*") ~ 
                                
                                "International Cooperation and Development Fund",
                        
                        str_detect(source, ".*[Mm]onetary.*") ~ "International Monetary Fund",
                        
                        str_detect(source, "Kuwait Fund For.*") ~ "Kuwait Fund for Arab Economic Development",
                        
                        str_detect(source, "Mega International.*") ~ "Mega International Commercial Bank Company Limited",
                        
                        str_detect(source, "Opec Fund.*") ~ "Opec Fund for International Development",
                        
                        str_detect(source, "Paine Webber.*") ~ "Paine Webber Real Estate Securities Inc.",
                        
                        str_detect(source, "Royal Merchant.*") ~ "Royal Merchant Bank And Finance Co.",
                        
                        str_detect(source, ".*\\s*Nova Scotia") ~ "The Bank Of Nova Scotia",
                        
                        str_detect(source, "^U.*Fixed.*Notes$") ~ "US$30mn Fixed Rate Notes",
                        
                        TRUE ~ source
                        
                )   
                
        ) %>% 
        
        filter(borrower == "central government") %>% 
        
        filter(!duplicated(.)) %>% 
        
        as.data.table()


#################################################
lender_type <- read_csv("lenders_type.csv")

#################################################
final_data <- final_data %>% 
        
        left_join(lender_type, by = "source") %>% 
        
        select(-period) %>% 
        
        mutate(month = str_to_title(month)) %>% 
        
        relocate(month, year) %>% 
        
        relocate(lender_type, .after = "source")
        
######################################################
final_data %>% count(source) %>% 
        
        fwrite(., "final_data/lenders.xlsx")

######################################################
final_data %>% 
        
        fwrite(., "final_data/final_data_clean.xlsx")

######################################################
final_data[, .N, by = source] 

final_data[source %like% "China", 
           
           .(no_of_loans =.N, average_loan = mean(outstanding_debt_end)), 
           
           by = source]

uniqueN(final_data)

final_data[,uniqueN(source), by = lender_type]
final_data[,.SD[1], by = lender_type, .SDcols = "source"]

#############################################
final_data[,.(mean_debt = mean(outstanding_debt_end)), 
           
           by = lender_type][order(mean_debt)]



## TT
tt <- read.csv("final_data/main_tt.csv") %>% 
        
        janitor::clean_names() %>% 
        
        filter(!is.na(students)) %>% 
        
        mutate(day = lubridate::dmy(day)) %>% 
        
        filter(duplicated(
        
tt %>% filter(is.na(room))

tt %>% 
        
        group_by(room) %>% 
        
        summarise(total = sum(students))
