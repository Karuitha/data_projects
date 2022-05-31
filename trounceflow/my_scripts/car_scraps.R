library(tidyverse)
library(rvest)
library(glue)


read_html("https://www.beforward.jp/stocklist/client_wishes_id=/description=/make=/model=/fuel=/fob_price_from=/fob_price_to=/veh_type=/steering=Right/mission=/mfg_year_from=2016/mfg_month_from=/mfg_year_to=/mileage_from=/mileage_to=/cc_from=/cc_to=/showmore=/drive_type=/color=/stock_country=47/area=/seats_from=/seats_to=/max_load_min=/max_load_max=/veh_type_sub=/view_cnt=25/page=1/sortkey=n/sar=/from_stocklist=1/keyword=/kmode=and/") %>%
        
        html_nodes(".make-model .vehicle-url-link") %>% 
        
        html_text()




url1 <- "https://www.beforward.jp/stocklist/client_wishes_id=/description=/make=/model=/fuel=/fob_price_from=/fob_price_to=/veh_type=/steering=Right/mission=7/mfg_year_from=2016/mfg_month_from=/mfg_year_to=/mileage_from=/mileage_to=/cc_from=/cc_to=/showmore=/drive_type=/color=/stock_country=47/area=/seats_from=/seats_to=/max_load_min=/max_load_max=/veh_type_sub=/view_cnt=25/page="

url2 <- '/sortkey=n/sar=/from_stocklist=1/keyword=/kmode=and/'  

## make a function
scrapper <- function(x, nodes){
        
        
        read_html(glue("{url1}x{url2}")) %>% 
        
        html_nodes(nodes) %>% 
        
        html_text() %>% 
                
        str_remove_all(., '\\\n') %>% 
                
        str_trim()
}
        

## Scrap car names
dat1 <- scrapper(1, '.make-model .vehicle-url-link') %>% 
        
        tibble() %>% 
        
        set_names("make") %>% 
        
        mutate(year = str_extract(make, "^\\d{4}")) %>% 
        
        mutate(make = make %>% str_remove_all(year) %>% str_replace_all("\\s{2,}", " "))

## Scrap car details "mileage", "year", "cc", "trans", "location"
dat2 <- scrapper(1, ".val") %>% tibble() %>% 
        
        set_names(c("details")) %>% 
        
        mutate(data = rep(c("mileage", "year", "cc", "trans", "location"), times = 25),
               
               no = rep(1:25, each = 5)) %>% 
        
        pivot_wider(names_from = "data", values_from = "details")


## Get fuel and drive data 
dat3 <- scrapper(1, ".td-3rd") %>% 
        
        tibble() %>% 
        
        set_names("details") %>% 
        
        mutate(no = rep(1:25, each = 2), data = rep(c("fuel", "drive"), times = 25)) %>% 
        
        pivot_wider(names_from = "data", values_from = "details")


## Get drive and color
dat4 <- scrapper(1, ".td-2nd") %>% 
        
        tibble() %>% 
        
        set_names("details") %>% 
        
        mutate(no = rep(1:25, each = 2), data = rep(c("drive_side", "colour"), times = 25)) %>% 
        
        pivot_wider(names_from = "data", values_from = "details")

## Get tge total price 
scrapper5 <- scrapper(1, ".total-price span") %>% 
        
        tibble() %>% 
        
        set_names("price") %>% 
        
        filter(price != "")



## I now scrap all the pages and get the data together. 

final_data_1 <- lapply(1:1225, scrapper, '.make-model .vehicle-url-link') 

final_data_1 <- final_data_1 %>% 
        
        unlist() %>% 
        
        tibble() %>% 
        
        set_names("make") %>% 
        
        mutate(year = str_extract(make, "^\\d{4}")) %>% 
        
        mutate(make = make %>% str_remove_all(year) %>% str_replace_all("\\s{2,}", " ")) %>% 
        
        View()


## Final data 2 
final_data_2 <- lapply(1:1225, scrapper, ".val") %>% 
        
        unlist() %>% 
        
        tibble() %>% 
        
        set_names(c("details")) %>% 
        
        mutate(data = rep(c("mileage", "year", "cc", "trans", "location"), times = 25),
               
               no = rep(1:25, each = 5)) %>% 
        
        pivot_wider(names_from = "data", values_from = "details")


## Final data 3
final_data_3 <- sapply(1:1225, scrapper, ".td-3rd") %>% 
        
        unlist() %>% 
        
        tibble() %>% 
        
        set_names("details") %>% 
        
        mutate(no = rep(1:25, each = 2), data = rep(c("fuel", "drive"), times = 25)) %>% 
        
        pivot_wider(names_from = "data", values_from = "details")

## Final data 4
final_data_4 <- sapply(1:1225, scrapper, ".td-2nd") %>% 
        
        unlist() %>% 
        
        tibble() %>% 
        
        set_names("details") %>% 
        
        mutate(no = rep(1:25, each = 2), data = rep(c("drive_side", "colour"), times = 25)) %>% 
        
        pivot_wider(names_from = "data", values_from = "details")

## Final_ data 5
final_data_5 <- sapply(1:1225, scrapper, ".total-price span") %>% 
        
        unlist() %>% 
        
        tibble() %>% 
        
        set_names("price") %>% 
        
        filter(price != "")

## Final data
final_data <- final_data_1 %>% bind_cols(final_data_2, final_data_3, final_data_4, final_data_5)
