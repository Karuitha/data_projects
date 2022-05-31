## Load packages manager
if(!require(pacman)){
        
        install.packages("pacman")
        
}

## Load required packages 
pacman::p_load(tidyverse, tabulizer, data.table, tesseract)

## Scrap the pdf 
my_et_data <- tabulizer::extract_tables("extract_2021_table7.pdf") %>% 
        
        as.data.frame() %>% 
        
        tibble() %>% 
        
        fwrite(., "my_new_et_data.csv")
        
## Read in the data again ----
my_et_data <- read_csv("my_new_et_data.csv", na = "") %>% 
        
        mutate(
                
                X2 = if_else(is.na(X2), X3, X2),
                
                X5 = if_else(is.na(X5), X6, X5),
                       
                X8 = if_else(is.na(X8), X9, X8),
                
                X12 = if_else(is.na(X12), X13, X12),
                
                X16 = if_else(is.na(X16), X17, X16) 
                       
                       ) %>% 
        
        select(-X3, -X6, -X9, -X11, -X13, -X15, -X17, -X18) %>% 
        
        separate(X16, into = c("X16", "X16_1"), sep = "\\s+", 
                 
                 fill = "right") %>% 
        
        set_names(c("source", "X201718USD", "X201718Birr",
                    
                    "201819USD", "X201819Birr", "X201920USD",
                    
                    "X201920Birr", "X202021USD", "X202021Birr", 
                    
                    "X311221USD", "X311221Birr")) %>% 
        
        filter(!is.na(source)) %>% 
        
        select(-ends_with("Birr")) %>% 
        
        mutate(across(.cols = -source, 
                      
                      .fns = parse_number))
        
        
        

## Missing data ----
sapply(my_et_data, is.na) %>% 
        
        colSums() %>% 
        
        tibble(var_names = names(my_et_data), 
               
               missing = .) %>% 
        
        arrange(desc(missing)) %>% 
        
        mutate(per_missing = missing/ nrow(my_et_data) * 100)

## September 2021
eng <- tesseract("eng")
text <- tesseract::ocr("extract_2021_sept_1.png", engine = eng)
cat(text)

text %>% 
        
        str_split(pattern = "\\\n") %>% 
        
        tibble() %>% 
        
        pluck(1) %>% 
        
        .[[1]] %>% 
        
        tibble() %>% 
        
        set_names("source") %>% 
        
        filter(!str_detect(source, "^\\[")) %>% 
        
        mutate(real_source = str_extract_all(source,
                                             
        "^.*[a-zA-Z]*\\s+[a-zA-Z]*", simplify = TRUE   
                                             )) %>% 
        
        relocate(real_source)



str_view("TOTAL OUTSTANDING’ | 25,812.10", 
            
            "^.*[a-zA-Z]*\\s+[a-zA-Z]*")       
        relocate(real_source)
        
        separate(source, into = c(
                
                "source", "USD1", "EB1",
                
                "USD2", "EB2", "USD3", "EB3",
                
                "USD4", "EB4","USD5", "EB5"
        ), 
        
        sep = "|")



