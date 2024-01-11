library(tidyverse)
library(data.table)

titles <- c("Vaste_Schuld", "Vlottende_Schuld", "wo_Middenlange_Termjin_Schuld", 
            
            "wo_Lange_Termjin_Schuld", "subtotal_A", "Vlottende_Schuld", "Vaste_Schuld", 
            
            "wo_Middenlange_Termjin_Schuld", "wo_Lange_Termjin_Schuld", "subtotal_B")

class <- c(rep("A", 5), rep("B", 5))


june2017 <- c(1561.79, 0, 0, 0, 1561.79, 282.68, 779.45, 122.88, 
              
              656.57, 1062.12)


sept2017 <- c(1602.971, 0, 4.377, 1598.594, 1602.971, 469.794, 
              
              208.229, 81.852, 387.942, 678.022)


dec2017 <- c(1669.265, 0, 3.881, 1665.384, 1669.265, 488.222, 
             
             212.305, 120.051, 368.171, 700.527)


march2018 <- c(719.229, 0, 4.032, 1713.901, 1719.229, 529.186, 
               
               174.86, 165.973, 363.213, 704.046)

june2018 <- c(1654.318, 0, 3.229, 1651.089, 1654.318, 569.57,
              
              187.083, 207.069, 362.501, 756.653)

sept2018 <- c(1652.958, 0, 3.263, 1649.695, 1652.958, 
              
              593.846, 173.583, 205.674, 388.172, 767.429)

dec2018 <- c(1715.446, 0, 2.661, 1712.785, 1715.446, 593.936,
             
             178.716, 221.329, 372.610, 772.655)

march2019 <- c(1738.853, 0, 6.123, 1732.730, 1738.853, 
               
               595.151, 271.785, 223.473, 371.678, 866.936)

june2019 <- c(1745.101, 0, 5.678, 1739.423, 1745.101, 595.605,
              
              369.646, 228.406, 367.199, 965.251)

sept2019 <- c(1772.690, 0, 2.044, 1770.646, 1772.690, 603.496,
              
              376.098, 188.028, 415.468, 979.594)

dec2019 <- c(1987.183, 0, 5.097, 1982.086, 1987.183, 625.974, 358.075,
             
             251.513, 374.461, 984.049)

march2020 <- c(1988.169, 15.723, 11.172, 1976.997, 2003.892, 620.748, 329.539, 
               
               239.380, 381.368, 950.287)

june2020 <- c(1995.091, 32.895, 11.006, 1984.085, 2027.986, 279.501, 1449.378,
              
              243.546, 1205.832, 1728.879)

sept2020 <- c(2023.8, 35.338, 5.036, 2018.764, 2059.138, 287.761, 1594.242,
             
             365.160, 1229.082, 1881.863)

dec2020 <- c(1998.109, 115.400, 4.757, 1993.352, 2113.509, 323.401,
             
             1648.591, 412.674, 1235.916, 1971.991)

march2021 <- c(1990.178, 60.623, 7.169, 1983.009, 2050.801, 
               
               331.541, 1616.451, 271.347, 1345.104, 1947.992)

june2021 <- c(1973.837, 68.556, 6.919, 1966.918, 2042.393, 381.836,
              
              1606.290, 407.199, 1199.090, 1988.125)

sept2021 <- c(1924.771, 176.313, 6.945, 1917.826, 2101.084,
              
              223.807, 943.754, 286.538, 657.217, 1167.562)

full_suriname_data <- data.frame(titles, class, june2017, sept2017, dec2017, march2018, june2018,
           
           sept2018, dec2018, march2019, june2019, sept2019, dec2019, march2020,
           
           june2020, sept2020, dec2020, march2021, june2021, sept2021)

head(full_suriname_data)

full_suriname_data %>% 
        
        pivot_longer(cols = -c(titles, class), names_to = "date",
                     
                     values_to = "amount") %>% 
        
        mutate(titles_english = case_when(
                
                titles == "Vaste_Schuld" ~ "Fixed Debt",
                
                titles == "Vlottende_Schuld" ~ "Current Debt",
                
                titles == "wo Middenlange_Termjin_Schuld" ~ "Medium Term Debt", 
                
                TRUE ~ "Long Term Debt"
                
        )) %>% 
        
        relocate(titles_english) %>% 
        
        mutate(month = str_extract(date, "^[a-zA-Z]*"),
               
               year = str_remove_all(date, "^[a-zA-Z]*")) %>% 
        
        select(-date) %>% 
        
        mutate(date = glue::glue("{month} {year}")) %>% 
        
        select(-month, -year) %>% 
        
        relocate(date) %>% 
        
        fwrite(., "suriname_data.xlsx")
