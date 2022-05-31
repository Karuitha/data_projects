## Data for suriname debt 2017-2021
## Load the packages
library(tidyverse)
library(glue)
library(data.table)

## Create the dataset
months <- c("June", "Sept", "Dec", "March", "June", "Sept", "Dec", 
            
            "March", "June", "Sept", "Dec", "March", "June", "Sept", "Dec",
            
            "March", "June", "Sept")

years <- c(rep(2017, 3), rep(2018, 4), rep(2019, 4), 
           
           rep(2020, 4), rep(2021, 3))

panelA <- c(1569.71, 1602.971, 1669.265, 1719.229, 1654.318, 
            
            1652.958, 1715.446, 1738.853, 1745.101, 1772.690,
            
            1987.183, 2003.892, 2027.986, 2059.138, 2113.509,
            
            2050.801, 2042.393, 2101.084)

panelB <- c(1062.12, 678.022, 700.265, 704.046, 756.653,
            
            767.429, 772.655, 866.936, 965.251, 979.594,
            
            984.049, 950.287, 1728.879, 1881.863, 1971.991,
            
            1947.992, 1988.125, 1167.562)


suriname_data <- data.frame(months, years, panelA, panelB)

head(suriname_data)
tail(suriname_data)
str(suriname_data)



suriname_data %>% 
        
        pivot_longer(-c("months", "years"), 
                     
                     names_to = "data_type", 
                     
                     values_to = "values") %>% 
        
        transmute(period = glue("{months} {years}"), 
                  
                  data_type, values) %>% 
         
        ggplot(mapping = aes(x = period, y = values, 
                             
                             group = data_type,
                             
                             color = data_type)) + 
        
        geom_line() +
        
        facet_wrap(~ data_type) + 
        
        labs(title = "Debt Statistics for Suriname, 2017-21", 
             
             y = "Total Debt, USD") + 
        
        theme(plot.title = element_text(face = "bold")) + 
        
        scale_color_manual(values = c("red", "blue"))


suriname_data %>% 
        
        mutate(panelA = panelA * 1000000,
               
               panelB = panelB * 1000000) %>% 
        
        transmute(period = glue("{months} {years}"), panelA, panelB) %>% 
        
fwrite("suriname_data.xlsx")

