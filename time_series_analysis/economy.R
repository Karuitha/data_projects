library(fpp3)
library(tsibble)
library(tsibbledata)
library(tidyverse)
data(world_economy)

economy <- tsibbledata::global_economy %>% 
    distinct() %>% 
    mutate(GDP = case_when(
        
        is.na(GDP) ~ 3478787909,
        TRUE ~ GDP
        
    ))

################################################################################
economy %>% 
    ggplot(mapping = aes(x = Year, 
                         y = GDP,
                         color = Country)) + 
    geom_line(show.legend = FALSE) + 
    artyfarty::theme_ft()

################################################################################
################################################################################
economy_tsibble <- economy %>% 
    select(Country, Year, GDP) %>% 
    as_tsibble(
        
        index = Year,
        key = Country
    ) %>% 
    
    fill_gaps() 
    
################################################################################
economy_tsibble %>% 
    autoplot(GDP) + 
    artyfarty::theme_ft() + 
    scale_y_continuous(labels = scales::comma_format()) + 
    theme(legend.position = "none")

################################################################################
economy_tsibble %>% 
    ACF(GDP) %>% 
    autoplot()
