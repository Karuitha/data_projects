## Plotting data 
if(!require(pacman)){
        
        install.packages("pacman")
        
}

## Load required packages ----
pacman::p_load(tidyverse, lubridate, tidyquant)

## Theme
ggplot2::theme_set(theme_tq())

## Read in the data ----
et_data <- readxl::read_xlsx("5413218797295657941ethiopia_tables__4_.xlsx",
                             
                             sheet = 1, skip = 1, na = "") %>% 
        
        filter(!is.na(`...1`)) %>% 
        
        rename(source = `...1`) %>% 
        
        pivot_longer(-source, names_to = "date", 
                     
                     values_to = "amount") %>% 
        
        mutate(date = str_remove_all(date, "^X"),
               
               amount = str_remove_all(amount, ","),
               
               amount = as.numeric(amount),
               
               date = mdy(date))

## Explore the data ----
head(et_data)

## Chart total debt and
et_data %>% 
        
        filter(source != "BILATERALS") %>% 
        
        ggplot(mapping = aes(x = date, y = amount, 
                             
                             color = source)) + 
        
        geom_line(lwd = 1)
