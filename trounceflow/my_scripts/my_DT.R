## Libraries----
library(data.table)
library(tidyverse)
data("LifeCycleSavings")

head(LifeCycleSavings)
row.names(LifeCycleSavings)


LifeCycleSavings %>% 
        
        ggplot(mapping = aes(x = row.names(LifeCycleSavings), 
                             
                             y = pop15) + 
                       
                       geom_col()
               
               