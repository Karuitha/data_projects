## Most dissapointed people after BABA
library(twitteR)
library(rtweet)
library(httpuv)
library(tidyverse)
library(ggtextures)
library(magick)
library(rsvg)

## Get Data ----
my_tweeter_data <- lookup_users(c("DavidNdii", "ahmednasirlaw", 
                                  
                                  "MigunaMiguna")) %>% 
  
  select(name, statuses_count) %>% 
  
  mutate(image = list(
    
    image_read_svg("ndii.svg"),
    
    image_read_svg("ahmed.svg"), 
    
    image_read_svg("miguna.svg")
  )) %>% 
  
  mutate(name = fct_reorder(name, -statuses_count))
  
my_tweeter_data %>% 
  
  ggplot(aes(x = name, y = statuses_count, image = c(image))) + 
  
  geom_bar(stat = "identity", col = "black", fill = "white") +
  
  geom_isotype_col(
    img_height = grid::unit(1, "null"), img_width = NULL,
    ncol = 1, nrow = 1, hjust = 1, vjust = 0.5
  ) +
  
  
  
  theme_minimal() + 
    
    labs(title = "The Real Losers: Disillusionment with Voting Outcome (aka Debe)",
         
         subtitle = glue::glue("Analysis of Tweeter Data for 18 Days upto {format(Sys.Date(), format = '%A %B %d, %Y')} (Statuses Count)"), 
         
         x = NULL, y = NULL,
         
         caption = "John Karuitha, 2022
         Using R and GGPLOT2") + 
  
  theme(axis.text.x = element_blank(),
        
        plot.title = element_text(size = 20, face = "bold"),
        
        plot.subtitle = element_text(size = 14, face = "italic", color = "red")
        
        ) 
