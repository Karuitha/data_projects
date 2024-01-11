## mapping in R 

## Joh Karuitha 

## Load required packages ----

library(tidyverse)

library(maps)

library(mapproj)

## Load the world coordinates ----

world_map_data <- map_data("world") %>% 
        
        tibble()

head(world_map_data)


## Draw a map ----

world_map_data %>% 
        
        ggplot(mapping = aes(x = long, y = lat, map_id = region, fill = region)) + 
        
        geom_map(map = world_map_data, col = "gray30", show.legend = FALSE) 

##coord_map(projection = "mercator", orientation = c(0,38,0))
