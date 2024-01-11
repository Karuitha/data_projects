## Load packages ----
library(tidyverse)
library(maps)
library(mapproj)
library(ggthemes)

## The data
my_maps_data <- map_data("world") %>%
    tibble()

## Draw the map
(base_map <- my_maps_data %>%
    ggplot() +
    geom_map(
        mapping = aes(
            x = long, y = lat,
            map_id = region
        ),
        map = my_maps_data,
        color = "gray80", fill = "gray30",
        size = 0.2
    ) +
    labs(x = "", y = "", title = "My Map") +
    theme_economist())

base_map +
    coord_map("ortho", orientation = c(8, 34, 0))

base_map +
    coord_map("ortho", orientation = c(55.4, 3.4, 0))

base_map +
    coord_map("ortho", orientation = c(37, -96, 0))
