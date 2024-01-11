## Load package manager
if (!require(pacman)) {
    install.packages("pacman")
    library(pacman)
}

## Load required packages (install if not present)
## NB This step requires INTERNET connection
pacman::p_load(
    tidyverse, palmerpenguins,
    ggtext, ggside, RCurl, monochromeR,
    httpgd, languageserver
)

## Load the dataset
data("penguins")

## We work with the refined penguins data
## We explore the data
head(penguins)
glimpse(penguins)

## Plot 
banana_colors <- list(
    "Adelie" = "#89973d",
    "Chinstrap" = "#e8b92f",
    "Gentoo" = "#a45e41"
)

penguins %>%
    
    na.omit() %>% 
    
    mutate(sex_code = case_when(
        
        sex == "male" ~ 0,
        
        TRUE ~ 1
        
    )) %>% 
    
    ggplot(mapping = aes(x = bill_length_mm, 
                         
                         y = bill_depth_mm,
                         
                         color = species)) + 
    
    geom_point(mapping = aes(alpha = flipper_length_mm), 
               
               shape = 1,
               
               stroke = 2, size = 4) + 
    
    scale_color_manual(values = banana_colors) + 
    
    scale_alpha(range = c(0.3, 1)) +
    
    theme_minimal()
