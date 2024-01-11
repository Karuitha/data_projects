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

## 1. Use color purposefully (if not possible be consistent)
## This makes it easy for users to remember
## Example of generating a color palette
## use imagecolorpicker.com
machine <- "#061939"
human <- "#25470"

??monochromeR::generate_palette
monochromeR::generate_palette(machine,
    modification = "go_darker",
    n_colours = 4,
    view_palette = TRUE
)

## Wrangle data to our taste
my_penguins <- penguins %>%
    mutate(banana_quantity = case_when(
        species == "Adelie" & island == "Biscoe" ~ 1,
        species == "Adelie" & island == "Dream" ~ 0.6,
        species == "Adelie" & island == "Torgesten" ~ 0,
        TRUE ~ 1
    ))

## We  now plot the data

banana_colors <- list(
    "Adelie" = "#89973d",
    "Chinstrap" = "#e8b92f",
    "Gentoo" = "#a45e41"
)

my_penguins %>%
    ggplot(mapping = aes(
        x = bill_depth_mm,
        y = bill_length_mm,
        color = species
    )) +
    geom_point(mapping = aes(alpha = banana_quantity), size = 4) +
    scale_alpha(range = c(0.2, 1)) +
    theme_minimal(base_size = 12) +
    labs(title = "Banana Loaf Tastes Better When baked with Ripe or Over-ripe Bananas",
        subtitle = "Which Banana loafs taste better- with raw, ripe, or over-ripe bananas?",
        caption = "Karuitha, 2022"
    ) +
    scale_color_manual(values = banana_colors) +
    geom_smooth(se = FALSE)

## 2. Use intuitive orientations

## 3. Add color to text
