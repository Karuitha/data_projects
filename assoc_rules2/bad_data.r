# -------------------------------------------
## Load the packages ----
if(!require(pacman)){
    install.packages('pacman')
}

## load the packages 
pacman::p_load(tidyverse, janitor)
# -------------------------------------------
bad_data <- tribble(~"", ~"", ~"", ~ "", ~"", ~"",
"NAME", "AGE", NA_character_, "WeiGHT", "PLACE____",  "gender",
"Jane", "15", NA_character_, "65", "Kenya", "F",
"Paul", "21", NA_character_, "73", "Kenya", "M",
"Oloo", "25", NA_character_, "75", "Kenya", "M",
"Jane", "18", NA_character_, "63", "Kenya", "F",
"Nyokabi", "35", NA_character_, "85", "Kenya", "F",
"Ciku", "20", NA_character_, "70", "Kenya","F",
NA_character_, NA_character_, NA_character_, 
NA_character_, NA_character_, NA_character_)
# --------------------------------------------
bad_data %>%
    row_to_names(1) %>%
    clean_names() |>
    remove_empty(which = c('rows', 'cols')) |>
    remove_constant() %>%
    set_names(names(.) %>% str_remove("_"))














