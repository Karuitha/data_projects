## Load required packages ----
if(!require(pacman)){
  install.packages('pacman')
}


pacman::p_load(tidyverse, janitor, skimr)

## Load the data ----
my_data <- read_csv("Account Sales Data for Analysis v2.xlsx - Sheet1.csv") |>
  janitor::row_to_names(3) |>
  rename(y2017 = '2017', y2018 = '2018', y2019 = '2019', y2020 = '2020', 
         y2021 = '2021') |>
  janitor::clean_names() |>
  mutate(y2017 = parse_number(y2017))
glimpse(my_data)

## Create the compund growth rate 

my_data <- my_data |>
  
  mutate(
    
    
    cagr = (y2021 / y2017) ** ((1/5) -1)) |>
  
  pivot_longer(cols = starts_with("y2"),
               
               names_to = "year", values_to = "values") |>
  mutate(year = parse_number(year))
  


my_data |> 
  reframe(
    mean_cagr = mean(cagr, na.rm = TRUE),
    
    
    median_cagr = median(cagr, na.rm = TRUE),
    
    .by = c(account_type, year)
    
  ) %>%
  
  ggplot(aes(x = year)) +
  geom_line(aes(y = median_cagr, color = account_type))

  

