#################################################
if(!require(pacman)){
  install.packages("pacman")
}
################################################
pacman::p_load(janitor, tidyverse, ggthemes, tidyquant, 
               ggridges, GGally)
################################################
my_data <- read.csv("us_state_6digitnaics_2019.txt") %>% 
  clean_names()

head(my_data)

my_data %>% 
  
  ggplot(aes(x = entrsize)) + 
  
  geom_histogram(col = "black", fill = "skyblue") + 
  
  theme_tq()

my_data %>% 
  
  select(-naics, -statedscr) %>% 
  
  GGally::ggpairs()


my_data %>% 
  
  ggplot(mapping = aes(x = naicsdscr, y = entrsize)) + 
  
  geom_boxplot()
