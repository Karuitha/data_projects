## Load packages manager ----
if(!require(pacman)){
  install.packages('pacman')
}

p_load(tidyverse, janitor, naniar, readxl)

## Read in the data ----
??read_xlsx
eco <- read_xlsx("B106.4draft3.xlsx", na = "X", skip = 4, n_max = 83) %>% 
  remove_empty() %>% 
  clean_names() %>% 
  rename(sno = x1, reg = x2, name = x8) %>% 
  mutate(across(
    .cols = -c(sno, reg, name)
  ))
tail(eco)
