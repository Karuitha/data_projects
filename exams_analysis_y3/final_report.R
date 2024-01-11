## load packages ----
if(!require(pacman)){
  install.packages('pacman')
}

p_load(tidyverse, janitor)


read_csv("bis.csv") %>% 
  select(-comment) %>% 
  mutate(row = row_number()) %>% 
  relocate(row) %>% 
  mutate(mark = row) %>% 
  mutate(subject = str_to_upper(subject)) %>% 
  pivot_wider(id_cols = -row, 
              names_from = subject,
              values_from = mark) %>% 
  write_csv("final_report/bis.csv")





read_csv("accounting.csv") %>% 
  select(-comment) %>% 
  mutate(row = row_number()) %>% 
  relocate(row) %>% 
  mutate(mark = row) %>% 
  mutate(subject = str_to_upper(subject)) %>% 
  pivot_wider(id_cols = -row, 
              names_from = subject,
              values_from = mark) %>% 
  write_csv("final_report/acc.csv")



read_csv("finance.csv") %>% 
  select(-comment) %>% 
  mutate(row = row_number()) %>% 
  relocate(row) %>% 
  mutate(mark = row) %>% 
  mutate(subject = str_to_upper(subject)) %>% 
  pivot_wider(id_cols = -row, 
              names_from = subject,
              values_from = mark) %>% 
  write_csv("final_report/fin.csv")
