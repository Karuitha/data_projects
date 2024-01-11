## Load packages ----
if(!require(pacman)){
  install.packages('pacman')
}

p_load(tidyverse, janitor, readxl, rio)

## Accounting option ----
readxl::read_xlsx("Book1.xlsx", na = c("X", ""),
                  sheet = 1) %>% 
  clean_names() %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  select(-number) %>% 
  pivot_longer(-c(reg_no, student_name),
               names_to = "subject",
               values_to = "marks") %>% 
  filter(str_detect(marks, "e$|c$")|is.na(marks)) %>% 
  mutate(comment = case_when(
    
    is.na(marks) ~ "Did not sit for CAT and Exams",
    str_detect(marks, "e$") ~ "Did not sit for CAT",
    TRUE ~ "Did not sit for final exam (but sat for CAT)"
  )) %>% 
  
  select(-marks) %>% 
  write_csv("accounting.csv")

## Finance option ----
readxl::read_xlsx("Book1.xlsx", na = c("X", ""),
                  sheet = 2) %>% 
  clean_names() %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  select(-number) %>% 
  pivot_longer(-c(reg_no, student_name),
               names_to = "subject",
               values_to = "marks") %>% 
  filter(str_detect(marks, "e$|c$")|is.na(marks)) %>% 
  mutate(comment = case_when(
    
    is.na(marks) ~ "Did not sit for CAT and Exams",
    str_detect(marks, "e$") ~ "Did not sit for CAT",
    TRUE ~ "Did not sit for final exam (but did CAT)"
  )) %>% 
  
  select(-marks) %>% 
  write_csv("finance.csv")


## Management information system ----
readxl::read_xlsx("Book1.xlsx", na = c("X", ""),
                  sheet = 3) %>% 
  clean_names() %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  select(-number) %>% 
  pivot_longer(-c(reg_no, student_name),
               names_to = "subject",
               values_to = "marks") %>% 
  filter(str_detect(marks, "e$|c$")|is.na(marks)) %>% 
  mutate(comment = case_when(
    
    is.na(marks) ~ "Did not sit for CAT and Exams",
    str_detect(marks, "e$") ~ "Did not sit for CAT",
    TRUE ~ "Did not sit for final exam (but did CAT)"
  )) %>% 
  
  select(-marks) %>% 
  write_csv("bis.csv")



##################################################
updated <- read_xlsx("B100.3_updated.xlsx",
                     na = c("X", "")) %>% 
  clean_names()

updated %>% 
  filter(group == "A") %>% 
  remove_empty() %>% 
  select(-group) %>% 
  pivot_longer(cols = -c(regd, name),
               
               names_to = "subject",
               
               values_to = "marks"
               
               
               ) %>% 
  filter(str_detect(marks, "e$|c$|X") | is.na(marks)) %>% 
  mutate(subject = str_to_upper(subject),
         subject = str_replace(subject, "_", " ")) %>% 
  
  bind_rows(
updated %>% 
  filter(group == "B") %>% 
  remove_empty() %>% 
  select(-group) %>% 
  pivot_longer(cols = -c(regd, name),
               
               names_to = "subject",
               
               values_to = "marks"
               
               
  ) %>% 
  filter(str_detect(marks, "e$|c$|X") | is.na(subject)) %>% 
  mutate(subject = str_to_upper(subject),
         subject = str_replace(subject, "_", " "))
) %>% 
  
  write_csv("final.csv")
