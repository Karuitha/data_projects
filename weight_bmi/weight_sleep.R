## UK Age, BMI and sleep data

## Load required packages

my_packages <- c("tidyverse", "readxl", "janitor", "knitr")

if(!require(my_packages)){

install.packages(my_packages)
  
}

library(tidyverse)
library(readxl)
library(janitor)


theme_set(ggthemes::theme_clean())

##################################
## Read in the data
weight_data <- read_csv("dataset.csv") %>% 
  
  ## Clean names by removing special characters and capital letters
  janitor::clean_names()

#############################################################
##Get the first six rows of the dataset
head(weight_data) %>% 
  
  ## Convert the row names to upper case
  set_names(names(.) %>% str_to_upper()) %>% 
  
  ## Make a table
  knitr::kable(booktabs = TRUE, 
               
               ## Insert table title                            
               caption = "The First Six Rows of the BMI-Sleep-Age Data set from the UK") %>% 
  
  kableExtra::kable_styling(full_width = TRUE, bootstrap_options = "striped")

###################################################################

## structure of the dataset
str(weight_data)

## Checking for missing values

sapply(weight_data, is.na) %>% 
  
  colSums() %>% 
  
  tibble(Variables = names(weight_data), missing_values = .) %>% 
  
  mutate(Variables = (Variables %>% str_to_upper())) %>% 
  
  knitr::kable(booktabs = TRUE, caption = "Missing Values") %>% 
  
  kableExtra::kable_styling(full_width = TRUE, bootstrap_options = "striped")

#######################################################################
## Summary statistics
summary(weight_data)

######################################################################
## Checking for duplicates
weight_data %>% 
  
  filter(duplicated(.)) %>% 
  
  knitr::kable(booktabs = TRUE, caption = "Duplicate Values") %>% 
  
  kableExtra::kable_styling(full_width = TRUE, bootstrap_options = "striped")

####################################################################
## Full list of duplicates
weight_data %>% 
  
  filter(id %in% c(10, 11, 12, 41, 42, 43)) %>% 
  
  arrange(id) %>% 
  
  knitr::kable(booktabs = TRUE, caption = "Duplicate Values") %>% 
  
  kableExtra::kable_styling(full_width = TRUE, bootstrap_options = "striped")

#######################################################################
## Removing duplicated data

weight_data <- weight_data %>% 
  
  filter(!duplicated(.))

#############################################################################
## Summary statistics using skimr
weight_data %>% 
  
  select(-id) %>% 
  
  skimr::skim_without_charts() %>% 
  
  select(-complete_rate, -skim_type) %>% 
  
  rename(Variable = skim_variable, Mean = numeric.mean, 
         
         SD = numeric.sd, Missing = n_missing,
         
         Min = numeric.p0, Q1 = numeric.p25, Median = numeric.p50, 
         
         Q3 = numeric.p75, Max = numeric.p100) %>% 
  
  mutate(Variable = (Variable %>% str_to_upper())) %>% 
  
  knitr::kable(booktabs = TRUE, caption = "Summary Statistics") %>% 
  
  kableExtra::kable_styling(full_width = TRUE, bootstrap_options = "striped")

############################################################################
## make sleep an ordered factor

weight_data <- weight_data %>% 
  
  mutate(sleep = factor(sleep, ordered = TRUE, levels = c("5", "6", "7", "8", "9", "10")))


##########################################################################
## Frequency table for sleep

table(weight_data$sleep) %>% 
  
  knitr::kable(booktabs = TRUE, caption = "Frequency Table for Sleep") %>% 
  
  kableExtra::kable_styling(full_width = TRUE, bootstrap_options = "striped")

################################################################################
## Scatter plot of age vs BMI

weight_data %>% 
  
  ggplot(aes(x = bmi, y = age)) + 
  
  geom_point() + 
  
  theme_bw() + 
  
  labs(x = "BMI", y = "Age", title = "Scatterplot of Age vs BMI")

#######################################################################
## Boxplot of BMI vs sleep

weight_data %>% 
  
  ggplot(aes(x = fct_reorder(sleep, bmi, median), y = bmi, fill = sleep)) + 
  
  geom_boxplot(show.legend = FALSE) + 
  
  labs(x = "Sleep", y = "BMI", title = "BMI vs Sleep in Hours") + 
  
  theme_bw()

######################################################################
## Plot of BMI vs Age factored by Sleep

weight_data %>% 
  
  ggplot(aes(x = bmi, y = age, col = sleep)) + 
  
  geom_point(alpha = 0.6) + 
  
  geom_smooth(se = FALSE, method = "lm") + 
  
  labs(x = "BMI", y = "Age", title = "BMI vs Age Faceted by Sleep") + 
  
  theme_bw()
#####################################################################
