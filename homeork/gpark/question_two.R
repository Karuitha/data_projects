## ----setup, include=FALSE--------------------
###################################################
## Load required packages 
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)

if(!require(pacman)){
    install.packages("pacman")
}

##################################################
pacman::p_load(tidyverse, janitor, skimr, corrplot, 
               Amelia, xtable, tidymodels, KableExtra)

##################################################
## Set theme and digits

options(digits = 2)

## Table formatting function
formatting_function <- function(data, caption = "Table 1", 
                                full_width = FALSE){
    library(kableExtra)
    data %>% 
        kbl(booktabs = TRUE, caption = caption) %>% 
        kableExtra::kable_classic(full_width = full_width,
                      latex_options = "hold_position")
}

if(!require(firatheme)){remotes::install_github("vankesteren/firatheme")}



## --------------------------------------------
## Read in the data
banks <- read_csv("banks.csv")
## Training and testing set
bank_split <- initial_split(banks, prop = 0.6, 
                            strata = "Financial_Condition")

bank_training_set <- bank_split %>% training()
bank_testing_set <- bank_split %>% testing()
## Fit a logistic regression on the training set
set.seed(134)
bank_logit <- glm(Financial_Condition ~ . -Obs, 
                  family = binomial(link = 'logit'), 
                  data = bank_training_set)

## Summary of the regression model
summary(bank_logit)



## --------------------------------------------
## Predicted probability
set.seed(134)
predict(bank_logit, newdata = bank_testing_set, 
        type = "response")


## --------------------------------------------
## Get the predictions
## If prob >= 0.5 then 1, else 0
bank_testing_set %>% 
    select(Financial_Condition) %>% 
    mutate(predicted_probs = predict(bank_logit, newdata = bank_testing_set, 
        type = "response"),
        
        predicted_outcomes = if_else(predicted_probs >= 0.5, 1, 0)
        
        ) %>% 
    
## See the first 5 predictions
    head(5)

