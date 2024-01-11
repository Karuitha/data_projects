## ----setup, include=FALSE--------------------
## Load the required packages 
###################################################
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)

if(!require(pacman)){
    install.packages("pacman")
}

##################################################
pacman::p_load(tidyverse, janitor, skimr, corrplot, 
               Amelia, xtable, tidymodels, KableExtra, 
               rpart, rpart.plot,  arules, arulesViz, gt)

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
## read in the data 
cosmetics <- read_csv("Cosmetics.csv") %>% 
    clean_names() %>% 
    mutate(invoice_number = paste0("c", 1:nrow(.))) %>% 
    pivot_longer(cols = -invoice_number, 
                 names_to = "item", 
                 values_to = "bought") %>% 
    filter(bought == 1) %>% 
    mutate(invoice_number = factor(invoice_number)) %>% 
    group_by(invoice_number) %>% 
    filter(!duplicated(item)) %>% 
    ungroup()

## Split the data by invoice number
my_bundles <- split(cosmetics$item, cosmetics$invoice_number)


## --------------------------------------------
trans <- as(my_bundles, "transactions")
summary(trans)


## ---- fig.cap = "Most Frequently Bought Items"----
itemFrequency(trans)
itemFrequencyPlot(trans, topN = 10)


## --------------------------------------------
my_rules <- apriori(data = trans, 
                    parameter = list(support = 0.01, 
                                   confidence = 0.8,
                                   minlen=2))
summary(my_rules)


## --------------------------------------------
as(my_rules, "data.frame") %>% 
    arrange(desc(lift)) %>% 
    head(8) %>% 
    gt(caption = "Top Rules by Lift")
#inspect(sort(my_rules, by = "lift"))

