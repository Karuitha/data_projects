## ----setup, include=FALSE--------------------
## Load required packages 
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)

if(!require(pacman)){
    install.packages("pacman")
}

pacman::p_load(tidyverse, janitor, skimr, corrplot, 
               Amelia, xtable, tidymodels)

## Set theme and digits
#theme_set(theme_few())

options(digits = 2)

## Table formatting function
formatting_function <- function(data, caption = "Table 1", 
                                full_width = FALSE){
    library(kableExtra)
    data %>% 
        kbl(booktabs = TRUE, caption = caption) %>% 
        kable_classic(full_width = full_width,
                      latex_options = "hold_position")
}
if(!require(firatheme)){
remotes::install_github("vankesteren/firatheme")}



## ---- fig.width = 7--------------------------
#Q1.1
## Reading in the data 
housing <- read_csv("BostonHousing.csv")

## Get correlations between all variables
my_corr <- housing %>% 
    cor() %>% round(2)

## Create a correlation matrix
upper<- my_corr
upper[upper.tri(my_corr)]<-""
upper<-as.data.frame(upper)
upper


## --------------------------------------------
## Plot the correlation matrix
housing %>% 
    cor() %>% 
    corrplot(method = "circle", 
             type = "lower",
             diag = FALSE)


## --------------------------------------------
## Q1.3
## Allows reproducibility
set.seed(134)

## Create a 60-40 split of the data
split_object <- initial_split(housing, prop = 0.6)
training_set <- split_object %>% training()
testing_set <- split_object %>% testing()


## --------------------------------------------
##Regression of MEDV as a function of CRIM, CHAS, and RM
## MEDV as a function of CRIM, CHAS, and RM

my_first_reg <- lm(MEDV ~ CRIM + CHAS + RM, data = training_set)

summary(my_first_reg)


## --------------------------------------------
## Predicting on the testing / validation test
my_first_reg_prediction <- predict(my_first_reg, newdata = testing_set)


## --------------------------------------------
## Run a second regression
my_second_reg <- lm(MEDV ~ LSTAT + INDUS + NOX, data = training_set)

summary(my_second_reg)

## Make predictions on the second regression
my_second_reg_prediction <- predict(my_second_reg, newdata = testing_set)


## --------------------------------------------
## Create a data frame of actual values
## And predcitions from both models
predictions <- testing_set %>% 
    select(MEDV) %>% 
    bind_cols(my_first_reg_prediction) %>% 
    bind_cols(my_second_reg_prediction) %>% 
    set_names(c("median_price", "first_reg", "second_reg"))



## --------------------------------------------
## First regression metrics
sqrt(mean((predictions$median_price - predictions$first_reg)^2))

## Second regression 
sqrt(mean((predictions$median_price - predictions$second_reg)^2))


## --------------------------------------------
## mean absolute error
mean(abs(predictions$median_price - predictions$first_reg))


mean(abs(predictions$median_price - predictions$second_reg))


## --------------------------------------------
## Other metrics
glance(my_first_reg)
glance(my_second_reg)

