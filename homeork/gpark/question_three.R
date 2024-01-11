## ----setup, include=FALSE--------------------
###################################################
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)

## Load required data

if(!require(pacman)){
    install.packages("pacman")
}

## Load required packages 
##################################################
pacman::p_load(tidyverse, janitor, skimr, corrplot, 
               Amelia, xtable, tidymodels, KableExtra, rpart, rpart.plot)

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
## read the data and clean names
flights <- read_csv("FlightDelays.csv") %>% clean_names()

## Split the data into training and testing set
flights_split <- initial_split(flights, prop = 0.6, strata = flight_status)


flights_training <- flights_split %>% training()
flights_testing <- flights_split %>% testing()


## --------------------------------------------
## Fitting the regression tree on training data
flights_tree <- rpart(flight_status ~ carrier + distance + weather + day_week + day_of_month, data = flights_training, method = 'class', 
                      control = rpart.control(cp = 0.001,
                                              maxdepth = 8))
## Plotting the tree
rpart.plot(flights_tree)


## --------------------------------------------
## Prunning the tree
pruned_tree <- prune(flights_tree, cp = 0.01)

## Plot the prunned tree
rpart.plot(pruned_tree)


## --------------------------------------------
## Create another classification tree, cp = 0.001, depth = 6
another_flights_tree <- rpart(flight_status ~ carrier + distance + weather + day_week + day_of_month, data = flights_training, method = 'class', 
                      control = rpart.control(cp = 0.001,
                                              maxdepth = 6))

## Plot the tree
rpart.plot(another_flights_tree)


## --------------------------------------------
## Prediction on the training set
train_prediction <- predict(another_flights_tree, type = "class")

## Confusion matrix on the training set
flights_training %>% 
    select(flight_status) %>% 
    mutate(flight_status = factor(flight_status, labels = c("delayed", "ontime"))) %>% 
    bind_cols(train_prediction) %>% 
    conf_mat(truth = flight_status, estimate = `...2`)



## --------------------------------------------
## Prediction on the testing set
train_prediction_test <- predict(another_flights_tree, newdata = flights_testing, type = "class")

## Confusion matrix on the testing set
flights_testing %>% 
    select(flight_status) %>% 
    mutate(flight_status = factor(flight_status, labels = c("delayed", "ontime"))) %>% 
    bind_cols(train_prediction_test) %>% 
    conf_mat(truth = flight_status, estimate = `...2`)

