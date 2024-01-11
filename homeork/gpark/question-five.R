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
               rpart, rpart.plot,  arules, arulesViz, 
               forecast, fpp2, fpp3, gt, ggthemes)

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

## plots themes
theme_set(theme_clean())



## --------------------------------------------
## Read in the data and see first six points
sales <- read_csv("DepartmentStoreSales.csv")
head(sales) %>% 
    gt(caption = "Sample Data")


## --------------------------------------------
## Create a time series data
my_sales_ts <- ts(sales, start = c(2000, 1), frequency = 4)

## View the data
print(my_sales_ts)

## Plot the data
autoplot(my_sales_ts[, "Sales"]) + 
    labs(title = "Quarterly Sales, 2000-2005", y = "Sales", x = "Year")


## --------------------------------------------
ggseasonplot(my_sales_ts[, "Sales"]) +
    labs(y =  "Sales",
         title = "Seasonal Plots")



## --------------------------------------------
ggsubseriesplot(my_sales_ts[, "Sales"]) + 
    labs(y =  "Sales",
         title = "Subseries Plots by Quarters")


## --------------------------------------------
## Decompose the data into level,seasons and trend
my_dec_ts <- decompose(my_sales_ts[, "Sales"])

## Plot the decomposition
autoplot(my_dec_ts)


## --------------------------------------------
## Create training and testing sets 
ts_training <- window(my_sales_ts, start = c(2000, 1), 
                      end = c(2004, 2))

ts_testing <- window(my_sales_ts, start = c(2004, 2))


## --------------------------------------------
## Fit a linear trend model
sales_model <- tslm(Sales ~ Quarter, data = ts_training)

## Summary of linear trend model
summary(sales_model)


## --------------------------------------------
## Forecast model with linear model
my_linear_forecast <- forecast::forecast(sales_model, newdata = ts_testing[, "Quarter"])

## Print my linear model forecast
my_linear_forecast





## --------------------------------------------
autoplot(my_linear_forecast) + 
    labs(title = "Forecasts from Linear Trend Model")


## --------------------------------------------
## Fit a model with season a trend 
my_trend_model  <- forecast::tslm(Sales ~ trend + season, data = ts_training)

## Summarise the model
summary(my_trend_model)

## A function to compute mean percent error
mape <- function(actual,pred){

  mape <- mean(abs((actual - pred)/actual))*100

  return (mape)

}



## --------------------------------------------
## Forecasting with the model with season a trend 
my_trend_forecast <- forecast::forecast(my_trend_model, newdata = ts_testing)

my_trend_forecast


## --------------------------------------------
autoplot(my_trend_forecast)


## --------------------------------------------
## capturing the forecasts
forecasts <- data.frame(
    
    Quarter = (nrow(my_sales_ts) - 6):nrow(my_sales_ts),
    linear_trend = c(65721, 66365, 67008, 67652, 68296, 68940, 69584),
    trend_season = c(65596, 85399, 58221, 60166, 67824, 87627, 60449)
)
## % Error for the linear trend model
mape(ts_testing[, "Sales"], forecasts$linear_trend)
## % Error for model with trend and seasons
mape(ts_testing[, "Sales"], forecasts$trend_season)


## --------------------------------------------
sales %>% 
    ggplot(aes(x = Quarter, y = Sales)) + 
    geom_line() + 
    geom_line(data = forecasts, aes(y = linear_trend),
              col = "blue") + 
    geom_line(data = forecasts, aes(y = trend_season),
              col = "red", linetype = "dashed") + 
    labs(title = "Comaring the Linear Trend Model and the Model with Season and Trend")

