## load the libraries
library(tidyverse)

## Create a dataset
gpa <- runif(1000, 3, 4) |> sort(decreasing = FALSE)

admit <- if_else(gpa > 3.5, 1, 0)

admit_data <- data.frame(gpa, admit)
head(admit_data)


## Plot the data ----
admit_data %>% 
  
  ggplot(mapping = aes(x = gpa, y = admit)) + 
  
  geom_point() + 
  
  geom_smooth(method = "lm", 
              
              se = FALSE, color = "red")

## fit a logistic model ----
logistic_model <- glm(admit ~ gpa, family = "binomial", maxit = 1)

## I make predictions of the data ----
predictions_log <- predict(logistic_model, type = "response")

## Join predcitions to dataset ----
admit_data$pred <- predictions_log

## Plot the logisticfunction 
admit_data %>% 
  
  ggplot(mapping = aes(x = gpa, y = pred)) + 
  
  geom_point(alpha = 0.1, color = "blue") + 
  
  geom_point(mapping = aes(x = gpa, y = admit)) + 
  
  theme_minimal()
