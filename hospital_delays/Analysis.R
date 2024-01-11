## ----set up, include=TRUE----
# load package manager- install it if not already installed
if (!require(pacman)) {
  install.packages("pacman")
}

## Download (when not available) and load required packages-------
pacman::p_load(
  tidyverse, janitor, skimr,
  kableExtra, readxl, conflicted,
  GGally, corrplot, ggthemes,
  randomForest, vip, stargazer,
  naivebayes, e1071, caret,
  kernlab, styler
)

## Set numbers digits and plots theme----------
options(digits = 3)
theme_set(ggthemes::theme_clean())

## ---------------------------
## Load the data ----
waiting <- readxl::read_xlsx("Hw_data.xlsx") |>
  janitor::clean_names()

## Overview of the data
glimpse(waiting)

## ---------------------------
# Describing the variables in the raw data ----
tribble(
  ~Variable, ~Description,
  "date_and_time",
  "Time and date of patient arrival.",
  "number_of_ed_beds",
  "Number of beds in the emergency department.",
  "number_of_ip_beds",
  "Number of inpatient beds.",
  "number_of_ed_pts",
  "Number of emergency department patients.",
  "number_of_ed_pts_waiting_ip_bed",
  "Number of emergency department patients waiting for bed",
  "number_of_critical_care_pts_display",
  "Number of critical care patients display",
  "door_to_bed_time_for_last_ed_patient",
  "Door to bed time for last patient in emergency department.",
  "longest_admit_time_waiting_in_ed",
  "Longest admission time for patient waiting in emergency department."
) |>
  kbl(booktabs = TRUE, caption = "Variables Description") |>
  kable_classic(
    full_width = TRUE,
    latex_options = "hold_position"
  )


## ---------------------------
## Check missing values
waiting |>
  sapply(is.na) |>
  colSums() |>
  tibble(variables = names(waiting), missing = _) |>
  kbl(booktabs = TRUE, caption = "Missing values") |>
  kable_classic(
    full_width = FALSE,
    latex_options = "hold_position"
  )


## ---------------------------
## Check duplicate observations
waiting |>
  janitor::get_dupes()


## ---------------------------
## Summary statistics for the data
waiting |>
  select(-date_and_time) |>
  skimr::skim_without_charts() |>
  select(-complete_rate, -n_missing) |>
  set_names(c(
    "no", "variable", "Mean", "SD", "Min",
    "Q1", "Median", "Q3", "Max"
  )) |>
  select(-no) |>
  kbl(booktabs = TRUE, caption = "Data Summary") |>
  kable_classic(
    full_width = FALSE, latex_options = "hold_position"
  )


## ---- fig.cap = "Pairs Plot for Raw Data"----
## Pairs plotof the raw data
waiting |>
  select(
    -date_and_time, -number_of_ed_beds,
    -number_of_ip_beds
  ) |>
  GGally::ggpairs(title = "Variables Pairs Plots")


## ---------------------------
## Remove the columns with constant values
## These are; number_of_ed_beds, number_of_ip_beds.
waiting <- waiting |>
  remove_constant()


## ---------------------------
## Creating new variables, day of week, hour of day, and wait_cat
## wait_cat bins the waiting times into 9 categories
waiting <- waiting |>
  mutate(
    week_day = wday(date_and_time, label = FALSE),
    hour_day = hour(date_and_time)
  ) |>
  select(-date_and_time) |>
  mutate(
    hour_day = factor(hour_day),
    week_day = factor(week_day)
  ) |>
  mutate(wait_cat = cut(longest_admit_time_waiting_in_ed,
    breaks = 9
  ))


## Distribution of patient wait times
waiting |> ggplot(aes(x = longest_admit_time_waiting_in_ed)) +
  geom_histogram(color = "black", fill = "green") +
  geom_density(aes(y = ..count..),
  color = "blue",
  linewidth = 2) +
  labs(
    x = "Wait Time", y = "Count",
    title = "Distribution of Wait Time"
  )


## Waiting time by categories
waiting |>
  count(wait_cat) |>
  ggplot(mapping = aes(x = wait_cat, y = n)) +
  geom_col(fill = 'green',
  color = "black") +
  labs(
    x = "length of Wait", y = "Count",
    title = "Waiting Times in Emergency Departments"
  ) +
  coord_flip()


## ---------------------------
## Patients by day of the week
waiting |>
  count(week_day) |>
  kbl(booktabs = TRUE, caption = "Patients by Day of Week") |>
  kable_classic(
    full_width = FALSE,
    latex_options = "hold_position"
  )


## ---------------------------
## Patients by hour of day
waiting |>
  count(hour_day) |>
  kbl(booktabs = TRUE, caption = "Patients by Hour of Day") |>
  kable_classic(
    full_width = FALSE,
    latex_options = "hold_position"
  )


## Run the regression model
lm_model <- lm(longest_admit_time_waiting_in_ed ~ . - wait_cat, data = waiting)


## Generate regression model output for either HTML or PDF output

if (knitr::is_html_output()) {
  stargazer::stargazer(lm_model,
    type = "latex",
    title = "Regression Output"
  )
} else {
  stargazer::stargazer(lm_model,
    type = "latex",
    title = "Regression Output",
    font.size = "tiny"
  )
}


## Create a dataset for Naive Bayes and SVM Models

final_data <- waiting |>
  select(-longest_admit_time_waiting_in_ed) |>
  data.frame()


# Support Vector Machines Model
# Create cross validation folds
train_control <- trainControl(
  method = "repeatedcv", number = 10, repeats = 3
)
# Fit the model
svm_model <- train(wait_cat ~ .,
  data = final_data,
  method = "svmLinear", trControl = train_control,
  preProcess = c("center", "scale"), 
  tuneGrid = expand.grid(C = seq(0, 2,
    length = 20
  ))
)
# View the model
plot(varImp(svm_model))


## Naive Bayes Model
## Create a hyperparameter tuning grid
Grid <- data.frame(usekernel = FALSE, laplace = 0, adjust = 1)
## Run the Naive Bayes model
mdl <- train(wait_cat ~ .,
  data = final_data,
  method = "naive_bayes", trControl = trainControl(method = "none"),
  tuneGrid = Grid
)
## Plot the variable importance
plot(varImp(mdl))


## Random Forest Model
## Set random seed for reproducibility
set.seed(123)

## Run the random forest model
rf_model <- randomForest(longest_admit_time_waiting_in_ed ~ .
  - wait_cat, data = waiting)

summary(rf_model)


## Variable importance plots for the random forest model

vip(rf_model) +
  labs(title = "Variable Importance: Random Forest Model")


## ---------------------------
sessionInfo()
