# suppose we were to just guess the chance that 
# a patient will be admitted late. 
# How often would we get it right. 
# In this case, we guess the most prevalent class
# of delays
# 506 patients in this category.
506/594 * 100

## Guess work would give an accuracy of 85.2%.
## None of our models reach this level of accuracy.
## This is the danger of using accuracy.
## rather use specificity
## Chance of a patient getting delayed 
## when the model actually predicts a delay.

# ------------------------------------------------------
## ----set up, include=TRUE----
# load package manager- install it if not already installed
if (!require(pacman)) {
  install.packages("pacman")
}

# ------------------------------------------------------
## Download (when not available) and load required packages-------
pacman::p_load(
  tidyverse, janitor, skimr,
  kableExtra, readxl, conflicted,
  GGally, corrplot, ggthemes,
  randomForest, vip, stargazer,
  naivebayes, e1071, caret,
  kernlab, styler, tidymodels,
  doParallel, discrim, 
  NeuralNetTools, ranger,
  themis
)

# -------------------------------------------------------
## Set numbers digits and plots theme--------------------
options(digits = 3)
theme_set(ggthemes::theme_clean())


## -------------------------------------------------------
## Load the data ----
waiting <- readxl::read_xlsx("Hw_data.xlsx") |>
  janitor::clean_names()

## Overview of the data
glimpse(waiting)


## ------------------------------------------------------
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


## ------------------------------------------------------
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

## ------------------------------------------------------
## Check duplicate observations
waiting |>
  janitor::get_dupes()


## --------------------------------------------------------
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

# -------------------------------------------------------
## ---- fig.cap = "Pairs Plot for Raw Data"----
## Pairs plot of the raw data
waiting |>
  select(
    -date_and_time, -number_of_ed_beds,
    -number_of_ip_beds
  ) |>
  GGally::ggpairs(title = "Variables Pairs Plots")


## --------------------------------------------------------
## Remove the columns with constant values
## These are; number_of_ed_beds, number_of_ip_beds.
waiting <- waiting |>
  remove_constant()


## --------------------------------------------------------
## Creating new variables, day of week, hour of day, and wait_cat
## wait_cat bins the waiting times into 4 categories
## The wait_cat variable has 3 levels 
## 0-10minutes, 10-20 minutes, 20-30 minutes waiting
## We use these bins as the target variable
## --------------------------------------------------------
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
    breaks = 3
  ))


## Distribution of patient wait times
waiting |> 
  ggplot(aes(x = longest_admit_time_waiting_in_ed)) +
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


## -------------------------------------------------------
## Patients by day of the week
waiting |>
  count(week_day) |>
  kbl(booktabs = TRUE, caption = "Patients by Day of Week") |>
  kable_classic(
    full_width = FALSE,
    latex_options = "hold_position"
  )


## ------------------------------------------------------------
## Patients by hour of day
waiting |>
  count(hour_day) |>
  kbl(booktabs = TRUE, caption = "Patients by Hour of Day") |>
  kable_classic(
    full_width = FALSE,
    latex_options = "hold_position"
  )

## Create a training and testing set -----
set.seed(100)
split <- rsample::initial_split(waiting, 
                                prop = 0.8, 
                                strata = wait_cat)

train_set <- split |> 
  training() |>
  select(-longest_admit_time_waiting_in_ed)
  

test_set <- split |> 
  testing() |> 
  select(-longest_admit_time_waiting_in_ed)

## Create a recipe ----
my_recipe <- recipes::recipe(wait_cat ~ ., 
                             data = train_set) |>
  themis::step_upsample(all_outcomes(), 
                        over_ratio = 1) |> 
  prep()

train_set <- my_recipe |> 
  bake(new_data = NULL)

## Create a workflow to use in all models ----
my_wf <- workflow() |>
  add_recipe(my_recipe)

# -----------------------------------------------------------
## Linear model ----
# -----------------------------------------------------------
## Run the regression model
linear_model <- lm(longest_admit_time_waiting_in_ed ~ . - wait_cat, 
               data = waiting)

summary(linear_model)

# -----------------------------------------------------------
#############################################################
## Logit model ----
#############################################################
## Create a logit model ----

logit_model <- multinom_reg() |>
  set_engine("nnet") |>
  set_mode("classification")

## Fit a logit model ----
final_lm <- my_wf |>
  add_model(logit_model) |>
  fit(data = train_set)

## Make predictions on the test set 
lm_prediction <- final_lm |>
  augment(new_data = test_set)

## Confusion matrix ----
lm_prediction |>
  conf_mat(truth = wait_cat,
       estimate = .pred_class) |>
  autoplot('heatmap') + 
  labs(title = "Confusion Matrix for Logit Model")

## get the metrics
lm_prediction |>
  conf_mat(truth = wait_cat,
                estimate = .pred_class) |>
  summary() |>
  kbl(booktabs = TRUE, 
      caption = "Logit Model Summary") |>
  kable_classic()

# ---------------------------------------------------
#####################################################
## Run the SVM Radial model ----
## In this case, I run the model as a classification 
## Algorithm
#####################################################
## Define a redial SVM algorithm
svm_model <- svm_rbf(cost = tune(), 
        rbf_sigma = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

# Define a grid to vary cost and sigma
svm_grid <- expand.grid(cost = seq(0, 2, length = 20),
                        rbf_sigma = c(0.1, 1, 2, 3))

# Perform CV splits on training data
cv_folds <- vfold_cv(data = train_set, 
                     v = 2)

# Workflow for tuning
svm_wf <- my_wf |> 
  # add the tuning specifications
  add_model(svm_model)
  

# Tuning is faster in parallel
all_cores <- parallel::detectCores(logical = FALSE)
cl <- makeCluster(all_cores)
registerDoParallel(cl)

# Start Tuning
set.seed(123)
svm_results <-  svm_wf %>% 
  tune_grid(resamples = cv_folds,
            grid = svm_grid)

# Select the best model ----
svm_tune <- svm_results %>%
  select_best(metric = "accuracy")

# Finalize workflow ----
# fit to the train
svm_final_model <- svm_wf %>%
  finalize_workflow(svm_tune) %>%
  fit(data=train_set)

## Predict on the test set ----
svm_pred <- svm_final_model |>
  augment(new_data = test_set)

# Evaluate the model ----
svm_pred |>
  conf_mat(truth = wait_cat, estimate = .pred_class) |> 
  autoplot(type = 'heatmap') + 
  labs(title = "Confusion Matrix Heat Map- SVM Model")

## metrics
svm_pred |>
  conf_mat(truth = wait_cat, estimate = .pred_class) |>
  summary() |> 
  kbl(booktabs = TRUE, 
      caption = "SVM Model Summary") |>
  kable_classic()

# ---------------------------------------------------
#####################################################
## Run a Naive Bayes Model
## I run the Naive Bayes as a classification algorithm.
#####################################################

bayes_model <- naive_Bayes(
  smoothness = tune::tune(),
  Laplace = 1
) |>
  
  set_engine("naivebayes") |>
  set_mode("classification")

##Naive Bayes tunegrid 
naive_grid <- expand_grid(smoothness = seq(0, 10, 1))

## Define worlflow ----
nb_wf <- my_wf |>
  add_model(bayes_model)

## Tune the model ----
set.seed(123)
final_tune <- nb_wf |> 
  tune_grid(resamples = cv_folds,
            grid = naive_grid)

## Select best model ----
best <- final_tune |> 
  select_best(metric = "accuracy")

## Finalize workflow
final_nb_model <- nb_wf |> 
  finalize_workflow(best) |>
  fit(data=train_set)

final_nb_model |> 
  pull_workflow_fit() |> 
  vip()

## Predict on test set 
nb_predictions <- final_nb_model |> 
  augment(new_data = test_set)

## Confusion matrix ----
nb_predictions |>
  conf_mat(truth = wait_cat, estimate = .pred_class) |>
  autoplot("heatmap") + 
  labs(title = "Confusion Matrix for Naive Bayes Model")

## Summary Statistics ----
nb_predictions |>
  conf_mat(truth = wait_cat, estimate = .pred_class) |>
  summary() |> 
  kbl(booktabs = TRUE, 
      caption = "Naive Bayes Model Summary") |>
  kable_classic()

# ----------------------------------------------------------
############################################################
## Random Forest Model
## Run the random forest model
############################################################
rf_spec <- rand_forest(
  mtry = tune(),
  trees = 1000
) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "impurity")

## Create workflow ----
rf_wf <- my_wf |> 
  add_model(rf_spec)

## Create a parameter tuning grid 
rf_grid = expand_grid(mtry = seq(1, 7, 1))

## Tune the parameters ----
set.seed(123)
rf_tune <- rf_wf |> 
  tune_grid(resamples = cv_folds,
            grid = rf_grid)

## Select best model ----
rf_best <- rf_tune |> 
  select_best("accuracy")

## Finalize workflow
final_rf_model <- rf_wf |> 
  finalize_workflow(rf_best) |> 
  fit(data = train_set)

## Predict on test set ----
rf_pred <- final_rf_model |> 
  augment(new_data = test_set)

## RF metrics ----
rf_pred |> 
  conf_mat(truth = wait_cat, estimate = .pred_class) |> 
  autoplot("heatmap") + 
  labs(title = "Confusion Matrix for Random Forest Model")

rf_pred |> 
  conf_mat(truth = wait_cat, estimate = .pred_class) |> 
  summary() |> 
  kbl(booktabs = TRUE, 
      caption = "Random Forest Model Summary") |>
  kable_classic()

## Variable importance plots for the random forest model
final_rf_model |>
extract_fit_parsnip() |>
  vip(geom = "point") + 
  labs(title = "Random Forest Model Variable Importance") 

## ---------------------------
sessionInfo()

