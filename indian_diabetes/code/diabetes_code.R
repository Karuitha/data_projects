##########################################################################################
## Logistic regression project
## Indian diabetes project
## John Karuitha

##########################################################################################
## Load required packages ----

library(tidyverse)
library(tidymodels)
library(GGally)
library(Amelia)
library(tidymodels)

doParallel::registerDoParallel()
##########################################################################################

## Load the data ----

diabetes <- read_csv("https://raw.githubusercontent.com/Karuitha/Datasets/master/pima-indians-diabetes.data.csv",
                     
                     col_names = FALSE)

## Add names to the dataset
names(diabetes) <- c("times_pregnant", "plasma_glucose", "blood_pressure", "triceps_skin", 
                     
                     "insulin", "bmi", "diabetes_pedigree", "age", "class")

## Convert the diagnosis column into a factor 
diabetes <- diabetes %>% 
        
        mutate(class = factor(class, levels = c(1, 0), 
                              
                              labels = c("Positive", "Negative")))

##########################################################################################
## Explore the dataset ----

head(diabetes)
str(diabetes)

########################
## Check for missingness

Amelia::missmap(diabetes)

##########################################################################################
## Modelling ----
### Training & test set split 

split_object_diabetes <- initial_split(diabetes, prop = 0.6, strata = class)

#########################
### Training set 

train_diabetes <- split_object_diabetes %>% training()

### Testing set 
test_diabetes <- split_object_diabetes %>% testing()

##########################################################################################
## Data visualization on training set ----

train_diabetes %>% 
        
        GGally::ggpairs(columns = 1:8, mapping = aes(col = class), 
                        
        title = "Correlation Matrix for Features in the Diabetes Training Dataset") + 
        
        scale_color_manual(values = c("skyblue", "red")) + 
        
        scale_fill_manual(values = c("skyblue", "red"))

##########################################################################################
## Modelling ----
## Logistic regression  model 
#################################
### Set up the model 

logit_model <- logistic_reg() %>% 
        
        set_engine("glm") %>% 
        
        set_mode("classification")

##################################
### Run the model 

logit_output <- logit_model %>% 
        
        fit(class ~ ., data = train_diabetes)

#################################
### Tidy  the model output 

tidy_logit_output <- logit_output %>% 
        
        tidy()

##########################################################################################
### Do predictions ----
###############################
#### Class predictions

test_diabetes <- logit_output %>% 
        
        predict(new_data = test_diabetes) %>% 
        
        bind_cols(test_diabetes)

#################################
#### Probability predictions 
test_diabetes <- logit_output %>% 
        
        predict(new_data = test_diabetes, type = "prob") %>% 
        
        bind_cols(test_diabetes)

##########################################################################################
## Do evaluation metrics  ----

#################################
### Confusion matrix

test_diabetes %>% 
        
        conf_mat(truth = class, estimate = .pred_class) %>% 
        
        summary()

##################################
### metric set of sensitivity, specificity, accuracy, balanced accuracy, and f_meas

my_metrics <- metric_set(sens, spec, accuracy, bal_accuracy, f_meas)

test_diabetes %>% 
        
        my_metrics(truth = class, estimate = .pred_class)

##################################
### Compute the ROC

test_diabetes %>% 
        
        roc_auc(truth = class, estimate = .pred_Positive)

##################################
### Plot the confusion matrix 

test_diabetes %>% 
        
        conf_mat(truth = class, estimate = .pred_class) %>% 
        
        autoplot(type = "heatmap")

test_diabetes %>% 
        
        conf_mat(truth = class, estimate = .pred_class) %>% 
        
        autoplot(type = "mosaic")

####################################
### Plot the area under curve

test_diabetes %>% 
        
        roc_curve(truth = class, estimate = .pred_Positive) %>% 
        
        autoplot()
##########################################################################################
## Decision tree model
### Set up a decision tree model ----------------------------------------------------------------
my_tree_model  <- decision_tree(

        tree_depth = tune(),

        cost_complexity = tune(),

        min_n = tune()
) %>%

        set_engine("rpart") %>% 

        set_mode("classification")

## Set up cross validation for tuning parameters ----------------------------------------------------------------
my_folds  <- vfold_cv(train_diabetes, v = 5, strata = class)

my_folds

## Set up a tune grid model ----------------------------------------------------------------
tree_grid <- dials::grid_regular(tree_depth(), cost_complexity(), min_n(), levels = 5)

tree_grid

## Run the tuning model on the folds 

trees_rs <- tune_grid(my_tree_model, 
                      
                      class ~ .,
                      
                      resamples = my_folds,
                      
                      grid = tree_grid,
                      
                      metrics = metric_set(spec, sens, roc_auc)
                      
                      )

trees_rs %>% collect_metrics() %>% 
        
        arrange(desc(mean))

## Plot the trees
autoplot(trees_rs) + theme_light(base_family = 'IBMPlexSans')

## Generate best tuning parameters and metrics 
show_best(trees_rs, "spec")

show_best(trees_rs, "sens")

show_best(trees_rs, "roc_auc")

## Finalize model by selecting the best tuning parameters 

final_tree <- finalize_model(my_tree_model, select_best(trees_rs, "spec"))


## Run the model ----

my_final_tree_model <- workflow() %>% 
        
        add_model(final_tree) %>% 
        
        add_formula(class ~ .) %>% 
        
        fit(data = train_diabetes)

my_final_tree_model

## Predict on the test set

tree_prediction <- my_final_tree_model %>% 
        
        predict(new_data = test_diabetes) %>% 
        
        bind_cols(test_diabetes %>% select(class))

tree_prediction <- my_final_tree_model %>% 
        
        predict(new_data = test_diabetes, type = "prob") %>% 
        
        bind_cols(tree_prediction)

## Evaluation 

### Draw the tree 

my_tree_to_plot <- my_final_tree_model %>% 
        
        extract_fit_parsnip()
        
rpart.plot::rpart.plot(my_tree_to_plot$fit)

## Compute the metrics 

my_final_tree_metrics <- metric_set(sens, spec, roc_auc, f_meas, bal_accuracy)


tree_prediction %>% 
        
        my_final_tree_metrics(truth = class, estimate = .pred_class, .pred_Positive)


tree_prediction %>% 
        
        roc_curve(truth = class, .pred_Positive) %>% 
        
        autoplot()

tree_prediction %>% 
        
        conf_mat(truth = class, .pred_class) %>% 
        
        autoplot(type = "heatmap")

tree_prediction %>% 
        
        conf_mat(truth = class, .pred_class) %>% 
        
        summary()
