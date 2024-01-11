# title: "**HarvardX: PH125.9x: Data Science: Capstone: Predicting the Chance of a Patient Falling Using Data from China**"
# subtitle: "A Capstone Project for the `Professional Certificate in Data Science` offered by Harvard University (HarvardX) via EdX"
# author: "John King'athia Karuitha"
# date: "`r format(Sys.Date(), '%A %B %d, %Y')`"

################################################################################
# Download required packages ----
if(!require("pacman")){install.packages("pacman", repos = "http://cran.us.r-project.org")}
if(!require("tidyverse")){install.packages("tidyverse", repos = "http://cran.us.r-project.org")}
if(!require("plyr")){install.packages("plyr", repos = "http://cran.us.r-project.org")}
if(!require("readxl")){install.packages("readxl", repos = "http://cran.us.r-project.org")}
if(!require("data.table")){install.packages("data.table", repos = "http://cran.us.r-project.org")}
if(!require("RCurl")){install.packages("RCurl", repos = "http://cran.us.r-project.org")}
if(!require("janitor")){install.packages("janitor", repos = "http://cran.us.r-project.org")}
if(!require("Amelia")){install.packages("Amelia", repos = "http://cran.us.r-project.org")}
if(!require("GGally")){install.packages("GGally", repos = "http://cran.us.r-project.org")}
if(!require("ggthemes")){install.packages("ggthemes", repos = "http://cran.us.r-project.org")}
if(!require("caret")){install.packages("caret", repos = "http://cran.us.r-project.org")}
if(!require("corrplot")){install.packages("corrplot", repos = "http://cran.us.r-project.org")}
if(!require("psych")){install.packages("psych", repos = "http://cran.us.r-project.org")}
if(!require("gghalves")){install.packages("gghalves", repos = "http://cran.us.r-project.org")}
if(!require("mlogit")){install.packages("mlogit", repos = "http://cran.us.r-project.org")}
if(!require("rpart")){install.packages("rpart", repos = "http://cran.us.r-project.org")}
if(!require("rpart.plot")){install.packages("rpart.plot", repos = "http://cran.us.r-project.org")}
if(!require("e1071")){install.packages("e1071", repos = "http://cran.us.r-project.org")}
if(!require("modeest")){install.packages("modeest", repos = "http://cran.us.r-project.org")}
if(!require("skimr")){install.packages("skimr", repos = "http://cran.us.r-project.org")}
if(!require("tidymodels")){install.packages("tidymodels", repos = "http://cran.us.r-project.org")}
if(!require("gmnl")){install.packages("gmnl", repos = "http://cran.us.r-project.org")}
if(!require("nnet")){install.packages("nnet", repos = "http://cran.us.r-project.org")}
if(!require("stargazer")){install.packages("stargazer", repos = "http://cran.us.r-project.org")}
if(!require("themis")){install.packages("themis", repos = "http://cran.us.r-project.org")}
if(!require("tibble")){install.packages("tibble", repos = "http://cran.us.r-project.org")}

##############################################################
# SECTION 2
###############################################################
# Load required package----

library(pacman)
library(tidyverse)
library(plyr)
library(readxl)
library(data.table)
library(RCurl)
library(janitor)
library(Amelia)
library(GGally)
library(ggthemes)
library(caret)
library(corrplot)
library(psych)
library(gghalves)
library(mlogit)
library(rpart)
library(rpart.plot)
library(e1071)
library(modeest)
library(skimr)
library(tidymodels)
library(gmnl)
library(nnet)
library(stargazer)
library(themis)
library(tibble)
################################################################################
doParallel::registerDoParallel()
##############################################################
# SECTION 3
###############################################################
# Load the data ----

## Get the URL for the datafile
## The original source of the data is kagle.com, specifically https://www.kaggle.com/pitasr/falldata
## However, it is hard to download data straight from kagle without supplying login details
## hence, I loaded the data into my github account in the url below.

url <- "https://raw.githubusercontent.com/Karuitha/Final_Project_HarvardX/main/falldetection.csv"

## Download the dataset.

download.file(url = url, destfile = "fall.csv", method = "curl")

## load the dataset into R

fall <- read.csv("fall.csv") %>% 
  
  ## Clean the column names by removing capital letters, spaces and special characters
  janitor::clean_names()

## The dependent variable is activity, classified as follows
## Fall detection data set of Chinese hospitals of old age patients.

### 0- Standing
### 1- Walking
### 2- Sitting
### 3- Falling
### 4- Cramps
### 5- Running

## Independent variables

### time: monitoring time
### sl: sugar level
### eeg: eeg monitoring rate- 
## electroencephalogram (EEG) is a test that detects electrical activity in the brain.
### bp blood pressure
### hr: heart beat rate
### circulation: blood circulation

# Convert the dependent variable into a factor with category falling as the base
fall$activity <- factor(fall$activity, levels = c(3, 0, 1, 2, 4, 5))


## **Training set/ Test set split**

set.seed(123, sample.kind = "Rounding")

# Specify the index to split data into training and testing set
index <- initial_split(fall, prop = 0.7, strata = activity)

# Sopecify the training set
fall_train <- training(index)

# Dimensions of the training set
dim(fall_train) # 11470 observations of 7 variables

# Get the testing set
fall_test <- testing(index)

# Dimensions of the testing set
dim(fall_test) # 4912 observations of 7 variables

## **Exploratory Data Analysis- Training set**
### Data Structure 
## Overview of the training data.
## Structure of the training data
str(fall_train)

## First 6 observations of the training dataset
head(fall_train) %>% knitr::kable(caption = "First Six Observations of the Training Set")

## last 6 observations of the training dataset
tail(fall_train) %>% knitr::kable(caption = "Last Six Observations of the Training Set")

## Number of rows in the training dataset
nrow(fall_train)

## Number of columns in the training dataset
ncol(fall_train)

### Missing Data
# **************************************
# Exploratory data analysis: missing data
## Check for missing data.
sapply(fall_train, is.na) %>% 
  
  ## Get the colsums of the logical dataframe.
  ## The colsums represent missing data for each column.
  colSums() %>% 
  
  ## Make a tibble of column names and missing values.
  dplyr::tibble(variables = names(fall), missing = .) %>% 
  
  ## Arrange missing values in descending order of missingness
  dplyr::arrange(desc(missing)) %>% 
  
  ## Get the top 7 (number of columns)
  ## Our tibble has no missing data.
  head(7) %>% 
  
  ## make a nice table
  knitr::kable(caption = "Missing Data in the Training Set")

## Visualizing missingness of data 
Amelia::missmap(fall_train, main = "Figure 1: Missingness Map- Training Set") 
## Again there is no missing data

## Summary statistics for the dependent variables
summary(fall_train$activity) %>% knitr::kable(caption = "Summary of Dependent Variable")

##***************************************************
# Exploratory data analysis: summary statistics
fall_train %>% 
  
  ## Deselect the activity column
  select(-activity) %>% 
  
  ## Make a table of summary statistics
  skimr::skim() %>%
  
  ## Remove some uninformative columns
  dplyr::select(-contains(c("missing", "complete", "hist", "skim_type"))) %>% 
  
  ## Rename remaining columns
  dplyr::rename(Variable = skim_variable, 
                
                Mean = numeric.mean, SD = numeric.sd, 
                
                Min = numeric.p0, Q1 = numeric.p25, 
                
                Median = numeric.p50, 
                
                Q3 = numeric.p75, Max = numeric.p100) %>% 
  
  ## make a nice table
  knitr::kable(caption = "Summary Statistics for the `Fall` dataset", align = "l")

### Data Visualization
# Exploratory data analysis: data visualization
fall_train[,-1] %>% GGally::ggpairs() + 
  
  ## Add title and caption
  labs(title = "Figure 2: Correlation and Distribution of Independent Variables", 
       
       caption = "Source: Author's Computations") + 
  
  ## Add themes and adjust the font size plot title
  ggthemes::theme_clean() + theme(plot.title = element_text(size = 8)) + 
  
  ## Adjust font sizes of axis text
  theme(axis.text = element_text(size = 6))

### Correlation among the variables
## Visualize the correlation 
## Run correlation on independent variables and call the corrplot
cor(fall_train[,-1]) %>% corrplot::corrplot(method = "color",
                                            
## Specify the corrplot type and add title
type = "lower", title = "Figure 3: A Visual of the Correlation Matrix - Training Set")

## Check for possible class imbalance on the dependent variable
## Make a table of class counts 
table(fall_train$activity)

## Make a proportionate table of class counts
prop.table(table(fall_train$activity)) 

# Class 1 has a problem of low prevalence.

## **Method**
### Principal Components Analysis and Handling Class Imbalance
## Create a PCA recipe
pca_recipe <- recipe(activity ~ ., data = fall_train)

## Do the PCA analysis
pca_trans <- pca_recipe %>% 
  
  ## Ensure all predictors have a mean of zero
  step_center(all_predictors()) %>% 
  
  ## Ensure all predictors have a standard deviation of one
  step_scale(all_predictors()) %>% 
  
  ## Run the principal components analysis
  step_pca(all_predictors()) %>% 
  
  ## Apply adjustment to deal with outliers
  step_spatialsign(all_predictors()) %>% 
  
  ## Adjust data to deal with missing values
  step_upsample(activity, over_ratio = 1) %>% 
  
  ## Appply alll the steps above and generate new dataset.
  prep()


## Check the names of the dependent variables
names(pca_trans)

## Access the standard deviations 
sdev <- pca_trans$steps[[3]]$res$sdev

# View the standard deviations output
sdev

## The contribution of PCA to the total variation
variance_explained <- (sdev ^ 2) / sum(sdev ^ 2)

# The variance explained by each principal component
variance_explained

########################################################################
## Plot a scree plot
## Create dataframe of principal components and variance explained by each PC>
data.frame(pc = paste("pc", 1:length(variance_explained)), variance = variance_explained) %>% 
  
  # Call ggplot and supply the axes
  ggplot(aes(x = pc, y = variance, fill = pc)) + 
  
  ## Add the geoms- geom_col and geom_label
  geom_col() + geom_label(show.legend = FALSE, mapping = aes(label = round(variance, 4))) + 
  
  # Add a title
  ggtitle("Figure 4: Skree Plot - Contribution of Each PC to Total Variability") + 
  
  # Remove title
  theme(legend.position = "none") + 
  
  # Add a pleasant theme
  ggthemes::theme_clean() + 
  
  # Add labels and caption
  labs(y = "Variance", caption = "Source: Author's Construction")

################################################################################
## Plot a scree plot
## The cumulative variance captured by the PCAs
variance_explained_cum <- cumsum(sdev ^ 2) / sum(sdev ^ 2)

## View the cumulative variance explained
variance_explained_cum

################################################################################
## Create a dataframe of principal components and cumulative variance
data.frame(pc = paste("pc", 1:length(variance_explained_cum)), 
           
           variance = variance_explained_cum) %>% 
  
  ## Specify the axes
  ggplot(aes(x = pc, y = variance, group = pc)) + 
  
  # Add a geom
  geom_point() +
  
  # Add a title
  ggtitle("Figure 5: Skree Plot - Cumulative Contribution of Each PC to Total Variability") + 
  
  # remove legend
  theme(legend.position = "none") + 
  
  # Add a nice theme
  ggthemes::theme_clean() + 
  
  # Add axes labels and title
  labs(y = "Cumulative Variance", x = "Principal Components", 
       caption = "PC1 is the contribution of PC1 to overall variance. 
       PC2 is the contribution of PC1 and PC2 to overall variance, and so on \n 
       Source: Author's Construction")

################################################################################
## Extract the transformed dataset
fall_train <- pca_trans %>% juice()

## The number of rows in the training dataset containing pcas
pca_trans %>% juice() %>% nrow()

## Checking class balances by dependent variable
table(pca_trans %>% 
        
        ## Extract transformed data
        juice() %>% 
        
        ## Select dependent variable to check for balance in classes
        select(activity)) %>% 
  
  ## Make a nice table and add title      
  knitr::kable(caption = "Distribution of Classes")

## Summary of the PCAs
pca_trans %>% 
  
  ## Extract transformed data
  juice() %>% 
  
  # Select all variables except activity
  select(-activity) %>% 
  
  ## Summarize the data
  skim_without_charts() %>% 
  
  ## Select some variables in the resulting table
  select(-skim_type, -n_missing, -complete_rate) %>% 
  
  ## Make a nice table
  knitr::kable(caption = "Summary Statistics for PCAs in the training set")

################################################################################
## Checking for extreme values 
pca_trans %>% juice() %>% 
  
  ## Convert the data to tidy format
  pivot_longer(-activity, names_to = "pc", values_to = "value") %>% 
  
  ## Filter for values that meet threshold of 7.5 to filter out extreme values
  filter(value > 7.5) 

## Plot histogram for PCAs
pca_trans %>% juice() %>% 
  
  ## Convert data to tidy format
  pivot_longer(-activity, names_to = "pc", values_to = "value") %>% 
  
  ## Filter for values within limits to avoid extreme values
  filter(value <= 7.5 & value >= -7.5) %>% 
  
  ## Plot the data by supplying axes
  ggplot(mapping = aes(x = value, fill = pc), color = "black") + 
  
  ## Add the geom and specify binwidth
  geom_histogram(binwidth = 0.1) + 
  
  ## Add titles and labels
  labs(title = "Figure 6: Histogram of PCs", caption = "Author's computations")

################################################################################
pca_trans %>% 
  
  ## Extract transformed variables - the principal components
  juice() %>% 
  
  ## Plot the first two principal components
  ggplot(mapping = aes(x = PC1, y = PC2, color = activity)) + 
  
  ## Add a geom and a pleasant theme
  geom_point() + ggthemes::theme_clean() + 
  
  ## Add labels and titles
  labs(title = "Figure 7: Visualization of PC1 and PC2")

################################################################################
## Transform the testing data similar to the training data
fall_test <- pca_trans %>% 
  
  ## Use of bake function to generate testing test transformed exactly like the training set
  bake(fall_test)

################################################################################
## **Running the ML models**
## Set seed to be used in the models
seeds <- set.seed(123, sample.kind = "Rounding")

## Set up cross validation parameters
control <- trainControl(method = "repeatedcv",
                        
                        repeats = 10,
                        
                        seeds = seeds)


### Classification tree
# The classification tree model
tree <- caret::train(activity ~ ., 
                     
                     data = fall_train, 
                     
                     # specify engine to use
                     method = "rpart",
                     
                     # Set up cross validation
                     trControl = control,
                     
                     # Set up metric to use
                     metric = "Accuracy",
                     
                     # Tuning parameters
                     tuneGrid = expand.grid(cp = seq(0, 0.05, 0.01)))

# make predictions on the test set
tree_prediction <- predict(tree, newdata = fall_test)

# Generate confusion matrix on the test set
confusionMatrix(tree_prediction, fall_test$activity)

plot(tree)

#rpart.plot(tree$finalModel)

### The Random Forest Model
## The random forest model
## Set up the tuning parameters
tunegrid <- expand.grid(.mtry= sqrt(ncol(fall_train)))

## Set up the random forest model
rf_default <- caret::train(activity ~ ., data = fall_train, 
                           
                           # Set up engine, cross validation and tuning parameters       
                           method = "rf", tunegrid = tunegrid, trControl = control)

## make predictions on the test set
rf_prediction <- predict(rf_default, newdata = fall_test)

## generate confusion matrix
confusionMatrix(rf_prediction, fall_test$activity)

### K-Nearest Neighbours (KNN)
## K-Nearest Neighbours (KNN)
## Set up the model and cross validation
knn <- train(activity ~ ., data = fall_train, method = "knn", 
             
             trControl = control)


# Generate the confusion matrix
confusionMatrix(fall_test$activity, predict(knn, newdata = fall_test))

### Extreme Gradient Boosting (XGBoost)
# The XGboost model
## Set up the tuning parameters
tune_grid <- expand.grid(nrounds = 200,
                         max_depth = 5,
                         eta = 0.05,
                         gamma = 0.01,
                         colsample_bytree = 0.75,
                         min_child_weight = 0,
                         subsample = 0.5)

## Set up model, cross validation and tuning parameters
xgb_model <- train(activity ~., data = fall_train, method = "xgbTree",
                   trControl = control,
                   tuneGrid = tune_grid,
                   tuneLength = 10)

## generate confusion matrix for the test set predictions
confusionMatrix(fall_test$activity, predict(xgb_model, newdata = fall_test))

### Multinomial Logit Model
## Set up the multinomial logit model
multinom <- multinom(activity ~ ., data = fall_train)

## Predict the multinomial logit model on the test set
multinom_predict <- predict(multinom, newdata = fall_test)

## Get confusion matrix 
confusionMatrix(multinom_predict, fall_test$activity)

### Ensemble
#### Ensemble 1: With multinomial logit
## make a dataframe with predictions on the test on all the models
ensemble <- tibble(tree = predict(tree, newdata = fall_test), 
                   rf = predict(rf_default, newdata = fall_test), 
                   knn = predict(knn, newdata = fall_test), 
                   xgb = predict(xgb_model, newdata = fall_test), multinom_predict)


## Create a new colum with outcome being the most popular outcome for the models
ensemble$ensemble_all <- apply(ensemble, 1, function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
})

## Convert the ensembled column into a factor
ensemble$ensemble_all <- factor(ensemble$ensemble_all, levels = levels(ensemble$rf))

## Compute the confusion matrix on the model
confusionMatrix(ensemble$ensemble_all, fall_test$activity)

#### Ensemble 2: Without Mutinomial logit
## make a dataframe with predictions on the test on all the models
ensemble2 <- tibble(tree = predict(tree, newdata = fall_test), 
                    rf = predict(rf_default, newdata = fall_test), 
                    knn = predict(knn, newdata = fall_test), 
                    xgb = predict(xgb_model, newdata = fall_test))


## Create a new colum with outcome being the most popular outcome for the models
ensemble2$ensemble_all <- apply(ensemble2, 1, function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
})

## Convert the ensembled column into a factor
ensemble2$ensemble_all <- factor(ensemble2$ensemble_all, levels = levels(ensemble2$rf))

## Compute the confusion matrix on the model
confusionMatrix(ensemble$ensemble_all, fall_test$activity)

## **Model Evaluation**
## make a table of results
tribble(~ Model, ~ Specificity, ~ Sensitivity, ~ BalancedAccuracy, ~ Accuracy, ~ NoInformationRate, 
        "Classification Tree", "0.8645", "0.4487", "0.6566", "0.5138", "0.2814",
        "Random Forest Model", "0.8859", "0.5171", "0.7015", "0.6179", "0.2814", 
        "K Nearest Neighbours", "0.8587", "0.5105", "0.6846", "0.5189", "0.2134", 
        "Extreme Gradient Boosting", "0.8563", "0.5437", "0.7000", "0.5450", "0.2484", 
        "Multinomial Logit", "0.8630", "0.2970", "0.5800", "0.2828", "0.2814", 
        "Ensemble with Multinomial", "0.8815", "0.5282", "0.7049", "0.5930", "0.2814", 
        "Ensemble without Muntinomial", "0.8804", "0.5291", "0.7048", "0.5928", "0.2814") %>% 
  knitr::kable(caption = "Results of the Machine Learning Models")








