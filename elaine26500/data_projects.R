## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)

## Load packages manager----
if (!require(pacman)) {
  install.packages("pacman")
}


## Load the packages ---
p_load(
  tidyverse, janitor, naniar, rio, gt, skimr,
  doParallel, patchwork, ggside, performance,
  ggridges, readxl
)


## Load themes for plots ----
p_load_gh("datarootsio/artyfarty")

## Set the options  ---
options(digits = 3)
options(scipen = 999)
theme_set(theme_bain())

## Hasten code execution by parallel computing ----
all_cores <- parallel::detectCores(logical = FALSE)
cl <- makeCluster(all_cores)
registerDoParallel(cl)




## ---------------------------------------------------------------------
## Project 1 -----
## Read the data
hours <- read_csv("hours.txt")
head(hours) %>%
  gt()


## ---------------------------------------------------------------------
## Scatter plot- hours of study vs scores ----
hours %>%
  ggplot(aes(x = Hours, y = Scores)) +
  geom_point(
    shape = 1,
    size = 4,
    stroke = 2
  ) +
  labs(title = "Hours of Study vs Math Scores") +
  geom_xsidedensity(alpha = .3) +
  geom_ysidedensity(alpha = .3)


## ---------------------------------------------------------------------
## Correlation test for hours of study vs scores ----
with(hours, cor.test(Hours, Scores))


## ---------------------------------------------------------------------
## Project 2 --------
## Read in the data project 2
exams <- read_csv("data/exams.csv") %>%
  clean_names()


## ----fig.height=8, fig.width=12, fig.cap="Pairs Plot"-----------------
# Pairs plot for the data ----
GGally::ggpairs(exams)


## ----fig.cap = "Maths Score vs Parents Level of Education"------------
## Math scores and parent level of education ----
exams %>%
  ggplot(aes(
    x = parental_level_of_education,
    color = parental_level_of_education,
    y = math_score
  )) +
  geom_violin() +
  geom_jitter() +
  labs(
    x = "Parental Level of Education",
    y = "Maths Score",
    title = "Maths Score vs Parents Level of Education"
  ) +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  ) +
  coord_flip()



## ---------------------------------------------------------------------
## Summary of test preparation by race ----
exams %>%
  count(race_ethnicity, test_preparation_course) %>%
  arrange(n) %>%
  gt()


## ---------------------------------------------------------------------
## Chi-square test for test prep by race ----
chisq.test(table(exams$test_preparation_course, exams$race_ethnicity))


## ---------------------------------------------------------------------
## Test for normality ----
shapiro.test(exams$math_score)


## ---------------------------------------------------------------------
## The outlier test ----
boxplot.stats(exams$math_score)$out


## ---------------------------------------------------------------------
## T-test for maths scores greater than 4.17 ----
exams %>%
  mutate(math_score = log(math_score)) %>%
  pull(math_score) %>%
  t.test(
    mu = 4.17,
    alternative = "greater"
  )

mean(log(exams$math_score))


## ---------------------------------------------------------------------
# Maths score and test preparation ----
(exams %>%
  ggplot(aes(
    x = test_preparation_course,
    y = math_score,
    color = test_preparation_course
  )) +
  geom_boxplot() +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) +
  labs(
    x = "Test Preparation", y = "Math Score",
    title = "Test Preparation vs Math Score"
  ) +

  ## Distribution of maths score.
  exams %>%
  ggplot(aes(
    x = math_score,
    fill = test_preparation_course
  )) +
  geom_density(alpha = 0.5) +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) +
  labs(
    x = "Math Score",
    y = "",
    title = "Distribution of Math Scores for\n People that Completed and\n Did not Complete Test Prep Course"
  )

)


## ---------------------------------------------------------------------
## Variance for people completing the test prep
exams %>%
  dplyr::filter(test_preparation_course == "completed") %>%
  summarise(var_with_prep = var(math_score))
## Variance for people not completing the test prep
exams %>%
  dplyr::filter(test_preparation_course == "none") %>%
  summarise(var_no_prep <- var(math_score))




## ---------------------------------------------------------------------
## T-test of maths scores and test preparation ----
t.test(math_score ~ test_preparation_course, data = exams)


## ----fig.cap = "Box Plot of Math Scores by Race"----------------------
# Box plot of maths scores by race ----
exams %>%
  ggplot(aes(
    x = race_ethnicity,
    y = math_score,
    color = race_ethnicity
  )) +
  geom_boxplot() +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) +
  labs(
    x = "Race/Ethnicity", y = "Math Score",
    title = "Race/Ethnicity vs Math Score"
  )




## ----fig.cap = "Distribution of Maths Scores by Race"-----------------
# Distribution of maths scores across race ----
###############################################
exams %>%
  ggplot(aes(x = math_score)) +
  geom_histogram() +
  facet_wrap(~race_ethnicity) +
  labs(title = "Distribution of Maths Scores by Race")


## ---------------------------------------------------------------------
## Analysis of variance ----
aov(math_score ~ race_ethnicity, data = exams) %>%
  summary()


## ----results='asis'---------------------------------------------------
## Regression model and output ----
model <- lm(math_score ~ test_preparation_course + parental_level_of_education + gender + reading_score + lunch, data = exams)

## Regresion model output ----
stargazer::stargazer(model, out = "latex", header = FALSE)


## ----fig.width=12, fig.height=8, fig.cap="Model Diagnostics"----------
# Check for model performance ----
performance::check_model(model)


## ---------------------------------------------------------------------
## Histograms to check for maths and reading scores normality ----
(exams %>%
  ggplot(mapping = aes(x = reading_score)) +
  geom_histogram() |

  exams %>%
    ggplot(mapping = aes(x = math_score)) +
    geom_histogram())


## ---------------------------------------------------------------------
# Homogeneity of variance for maths score (var test) ----
var.test(math_score ~ test_preparation_course,
  data = exams
)


## ---------------------------------------------------------------------
## Homogeneity of variance for reading score ----
var.test(reading_score ~ test_preparation_course,
  data = exams
)



## ---------------------------------------------------------------------
# Linearity of reading and maths scores
exams %>%
  ggplot(mapping = aes(
    x = math_score,
    y = reading_score
  )) +
  geom_point(shape = 1) +
  labs(
    x = "Math Score", y = "Reading score",
    title = "Reading Score vs Maths Score",
    subtitle = "We see an approximate Linear Pattern"
  )


## ---------------------------------------------------------------------
## project 3 ----
## Load the data ----
recall <- readxl::read_xlsx("data/Dataset_3_Pilot_study_data.xlsx",
  na = "Absent"
) %>%
  clean_names() %>%
  mutate(recall_day_5 = as.numeric(recall_day_5))

## View the data ---
recall %>%
  head() %>%
  gt()


## ----fig.cap="Missing Data", fig.height=8, fig.width=12---------------
## Missing data ----
recall %>%
  vis_miss()


## ---------------------------------------------------------------------
## Summary of Numeric Variables by Group ----
recall %>%
  group_by(group) %>%
  skim_without_charts() %>%
  dplyr::filter(skim_type == "numeric") %>%
  dplyr::select(-starts_with("character")) %>%
  gt(caption = "Summary of Numeric Variables by Group")


## ---------------------------------------------------------------------
## Distribution of recall rates, day5 ----
recall %>%
  ggplot(aes(x = recall_day_5, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Recall Rates in Day 5",
    x = "Recall Rate, Day 5"
  )


## ---------------------------------------------------------------------
## Distribution of recall rates, day35 ----
recall %>%
  ggplot(aes(x = recall_day_35, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Recall Rates in Day 35",
    x = "Recall Rate, Day 35"
  )




## ---------------------------------------------------------------------
## Man Whitney test for day 5 ----
retrieval <- recall %>% pull(recall_day_5)
non_ret <- recall %>% pull(recall_day_5)
wilcox.test(retrieval, non_ret)


## ---------------------------------------------------------------------
## Man Whitney test for day 35 ----
retrieval35 <- recall %>% pull(recall_day_35)
non_ret35 <- recall %>% pull(recall_day_35)
wilcox.test(retrieval35, non_ret35)
