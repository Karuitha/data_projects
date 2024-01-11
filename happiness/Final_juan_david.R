## ----setup, include=FALSE----
# knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

options(scipen = 999)
options(digits = 3)


## ----packages---------------
if (!require(pacman)) {
  install.packages("pacman")
}

p_load(
  tidyverse, performance,
  janitor, GGally, psych,
  skimr, gt, kableExtra, styler,
  haven, doParallel, naniar,
  ggthemes, Amelia, plotly,
  nnet, patchwork
)

## Create a parallel computing cluster ----
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

## Set a theme for plots ----
theme_set(ggthemes::theme_clean() +
  theme(axis.text = element_text()))


## ----variables--------------
tribble(
  ~Variable, ~Description,
  "happy", "Frequency of being happy in the last 4 weeks from 1: very rare to 5: very often",
  "tv", "Hours per week on tv from 1: Daily to 5: Never",
  "mcs", "Summary Scale Mental: Mental Health Indicator", "pcs", "Summary Scale Physical: Physical Health Indicator", "sf_nbs", "Social functioning: Indicator of the state of social life", "sex_v1", "Gender of Individual from 1: Male to 2: Female", "income", "Gross salary as employee Amount previous year"
) %>%
  kbl(caption = "Variables Description") %>%
  kable_classic(full_width = FALSE) %>%
  footnote(general = "Source: https://paneldata.org/soep-core/")


## ----read_data--------------
jugendl <- read_dta("jugendl.dta")
health <- read_dta("health.dta")
pl <- read_dta("pl.dta")


## ----clean_jug_data---------
dt1 <- jugendl %>%
  select(pid, jl0383, sex_v1, jl0088) %>%
  ## Extract codes from happiness and sex
  mutate(jl0383 = str_extract(jl0383, "^-?\\d{1}$")) %>%
  mutate(sex_v1 = str_extract(sex_v1, "^-?\\d{1}$")) %>%
  ## Rename column jl0383 to happy
  rename(happy = jl0383, tv1 = jl0088) %>%
  ## Convert to numeric
  mutate(
    pid = as.numeric(pid),
    happy = as.numeric(happy),
    sex_v1 = as.numeric(sex_v1)
  ) %>%
  ## Code missing values
  mutate(sex_v1 = case_when(
    sex_v1 %in% c(1, 2) ~ sex_v1,
    .default = NA
  )) %>%
  mutate(happy = case_when(
    happy >= 1 ~ happy,
    .default = NA
  )) %>%
  ## Fill up sex as it barely changes
  group_by(pid) %>%
  fill(sex_v1, .direction = "updown") %>%
  fill(sex_v1, .direction = "downup") %>%
  ungroup() %>%
  ## Filter for non-missing data
  filter(happy >= 1) %>%
  ## Convert variables to factors
  mutate(sex_v1 = factor(sex_v1,
    levels = c(1, 2),
    labels = c("Male", "Female")
  )) %>%
  mutate(happy = factor(happy,
    levels = 1:5,
    labels = c(
      "Very Rare", "Rare",
      "Sometime", "Often",
      "Very Often"
    )
  )) %>%
  group_by(pid) %>%
  fill(happy, .direction = "updown")

head(dt1) %>%
  kbl(caption = "Jugendl Dataset Extract") %>%
  kable_classic(full_width = FALSE) %>%
  footnote(general = "Source: https://paneldata.org/soep-core/")


## ----clean_health_data------
dt2 <- health %>%
  select(pid, mcs, pcs, sf_nbs, gh_nbs)

head(dt2) %>%
  kbl(caption = "Health Dataset Extract") %>%
  kable_classic(full_width = FALSE) %>%
  footnote(general = "Source: https://paneldata.org/soep-core/")


## ----clean_pl_data----------
## TV usage per week ----
dt3 <- pl %>%
  select(pid, pli0083, plb0471_h) %>%
  rename(tv = pli0083, income = plb0471_h) %>%
  ## Extract the code
  mutate(tv = str_extract(tv, "^-?\\d{1}$")) %>%
  ## Extract the salary
  mutate(income = str_extract(income, "^-?\\d{1,5}$")) %>%
  ## Convert tv to numeric
  mutate(tv = parse_number(tv)) %>%
  mutate(
    pid = as.numeric(pid),
    income = as.numeric(income)
  ) %>%
  mutate(tv = case_when(
    tv < 1 ~ NA,
    .default = tv
  )) %>%
  mutate(income = case_when(
    income < 1 ~ NA,
    .default = income
  )) %>%
  filter(tv >= 1, !is.na(tv)) %>%
  ## Convert to factor
  mutate(tv = factor(tv,
    levels = 1:5,
    labels = c(
      "Daily",
      "Weekly",
      "Monthly",
      "Rarely",
      "Never"
    )
  ))

head(dt1) %>%
  kbl(caption = "Pl Dataset Extract") %>%
  kable_classic(full_width = FALSE) %>%
  footnote(general = "Source: https://paneldata.org/soep-core/")


## ----join_data--------------
final_data <- dt2 %>%
  left_join(dt3, by = join_by(pid)) %>%
  left_join(dt1, by = join_by(pid)) %>%
  drop_na(tv, happy)

# head(final_data)


## ----missing_values---------
final_data %>%
  Amelia::missmap()


## Summary Statistics---------
final_data %>%
  select(
    where(is.numeric), -pid,
    -starts_with("syear")
  ) %>%
  skimr::skim_without_charts() %>%
  select(-n_missing, -skim_type) %>%
  rename(
    Mean = numeric.mean,
    SD = numeric.sd,
    Min = numeric.p0,
    Q1 = numeric.p25,
    Median = numeric.p50,
    Q3 = numeric.p75,
    Max = numeric.p100,
    Variable = skim_variable
  ) %>%
  kbl(caption = "Summary Statistics") %>%
  kable_classic(full_width = FALSE) %>%
  footnote(general = "Source: https://paneldata.org/soep-core/")


## Summary Statistics ------------
final_data %>%
  select(where(is.factor)) %>%
  skimr::skim_without_charts() %>%
  select(-n_missing, -skim_type) %>%
  kbl(caption = "Summary Statistics") %>%
  kable_classic(full_width = FALSE) %>%
  footnote(general = "Source: https://paneldata.org/soep-core/")


## Pairs Plot ---------------------------
final_data %>%
  select(
    -pid, -tv1,
    -starts_with("syear")
  ) %>%
  GGally::ggpairs(mapping = aes(fill = happy))


## Bar plots on Happiness vs TV ----
(final_data %>%
  ggplot(mapping = aes(x = tv)) +
  geom_bar(show.legend = FALSE) +
  labs(
    x = "", y = "Frequency",
    title = "Watching TV"
  ) +
  final_data %>%
  ggplot(mapping = aes(x = tv, fill = happy)) +
  geom_bar(position = "fill") +
  labs(
    x = "", y = "",
    title = "Watching TV"
  ) +
  scale_fill_colorblind()) /

  (final_data %>%
    ggplot(mapping = aes(x = happy)) +
    geom_bar(show.legend = FALSE) +
    labs(
      x = "", y = "Frequency",
      title = "Happiness"
    ) +
    final_data %>%
    ggplot(mapping = aes(x = happy, fill = tv)) +
    geom_bar(position = "fill") +
    labs(
      x = "", y = "",
      title = "Happiness"
    ) +
    scale_fill_colorblind())


## Heatmap on Happiness vs TV Consumption ---
final_data %>%
  count(happy, tv) %>%
  ggplot(mapping = aes(
    y = happy, x = tv,
    fill = n
  )) +
  geom_tile() +
  scale_fill_gradient(
    low = "blue",
    high = "red"
  ) +
  labs(
    y = "Happiness",
    x = "Watching TV",
    title = "Happiness and Watching TV",
    subtitle = "The graph shows that happy people watch more TV. This does not imply causality.",
    caption = "Data Source: https://paneldata.org/soep-core/"
  )


## Table: Happiness vs TV Consumption------
final_data %>%
  count(happy, tv) %>%
  kbl(caption = "Happiness and TV Consumption") %>%
  kable_classic(full_width = FALSE) %>%
  footnote(general = "Source: Author's Construction Using Data from https://paneldata.org/soep-core/")


## Style the code -----------
styler::style_file("Final_juan_david.R")
