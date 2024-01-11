## Load packages manager ----
if(!require(pacman)){
  install.packages('pacman')
}

## Load packages ----
p_load(tidyverse, janitor,
       WDI, wbstats, 
       countrycode,
       naniar, mice,
       Amelia, plm,
       skimr, kableExtra,
       stargazer)

## Digits
options(digits = 4)

## Get the data ----
my_data <- read_csv('ff5b1b8a-0065-492a-8775-0a3fe60ed0a1_Data.csv',
         na = "..") %>%
  clean_names() %>%
  select(-time_code) %>%
  mutate(continent = countrycode(
    sourcevar = country_code,
    origin = "iso3c",
    destination = "continent"
  )) %>%
  filter(!is.na(continent)) %>%
  left_join(

    read_csv("9c0304bb-658e-491e-891d-a3c35a472b2c_Data.csv",
                na = "..") %>%
  clean_names(), 
  
  by = c("country_name", "country_code", "time")
  ) %>%
  mice::mice(
    n = 5,
    maxit = 500,
    seed = 123
  ) %>%
  complete()

my_data %>%
  select(-time_code) %>%
  write_csv("my_data.csv") 

## The data ----
gov <- my_data %>%
  select(ends_with('est')) %>%
  prcomp(
    scale. = TRUE,
    center = TRUE
  ) %>%
  pluck("x") %>%
  data.frame() %>%
  select(PC1) %>%
  set_names("governance")


## Data ----
my_data <- read_csv("my_data.csv") %>%
  bind_cols(gov) %>%
  mutate(
    educ = school_enrollment_secondary_percent_net_se_sec_nenr/
      school_enrollment_primary_percent_net_se_prm_nenr,
    openess = imports_of_goods_and_services_percent_of_gdp_ne_imp_gnfs_zs + 
      exports_of_goods_and_services_percent_of_gdp_ne_exp_gnfs_zs
  ) %>%
  select(
    time,
    country_code,
    country_name,
    continent,
    account_ownership_at_a_financial_institution_or_with_a_mobile_money_service_provider_percent_of_population_ages_15_fx_own_totl_zs,
    governance,
    educ,
    openess
  ) %>%
  rename(
    accounts = account_ownership_at_a_financial_institution_or_with_a_mobile_money_service_provider_percent_of_population_ages_15_fx_own_totl_zs
  ) %>%
  mutate(time = as.numeric(time))

## Create the graphs ----
my_data %>%
  summarise(
    accounts = mean(accounts),
    .by = c(time, continent)
  ) %>%
  ggplot(mapping = aes(x = time, 
                       y = accounts, 
                       color = continent)) + 
  geom_line(show.legend = FALSE) + 
  labs(x = "", y = "Banks Accounts",
       title = "% Accounts Held by People 15+ Years Old")


## Summary statistics 
my_data %>%
  select(where(is.numeric)) %>%
  skimr::skim_without_charts() %>%
  select(-n_missing, -complete_rate) %>%
  rename(
    Variable = skim_variable,
    Mean = numeric.mean,
    SD = numeric.sd,
    Min = numeric.p0,
    Q1 = numeric.p25,
    Median = numeric.p50,
    Q3 = numeric.p75,
    Max = numeric.p100
  ) %>%
  kbl(
    caption = "Summary Statistics",
    booktabs = TRUE
  ) %>%
  kable_classic(
    full_width = TRUE,
    latex_options = "hold_position"
  )


## PLM
fixed_effects <- plm(accounts ~ educ + openess + governance + factor(continent),
    index = c("continent", "time"),
    data = my_data,
    model = "within")

random_effects <- plm(accounts ~ educ + openess + governance + continent,
    index = c("continent", "time"),
    data = my_data,
    model = "random")

pooling_effects <- plm(accounts ~ educ + openess + governance + continent,
                      index = c("continent", "time"),
                      data = my_data,
                      model = "pooling")



summary(fixed_effects)
summary(pooling_effects)
summary(random_effects)

