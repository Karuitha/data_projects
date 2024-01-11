library(haven)
library(tidyverse)

gripstr <- read_dta("gripstr.dta")
names(gripstr)

health <- read_dta("health.dta")
names(health)

hgen <- read_dta("hgen.dta")
names(hgen)[1:70]

hl <- read_dta("hl.dta")
names(hl)
grep('hlf0528', names(hl))

jugendl <- read_dta("jugendl.dta")
names(jugendl)[1000:1100]

pequiv <- read_dta("pequiv.dta")
names(pequiv)

pl <- read_dta("pl.dta")
names(pl)[1:10]


pgen <- read_dta("pgen.dta")
names(pgen)

#######################################################
variables = c(names(gripstr), names(health), names(hgen), names(hl),
  names(jugendl), names(pequiv), names(pl), names(pgen))

grep('jl0383', variables)
variables[1601]

## pid
## jugendal- felt happy ## jl0383 Frequency of being happy in the last 4 weeks
## Select 1- very rare to 5 very often
## jugendal- Gross income plb0471_h (Gross salary as employee Amount previous year [harmonized]), 
## ---jugendal- jl0535 Gross income last month
## ---jugendal- TV consumption jl0088
## pl- pli0083: Hours per week on tv
## health - Mental health mcs - MCS: Summary Scale Mental (NBS)
## health- Physical health pcs- PCS: Summary Scale Physical (NBS)
## health - Social functioning sf_nbs
## health - General health (gh_nbs)
## jugendal- sex_v1 (Gender 2017-20) 1. male, 2. female
## jugendal- sex_v2 (Gender 2019, 2021)



jugendl %>% 
  select(pid, syear, jl0383, sex_v1
         ) %>% 
  ## Extract codes from happiness and sex
  mutate(jl0383 = str_extract(jl0383, "^-?\\d{1}$")) %>% 
  mutate(sex_v1 = str_extract(sex_v1, "^-?\\d{1}$")) %>% 
  rename(happy = jl0383) %>% 
  mutate(pid = as.numeric(pid),
         syear = as.numeric(syear),
         happy = as.numeric(happy),
         sex_v1 = as.numeric(sex_v1)) %>% 
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
  ungroup()

## Health data ----
health %>% 
  select(pid, syear, mcs, pcs, sf_nbs, gh_nbs)

## TV usage per week ----
pl %>% 
  select(pid, syear, pli0083, plb0471_h) %>% 
  rename(tv = pli0083, income = plb0471_h) %>% 
  ## Extract the code 
  mutate(tv = str_extract(tv, "^-?\\d{1}$")) %>% 
  ## Extract the salary
  mutate(income = str_extract(income, "^-?\\d{1,5}$")) %>% 
  ## Convert tv to numeric
  mutate(tv = parse_number(tv)) %>% 
  mutate(pid = as.numeric(pid),
         syear = as.numeric(syear),
         income = as.numeric(income)) %>% 
  mutate(tv = case_when(
    tv < 1 ~ NA,
    .default = tv
  )) %>% 
  
  mutate(income = case_when(
    income < 1 ~ NA,
    .default = income
  )) %>% 
## Convert to factor
mutate(tv = factor(tv))

         