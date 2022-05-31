## @knitr loadpackages
library(skimr)
library(tidyverse)
library(knitr)
library(GGally)
library(ggthemes)
library(tibble)
library(magrittr)
library(gghalves)
library(patchwork)

options(digits = 4)
options(scipen = 999)
theme_set(theme_clean())


## @knitr loandatastructure
str(loans)

## @knitr ggallyvisual
loans %>% GGally::ggpairs(columns = c("int_rate", "installment", 
                                      
                          "log_annual_inc",  
                          
                          "dti", "fico", "days_with_cr_line", 
                          
                          "revol_bal", "revol_util", 
                          
                          "inq_last_6mths", "delinq_2yrs", 
                          
                          "pub_rec", "not_fully_paid")) 

## @knitr descriptivestats
loans %>% 
  skim_without_charts() %>% 
  select(-(numeric.p0:numeric.p100)) %>%
  select(-(complete_rate))

## @knitr whoisrisky
(loans %>% 
  
  ggplot(mapping = aes(x = fico, y = revol_util, color = not_fully_paid)) + 
  
  geom_density2d() +
  
  geom_smooth(se = FALSE) +
  
  scale_color_manual(values = c("red", "blue")) + 
  
  labs(x = "FICO Score", y = "Loan Utilisation", 
       
       title = "FICO Score against Loan facilty Utilisation") + 
    
    theme(legend.position = "none") +
    
    facet_wrap(~ not_fully_paid) +

loans %>% 
  
  ggplot(mapping = aes(x = fico, y = int_rate, color = not_fully_paid)) + 
  
  geom_density2d() +
  
  geom_smooth(se = FALSE) +
  
  scale_color_manual(values = c("red", "blue")) + 
  
  labs(x = "FICO Score", y = "Interest Rate", 
       
       title = "FICO Score against Interest Rates on Loans") +
  
  facet_wrap(~ not_fully_paid) + 
    
    theme(legend.position = "none")) /


(loans %>% 
  
  group_by(factor(delinq_2yrs)) %>% 
  
  filter(n() > 2) %>% 
  
  ungroup() %>% 
  
  ggplot(mapping = aes(x = fct_reorder(factor(delinq_2yrs), int_rate, median), 
                       
                       y = int_rate, fill = factor(delinq_2yrs))) + 
  
  geom_half_violin(varwidth = TRUE, show.legend = FALSE) + 
  
  stat_summary(fun = median, show.legend = FALSE) +
   
  labs(x = "Loan Deliquency", y = "Interest Rate", 
       
       title = "Interest Rates versus Loans Delinquency") +



loans %>% 
  
  ggplot(mapping = aes(x = fico, y = revol_util, color = int_rate)) + 
  
  geom_point(alpha = 0.5) + 
  
  labs(x = "FICO Score", y = "Loan Utilisation", 
       
       title = "Loan Utilisation Vesus FICO Scores") +
  
  scale_color_gradient(low = "blue", high = "red"))

## @knitr bank_risk_perception
loans %>% ggplot(mapping = aes(x = fct_reorder(purpose, int_rate, median), 
                               
                               y = int_rate, fill = not_fully_paid, 
                               
                               color = not_fully_paid)) + 
  
  geom_boxplot() + 
  
  coord_flip() + 
  
  scale_fill_fivethirtyeight() +
  
  scale_color_wsj() + 
  
  labs(x = "Loan Purpose", y = "Interest Rate", 
       
       title = "Riskiness of Loans by Loan Type", 
       
       subtitle = "Higher Interest Rates Signal Higher Perceived Loan Risk") +
  
  theme(legend.position = 'top', 
        
        plot.title = element_text(size = 30, face = "bold", colour = "Black"),
        
        plot.subtitle = element_text(size = 15, face = "italic", colour = "red"),
        
        axis.text = element_text(size = 15))


## @knitr loans_by_fico
loan_updated <- loans %>% mutate(fico_level = case_when(
  
  fico >= 800 ~ "Exceptional",
  
  fico >= 750 & fico < 800 ~ "High",
  
  fico >= 650 & fico < 750 ~ "Medium",
  
  TRUE ~ "Low"
  
)) %>% 
  
  mutate(fico_level = factor(fico_level))

loan_updated %>% 
  
  select(fico_level, purpose) %>% 
  
  table() %>% 
  
  prop.table(1)*100
