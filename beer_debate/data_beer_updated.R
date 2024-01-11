##THE BEER DEBATE PROJECT ----
# Objective- to get and clean data on beer ratings ----
# Load required packages ----
## @knitr my_packages
library(tidyverse)
library(rvest)
library(ggthemes)
library(plotly)
library(GGally)
library(tidyquant)
library(skimr)
library(kableExtra)
library(plotly)
library(patchwork)
library(glmnet)
library(performance)
library(tidymodels)

theme_set(theme_tq())

## @knitr data_scrapping_beer_advocate
###############################################################
## Scrapping function -----
###############################################################

my_comprehensive_scrapper <- function(url, n_rows, category){
  
## get the Beers, number of votes and average rating ----
  
  beer_data_main <- read_html(url) %>% 
    
    html_nodes(".hr_bottom_light b") %>% 
    
    html_text() %>% 
    
    tibble() %>% 
    
    mutate(key = rep(1:3, n_rows))
  
  beer <- data.frame(beer = filter(beer_data_main, key == 1))
  
  votes <- data.frame(votes = filter(beer_data_main, key == 2))
  
  avg <- data.frame(avg = filter(beer_data_main, key == 3))
  
  long_beers <- bind_cols(beer, votes, avg) %>% 
    
    select(-ends_with("key")) %>% 
    
    set_names(c("beer", "votes", "rating_average"))
  
## Get the beer category- like stout ----
  
  beer_category <- read_html(url) %>% 
    
    html_nodes("#ba-content a~ a") %>% 
    
    html_text() %>% 
    
    tibble() %>% 
    
    set_names("type")
  
## Get the full table  ----
  
  full_table <- read_html(url) %>% 
    
    html_nodes("table") %>% 
    
    html_table() %>% 
    
    .[[1]] %>% 
    
    filter(!is.na(X1)) %>% 
    
    select(-X1, -X3, -X4, -X5)
  
## Combine the three tables to form one table ----
  
long_beers %>% bind_cols(full_table) %>% 
    
    bind_cols(beer_category) %>% 
    
    mutate(X2 = str_remove_all(X2, beer)) %>% 
    
    mutate(X2 = str_remove_all(X2, type)) %>% 
    
    mutate(alcohol_perc = str_extract(X2, "\\d{1,2}\\.\\d{2}%$"), 
           
           X2 = str_remove_all(X2, "\\|\\s?\\d{1,2}\\.\\d{2}%$"),
           
           alcohol_perc = parse_number(alcohol_perc),
           
           category = category) %>% 
    
    rename(brewer = X2)
  
}

## @knitr the_data
###################################################################################

top_250_beers <- my_comprehensive_scrapper(url = "https://www.beeradvocate.com/beer/top-rated/", 
                                           
                                           n_rows = 250, category = "top_250")


trending_beers <- my_comprehensive_scrapper(url = "https://www.beeradvocate.com/beer/trending/", 
                                            
                                            n_rows = 100, category = "trending")

top_new <- my_comprehensive_scrapper(url = "https://www.beeradvocate.com/beer/top-new/", 
                                            
                                            n_rows = 250, category = "new")

fame_beer <- my_comprehensive_scrapper(url = "https://www.beeradvocate.com/beer/fame/", 
                                     
                                     n_rows = 250, category = "fame")

popular_beer <- my_comprehensive_scrapper(url = "https://www.beeradvocate.com/beer/popular/", 
                                       
                                       n_rows = 250, category = "popular")

#########################################################################################
## @knitr combined_data
## Combine the three datasets ----

full_beer_data <- top_250_beers %>% 
  
  bind_rows(trending_beers) %>% 
  
  bind_rows(top_new) %>% 
  
  bind_rows(fame_beer) %>% 
  
  bind_rows(popular_beer) %>% 
  
## Add alcohol percentage for beers with the missing data

  mutate(
    
    alcohol_perc = case_when(
      
      beer == "Rare Scooop" ~ 6.16,
      
      beer == "Madness & Civilization #14" ~ 3.81,
      
      beer == "Thumbprint Lots O' Peach 21" ~ 7.55,
      
      beer == "Persevere" ~ 6.12,
      
      beer == "Sankt" ~ 7.21,
      
      beer == "Nonconformist 02" ~ 7.94,
      
      beer == "Civil Disobedience #31" ~ 6,
      
      beer == "Anna Pear" ~ 6.5,
      
      beer == "Foster" ~ 4,
      
      beer == "Leaves Of Grass: October 3, 2019" ~ 6.5,
      
      beer == "Medianoche Reserve (2021)" ~ 4,
      
      beer == "Steadfast" ~ 6.8,
      
      beer == "Tree Of Bliss" ~ 8,
      
      beer == "Tree Of Light" ~ 12,
      
      beer == "We Are Made Of The Same Stardust" ~ 9.3,
      
      TRUE ~ alcohol_perc
      
    )) %>% 

###################################################################################
## Add beer category light, medium, strong
  mutate(strength_class = case_when(
    
    alcohol_perc <= 5 ~ "lite",
    
    alcohol_perc > 5 & alcohol_perc <= 10 ~ "medium",
    
    alcohol_perc > 10 & alcohol_perc <= 15 ~ "strong",
    
    alcohol_perc > 15 & alcohol_perc <= 20 ~ "super",
    
    TRUE ~ "ultra"
    
  )) %>% 
  
## Convert rating average and votes to numeric
  
mutate(votes = parse_number(votes),
       
       rating_average = parse_number(rating_average)) %>% 
  
relocate(category, votes, rating_average, 
         
         .after = strength_class) %>% 
  
  select(-category) %>% 
  
  group_by(beer, brewer, type, alcohol_perc, strength_class) %>% 
  
  summarise(votes = sum(votes),
            
            rating_average = mean(rating_average)) %>% 
  
  ungroup() %>% 
  
  distinct() %>% 
  
  mutate(brewer = str_trim(brewer),
         
         beer = str_trim(beer),
         
         type = str_trim(type))

##########################################################################
## @knitr advocate_data_summary
full_beer_data %>% 
  
  select(where(is.numeric)) %>% 
  
  skim_without_charts() %>% 
  
  select(-skim_type) %>% 
  
  rename(Variable = skim_variable,
         
         Missing = n_missing, 
         
         Mean = numeric.mean,
         
         SD = numeric.sd, 
         
         Min = numeric.p0,
         
         Q1 = numeric.p25, 
         
         Median = numeric.p50,
         
         Q3 = numeric.p75, 
         
         Max = numeric.p100) %>% 
  
  kbl(., booktabs = TRUE, 
      
      caption = "Summary of Numeric Variables") %>% 
  
  kable_classic(full_width = TRUE, font_size = 10) %>% 
  
  footnote(number = "Data source: BeerAdvocate.com")

##########################################################################
## @knitr advocate_data_summary_two

full_beer_data %>% 
  
  select(-alcohol_perc, -votes, -rating_average) %>% 
  
  skimr::skim() %>% 
  
  select(-skim_type) %>% 
  
  rename(Variable = skim_variable) %>% 
  
  kbl(., booktabs = TRUE,
      
      caption = "Summary of Non-numeric Variables") %>% 
  
  kable_classic(full_width = TRUE, font_size = 10) %>% 
  
  footnote(number = "Data source: BeerAdvocate.com")

##########################################################################
## @knitr beer_strength_ratings

full_beer_data %>% 
  
  filter(!duplicated(.)) %>% 
  
  ggplot(aes(x = strength_class, y = rating_average)) + 
  
  geom_boxplot(show.legend = FALSE, 
               
               outlier.color = "red", outlier.shape = 1,
               
               outlier.size = 4) + 
  
  geom_jitter(alpha = 0.15, show.legend = FALSE) + 
  
  scale_fill_brewer(palette = 8) + 
  
  ggthemes::theme_fivethirtyeight() +
  
  theme(axis.text.x = element_text(size = 12))

################################################################
## @knitr strong_beer_types

full_beer_data %>% 
  
  filter(!duplicated(.)) %>% 
  
  group_by(type) %>% 
  
  filter(n() > 4) %>% 
  
  ungroup() %>% 
  
  ggplot(aes(x = fct_reorder(type, rating_average, max), 
             
             
             y = rating_average)) + 
  
  geom_boxplot(show.legend = FALSE, 
               
               outlier.color = "red", outlier.shape = 1,
               
               outlier.size = 4) + 
  
  geom_jitter(alpha = 0.15, show.legend = FALSE) + 
  
  scale_fill_brewer(palette = 8) + 
  
  ggthemes::theme_fivethirtyeight() +
  
  theme(axis.text.x = element_text(size = 12)) +
  
  coord_flip()


################################################################################
##Visualize the data ----
## @knitr strong_beer_types2

ggplotly(full_beer_data %>% 
  
  group_by(type) %>% 
  
  filter(n() >= 5) %>% 
  
  ungroup() %>% 
           
  ggplot(aes(x = reorder(type, alcohol_perc, median), 
             
  y = alcohol_perc, fill = type)) + 
    
    geom_boxplot() + 
    
  theme_hc() + theme(legend.position = "none") + 
    
  theme(axis.text.x = element_text(angle = 90)) + 
    
  labs(y = "Alcohol Percentage", x = "Beer Type", 
       title = "BEER DEBATE",
       subtitle = "A Visual Guide to Choosing Your Poison", 
       caption = "John Karuitha (2022),
       Data Source: Beer Advocate- Your Go-To Resource for Beer
       Website: https://www.beeradvocate.com, 
       **Respect Beer") + 
  
  theme(title = element_text(size = 20),
        
        axis.text.x = element_text(size = 12, angle = 60)
        
        ))

##########################################################################################
## @knitr Alcohol_content_ratings ----
full_beer_data %>% group_by(type) %>% 
           
           ggplot(aes(x = alcohol_perc, 
                      
                              y = rating_average, color = type)) + 
           
                              geom_point(alpha = 0.5, shape = 1, size = 5, 
                                         
                                         stroke = 3, show.legend = FALSE)

################################################################################
################################################################################
## @knitr mean_ratings_plot

full_beer_data %>% 
  
  group_by(brewer) %>% 
  
  summarize(mean_rating = mean(rating_average)) %>% 
  
  arrange(mean_rating) %>% 
  
  ggplot(mapping = aes(x = fct_reorder(brewer, mean_rating, max), y = mean_rating)) + 
  
  geom_point() + 
  
  ggthemes::theme_fivethirtyeight()

#####################################################
## Scrapping beer consumption data from wikipedia 
## @knitr wiki_beer_data
wiki_adress <- "https://en.wikipedia.org/wiki/List_of_countries_by_beer_consumption_per_capita"

wiki_data <- read_html(wiki_adress) %>% 
  
  html_nodes("table") %>% 
  
  html_table() %>% 
  
  .[[1]] %>% 
  
  janitor::clean_names() %>% 
  
  mutate(country = str_remove_all(country, "\\*")) %>% 
  
  mutate(total_nationalconsumption_a_million_litresper_year = 
           
           parse_number(total_nationalconsumption_a_million_litresper_year)) %>% 
  
  select(-sources)

## @knitr explore_wiki_data

head(wiki_data)

## @knitr per_capita_beer
wiki_data %>% 
  
  arrange(desc(consumptionper_capita_1_litres_per_year)) %>% 
  
  head(10) %>% 
  
  ggplot(mapping = aes(x = fct_reorder(country, consumptionper_capita_1_litres_per_year, max),
                       
                       y = consumptionper_capita_1_litres_per_year)) + 
  
  geom_col(show.legend = FALSE, color = "black") + 
  
  coord_flip() +
  
  labs(x = "Country", 
       
       y = "Beer Consumption, Litres per Year",
       
       caption = "Data Source: Wikipedia",
       
       title = "Per Capita Beer Consumption in the World")

## @knitr total_beer_consumption
wiki_data %>% 
  
  arrange(desc(total_nationalconsumption_a_million_litresper_year)) %>% 
  
  head(10) %>% 
  
  ggplot(mapping = aes(x = fct_reorder(country, total_nationalconsumption_a_million_litresper_year, max), 
                       
                       y = total_nationalconsumption_a_million_litresper_year)) +
  
  geom_col(color = "black", show.legend = FALSE) +
  
  labs(x = "Country", y = "Alcohol Consumed, Millions of Litres",
       
       title = "Alcohol Consumption in the World") + 
  
  coord_flip()

## @knitr wiki_data_summary

wiki_data %>% 
  
  select(where(is.numeric)) %>% 
  
  skim_without_charts() %>% 
  
  select(-skim_type) %>% 
  
  rename(Variable = skim_variable,
         
         Missing = n_missing, Mean = numeric.mean,
         
         SD = numeric.sd, Min = numeric.p0,
         
         Q1 = numeric.p25, Median = numeric.p50,
         
         Q3 = numeric.p75, Max = numeric.p100) %>% 
  
  kbl(., booktabs = TRUE, 
      
      caption = "Summary of Numeric Variables") %>% 
  
  kable_classic(full_width = TRUE, font_size = 10) %>% 
  
  footnote(number = "Data source: Wikipedia")

################################################################################
## @knitr wiki_data_summary_two

wiki_data %>% 
  
  select(country) %>% 
  
  skimr::skim() %>% 
  
  select(-skim_type) %>% 
  
  rename(Variable = skim_variable) %>% 
  
  kbl(., booktabs = TRUE,
      
      caption = "Summary of Non-numeric Variables") %>% 
  
  kable_classic(full_width = TRUE, font_size = 10) %>% 
  
  footnote(number = "Data source: Wikipedia")

## @knitr popular_beers

full_beer_data %>% 
  
  distinct() %>% 
  
  arrange(desc(rating_average)) %>% 
  
  head(10) %>% 
  
  kbl(., booktabs = TRUE) %>% 
  
  kable_classic(full_width = TRUE, font_size = 10) %>% 
  
  footnote(number = "Data Source: BeerAdvocate.com")  

## @knitr popular_beers_filtered

full_beer_data %>% 
  
  distinct() %>% 
  
  arrange(desc(rating_average)) %>% 
  
  filter(votes >= 100) %>% 
  
  head(10) %>% 
  
  kbl(., booktabs = TRUE) %>% 
  
  kable_classic(full_width = TRUE, font_size = 10) %>% 
  
  footnote(number = "Data Source: BeerAdvocate.com")  

## @knitr beers_with_most_votes

full_beer_data %>% 
  
  distinct() %>% 
  
  filter(votes >= 100) %>% 
  
  arrange(desc(votes)) %>% 
  
  head(10) %>% 
  
  ggplot(mapping = aes(x = fct_reorder(beer, votes, max), 
                       
                       y = votes, fill = brewer)) + 
  
  geom_col(show.legend = TRUE) + 
  
  coord_flip() +
  
  labs(x = "Beer", y = "Votes", 
       
       title = "Beers with the Highest Votes")

## @knitr beer_categories
full_beer_data %>% 
  
  filter(votes >= 100) %>% 
  
  count(type, sort = TRUE, name = "Number_of_Beers") %>% 
  
  filter(Number_of_Beers >= 10) %>% 
  
  kbl(., booktabs = TRUE,
      
      caption = "Most Popular Beer categories") %>% 
  
  kable_classic(full_width = FALSE, 
                
                font_size = 10) %>% 
  
  footnote(number = "Data Source: BeerAdvocate.com")  

## @knitr most_popular brewers

full_beer_data %>% 
  
  filter(votes >= 100) %>% 
  
  count(brewer, sort = TRUE, name = "Number_of_Beers") %>% 
  
  filter(Number_of_Beers >= 10) %>% 
  
  kbl(., booktabs = TRUE,
      
      caption = "Most Popular Brewers") %>% 
  
  kable_classic(full_width = FALSE, 
                
                font_size = 10) %>% 
  
  footnote(number = "Data Source: BeerAdvocate.com") 

## @knitr most_popular_beers_categories_votes

full_beer_data %>% 
  
  filter(votes >= 100) %>% 
  
  group_by(type) %>% 
  
  summarise(Median_Rating = median(rating_average),
            
            Mean_Rating = mean(rating_average)) %>% 
  
  arrange(desc(Median_Rating)) %>% 
  
  head(10) %>% 
  
  kbl(., booktabs = TRUE,
      
      caption = "Most Popular Beer Types (Mean and Median of Average Ratings)") %>% 
  
  kable_classic(full_width = FALSE, 
                
                font_size = 10) %>% 
  
  footnote(number = "Data Source: BeerAdvocate.com") 


## @knitr median_rating_brewers

full_beer_data %>% 
  
  filter(votes >= 100) %>% 
  
  group_by(brewer) %>% 
  
  summarise(Median_Rating = median(rating_average),
            
            Mean_rating = mean(rating_average)) %>% 
  
  arrange(desc(Median_Rating)) %>% 
  
  head(10) %>% 
  
  kbl(., booktabs = TRUE,
      
      caption = "Most Popular Brewers (Mean and Median of Average Ratings)") %>% 
  
  kable_classic(full_width = FALSE, 
                
                font_size = 10) %>% 
  
  footnote(number = "Data Source: BeerAdvocate.com") 

## @knitr brewers_with_most_votes

full_beer_data %>% 
  
  group_by(brewer) %>% 
  
  summarise(total_votes = sum(votes)) %>% 
  
  arrange(desc(total_votes)) %>% 
  
  head(10) %>% 
  
  kbl(., booktabs = TRUE,
      
      caption = "Brewers Whose Beers Received Highest Votes") %>% 
  
  kable_classic(full_width = FALSE, 
                
                font_size = 10) %>% 
  
  footnote(number = "Data Source: BeerAdvocate.com") 
  
## @knitr beers_highest_alcohol_conc
full_beer_data %>% 
  
  arrange(desc(alcohol_perc)) %>% 
  
  head(10) %>% 
  
  kbl(., booktabs = TRUE,
      
      caption = "Beers with Highest Alcohol Content") %>% 
  
  kable_classic(full_width = FALSE, 
                
                font_size = 10) %>% 
  
  footnote(number = "Data Source: BeerAdvocate.com") 
  
## @knitr beer_categories 

beer_type_names <- table(full_beer_data$type) %>% 
  
  as.data.frame() %>% 
  
  pull(Var1) %>% as.character()
       
beer_type_freq <- table(full_beer_data$type) %>% as.data.frame() %>% pull(Freq)

cbind(beer_type_names, beer_type_freq) %>% 
  
  as_tibble() %>% 
  
  mutate(beer_type_freq = as.numeric(beer_type_freq)) %>% 
  
  arrange(desc(beer_type_freq)) %>% 
  
  kbl(., booktabs = TRUE,
      
      caption = "Beers Categories/ Types") %>% 
  
  kable_classic(full_width = FALSE, 
                
                font_size = 10) %>% 
  
  footnote(number = "Data Source: BeerAdvocate.com") 


## @knitr numeric_variables_desc
## Distribution of alcohol content in beers

(
full_beer_data %>% 
  
  ggplot(mapping = aes(x = alcohol_perc)) + 
  
  geom_histogram(col = "gray", 
               
               fill = "purple") + 
  
  labs(x = "Alcohol Percentage",
       
       y = "Count",
       
       title = "Distribution of Alcohol Percentage") +


## Distribution of rating average 
full_beer_data %>% 
  
  ggplot(mapping = aes(x = rating_average)) + 
  
  geom_density(col = "gray", 
               
               fill = "purple") +
  
  labs(x = "Rating Average",
       
       y = "Count",
       
       title = "Distribution of Beer Rating")) /

## Distribition of votes received

(full_beer_data %>% 
  
  ggplot(mapping = aes(x = votes, y = ..count..)) + 
  
  geom_density(col = "purple", 
               
               fill = "purple") +
  
  scale_x_log10() +
  
  labs(x = "Votes",
       
       y = "Count",
       
       title = "Distribution of Votes Cast") +


## Average rating versus alcohol percentage

full_beer_data %>% 
  
  ggplot(mapping = aes(x = rating_average, 
                       
                       y = alcohol_perc,
                       
                       col = type,
                       
                       size = votes)) +
  
  geom_point(show.legend = FALSE) +
  
  labs(x = "Average Rating",  y = "Alcohol Percentage",
       
       title = "Alcohol Percentage Versus Average Rating of Beers",
       
       caption = "Size shows number of votes. Colors show beer categories
       
       John Karuitha, 2022: Data Source: BeerAdvocate.com")
)

## @knitr Regression_analysis

my_beer_regression <- lm(rating_average ~ alcohol_perc + votes + factor(brewer), 
   
   data = full_beer_data) 


## @knitr regression output
stargazer::stargazer(my_beer_regression, type = "html")

## @knitr Plot_regression_analysis
par(mfrow = c(2, 2)) ## Create a 2x2 plot panel

plot(my_beer_regression)

par(mfrow=c(1,1)) ## Reset the plotting panel to a 1 X 1.

## Regression with interaction terms 
## Here, I run the regression with an interaction of brewer 
## and alcohol percentage
my_beer_regression_inter <- lm(rating_average ~ alcohol_perc + 
                                 
                                 votes + factor(brewer) + 
                                 
                                 alcohol_perc:factor(brewer), 
                               
                               data = full_beer_data)


# ## Compare performance of the models 
# 
# plot(performance::compare_performance(my_beer_regression, 
#                                       
#                                       my_beer_regression_inter))


