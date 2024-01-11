## @knitr enter_url
url_address <- "https://en.wikipedia.org/wiki/World_population"

## @knitr scraper_code
## Scrape the page for tables with data
population_tables <- read_html(url_address) %>%
  html_nodes("table") %>%
  html_table()

head(population_tables, 2)

## @knitr get_data
## Capture the first few tables and clean the data
first_table <- population_tables %>% 
  .[[1]]

first_table

## @knitr data_cleaning
names(first_table) <- c("no", "country", "pop_2000", 
                        "pop_2015", "pop_2020")

first_table_clean <- first_table %>% 
  
  filter(no != "#") %>%
  
  mutate(no = 1:12) %>%
  
  filter(no != 12) %>%
  
  mutate(pop_2000 = parse_number(pop_2000),
         pop_2015 = parse_number(pop_2015),
         pop_2020 = parse_number(pop_2020),
         country = str_remove_all(country, "\\[.\\]"))

first_table_clean

## @knitr pop_graph_year
first_table_clean %>% 
  
  mutate(pop_2000 = round(pop_2000/6127 * 100),
         pop_2015 = round(pop_2015/7349 * 100),
         pop_2020 = round(pop_2020/8501 * 100)) %>%
  
  select(-no) %>%
  
  pivot_longer(-country, names_to = "years", values_to = "pop") %>%
  
  filter(country != "World total") %>%
  
  ggplot(mapping = aes(x = fct_reorder(country, pop, max), 
                       
                       y = pop, fill = years)) + 
  
  geom_col(show.legend = FALSE) + 
  
  geom_text(aes(label = pop)) +
  
  facet_wrap(~ years) +
  
  ggthemes::theme_wsj() +
  
  coord_flip() +
  
  labs(title = "Most Populous Countries- 2000-2020, %")
  
  