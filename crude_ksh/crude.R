## Load required packages ----
if(!require(pacman)){
  install.packages("pacman")
}

p_load(tidyverse, janitor, skimr, 
       tidyquant, rvest, ggthemes,
       ggtext, modeltime, timetk)

theme_set(theme_wsj() + 
            theme(
              panel.grid.major = element_blank()
            )) 
## Data 
ksh_dollar <- read_html("https://www.currency-converter.org.uk/currency-rates/historical/table/USD-KES.html") %>% 
  html_nodes("table") %>% 
  html_table() %>% 
  .[[1]] %>% 
  row_to_names(1) %>% 
  clean_names() %>% 
  select(-us_dollar, -analysis) %>% 
  set_names(c("week_day", "date", "ksh_dollar")) %>% 
  mutate(date = dmy(date),
         ksh_dollar = parse_number(ksh_dollar))

## https://datasource.kapsarc.org/explore/dataset/spot-prices-for-crude-oil-and-petroleum-products/export/?sort=period&q.timerange.period=period:%5B1999-01-01+TO+2023-09-15%5D
oil_barrel <- read_csv("spot-prices-for-crude-oil-and-petroleum-products.csv") %>% 
  clean_names() %>% 
  set_names("data_oil") %>% 
  separate(data_oil, into = c("date", 
                              "brent_spot", 
                              "wti_spot"),
           sep = ";") %>% 
  mutate(across(
    .cols = -date,
    .fns = as.numeric
  )) %>% 
  mutate(date = ymd(date))
  
  

ksh_dollar %>% 
  ggplot() + 
  geom_line(aes(x = date, 
                y = ksh_dollar), 
            color = "red",
            alpha = 0.5,
            linewidth = 1) + 
  geom_line(data = oil_barrel %>% 
              filter(date >= '2009-10-06'), 
            aes(x = date, y = brent_spot),
            color = "blue",
            alpha = 0.5) + 
  geom_line(data = oil_barrel %>% 
              filter(date >= '2009-10-06'), 
            aes(x = date, y = wti_spot),
            color = "green",
            alpha = 0.3,
            linewidth = 1) +
  theme_tq() + 
  labs(x = "", y = "US$-KES Exchange Rate/Oil Price",
       title = "Trends in US$-KES Exchange Rate and Oil Prices",
       subtitle = "<span style = 'color: red;'>Exchange Rate</span> is Trending up.\n<span style = 'color: blue;'>Brent Crude</span> and <span style = 'color: green;'>WTI Crude</span> are More Volatile.",
       caption = "John Karuitha 2022\nUsing R and GGPLOT2") + 
  theme(plot.subtitle = element_markdown())



