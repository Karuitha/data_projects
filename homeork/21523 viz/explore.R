if(!require(pacman)){
    install.packages(pacman)
}

pacman::p_load(explore, tidyverse, readxl, DataExplorer)

movies <- read_csv("toronto-movies.csv") %>% 
    
    mutate(across(
        
        .cols = c("imdb_votes", "imdb_rating", "metascore", "year"),
        
        .fns = parse_number
        
        
    ))

movies %>% summarise(n_distinct(genre))

movies %>% 
    group_by(genre) %>% 
    filter(n() > 5) %>% 
    count(genre)

head(movies)
names(movies)

movies %>% DataExplorer::create_report()

with(movies, plot(parse_number(imdb_votes), parse_number(imdb_rating)))

explore::explore_shiny(movies)
