##########################################################################################
if(!require(pacman)){
        install.packages("pacman")
}

p_load(tidyverse, rvest, polite, janitor)
p_load_gh("datarootsio/artyfarty")
p_load_gh("bbc/bbplot")

##########################################################################################

us_prisons_data <- polite::bow("https://en.wikipedia.org/wiki/Incarceration_in_the_United_States") %>% 
        
        polite::scrape() %>% 
        
        html_nodes("table") %>% 
        
        html_table() %>% 
        
        .[[3]] %>% 
        
        set_names(c("race", "%US pop", "%US prisons", "incarceration Rate")) %>% 
        
        filter(race %in% c("White (non-Hispanic)", "Hispanic", "Black", "Asian")) %>% 
        
        clean_names()


##########################################################################################

us_prisons_data %>% 
        
        pivot_longer(cols = -race, names_to = "pop_variables", values_to = "prop") %>% 
        
        mutate(race = factor(race),
               
               pop_variables = factor(pop_variables),
               
               prop = as.numeric(prop)) %>% 
        
        filter(pop_variables != "incarceration_rate") %>% 
        
        ggplot(mapping = aes(x = fct_reorder(race, prop, max), 
                             
                             y = prop,
                             
                             fill = pop_variables, col = pop_variables)) + 
        
        geom_col(position = "dodge") + 
        
        scale_fill_manual(values = c("purple", "red")) + 
        
        scale_color_manual(values = c("purple", "red")) + 
        
        bbplot::bbc_style() + 
        coord_flip()


#######################################################################################

us_prisons_data %>% 
        
        pivot_longer(cols = -race, names_to = "pop_variables", values_to = "prop") %>% 
        
        mutate(race = factor(race),
               
               pop_variables = factor(pop_variables),
               
               prop = as.numeric(prop)) %>% 
        
        filter(pop_variables != "incarceration_rate") %>% 
        
        ggplot(mapping = aes(x = race, 
                             y = prop, 
                             col = pop_variables)) + 
        
        geom_point(size = 20, 
                   shape = 1, 
                   stroke = 10, 
                   show.legend = FALSE) + 
        
        ggthemes::theme_clean() + 
        
        geom_text(mapping = aes(label = pop_variables),
                  
                  hjust = "outside", nudge_x = -0.225, size = 5, show.legend = FALSE) + 
        
        scale_color_manual(values = c( "gray40", "red")) + 
        
        labs(x =  "Race", y = "Proportion of Country Population/ Prison Population",
             
             title = "Incarceration Rates in the United States by Race",
             
             subtitle = "Blacks and Latinos are Over-represented in Prisons Relative to the Share of the Total Population", 
             
             caption = c("John Karuitha, 2021, 
                         
                         Data Source: Wikipedia")) + 
        
        scale_y_continuous(limits = c(0,70), breaks = seq(0,70,10)) +
        
        theme(plot.title = element_text(size = 26),
              
              plot.subtitle = element_text(size = 18),
              
              axis.title = element_text(size = 18),
              
              axis.text = element_text(size = 18)) + 
        coord_flip()


