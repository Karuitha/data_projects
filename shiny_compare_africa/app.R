## Load libraries

if(!require(pacman)){
        
        install.packages("pacman")
        
}

## Download and load required packages
pacman::p_load(tidyverse, shiny, gapminder, glue, 
               
               gganimate, ggthemes, tidyquant)

## Set theme from tidyquant
theme_set(tidyquant::theme_tq())

## Remove all scientific notation
options(scipen = 999)



## make the app
ui <- fluidPage(
        
        ## Title of the panels
        titlePanel(title = "Comparing GDP & Life Expectancy of African Countries Using Gapminder Data, @Karuitha 2021", 
                   
                   windowTitle = "Country Comparisons"
                   
        ),
        
        shinythemes::shinytheme("superhero"),
        
        sidebarLayout(
                sidebarPanel(
                        ## Enter the name of country
                        textInput(inputId = "country_1", label = "Enter Country", value = "Kenya"), 
                        
                        textInput(inputId = "country_2", label = "Enter Another Country", value = "Ghana"),
                        
                ),
                
                mainPanel(
                        
                        tabsetPanel(
                                
                                tabPanel("Comparisons",
                                         ## What type of output
                                         plotly::plotlyOutput("life_exp"),
                                         
                                         ## Another type of output
                                         plotly::plotlyOutput("gdp_capita")),
                                
                                
                                tabPanel("Data Table",
                                         DT::DTOutput("table"))))))



server <- function(input, output){
        
        output$life_exp <- plotly::renderPlotly(ggplot(
                
                data = gapminder::gapminder %>% filter(country %in% c(input$country_1, input$country_2))) + 
                        
                        geom_point(mapping = aes(x = year, y = lifeExp, size = pop, col = country)) + 
                        
                        geom_line(mapping = aes(x = year, y = lifeExp, col = country)) + 
                        
                        labs(title = glue("Life Expectancy in {input$country_1} & {input$country_2} 1957-2017", 
                                          
                                          caption = "John Karuitha 2021"
                                          
                        )) +
                        
                        theme(legend.position = "none") + 
                        
                        scale_color_viridis_d()
                
                
        )
        
        output$gdp_capita <- plotly::renderPlotly(
                
                ggplot(data = gapminder::gapminder %>% filter(country %in% c(input$country_1, input$country_2))) + 
                        
                        geom_point(mapping = aes(x = year, y = gdpPercap, 
                                                 
                                                 size = pop, col = country,
                                                 
                                                 show.legend = FALSE)) + 
                        
                        geom_line(mapping = aes(x = year, y = gdpPercap, col = country), 
                                  
                                  show.legend = FALSE) +
                        
                        labs(title = glue("GDP per Capita in {input$country_1} & {input$country_2} 1957-2017", 
                                          
                                          caption = "John Karuitha 2021"
                                          
                                          
                        )) +
                        
                        theme(legend.position = "bottom") + 
                        
                        scale_color_viridis_d()
                
        )
        
        
        output$table <- DT::renderDT(gapminder::gapminder %>% 
                                             
                                             
                                             filter(country %in% c(input$country_1, input$country_2)) 
                                     
        )
        
        
        
        
        
}



shinyApp(ui = ui, server = server)
