library(shiny)
library(tidyverse)


ui <- fluidPage(
    
    sliderInput(
        
        inputId = "bin",
        label = "Select bin",
        value = 10,
        min = 1,
        max = 30,
        animate = TRUE
    ),
    
    plotOutput("myplot", height = 800)
)


server <- function(input, output, session){
    
    my_data <- reactive({
        
        set.seed(200)
        data.frame(values = rnorm(1000, mean = 0, sd = 1))
    })
    
    
    output$myplot <- renderPlot({
        
        my_data() %>% 
            ggplot(mapping = aes(x = values)) + 
            geom_histogram(bins = input$bin) + 
            artyfarty::theme_bain()
    })
}



shinyApp(ui = ui, server = server)