if(!require(pacman)){
    install.packages("pacman")
}

p_load(tidyverse, janitor, shiny, glue)

ui <- fluidPage(
    
    textInput("greeting", 
              label = "Enter your name",
              value = "John Karuitha"),
    
    textOutput("sayhi")
    
)


server <- function(input, output, session){
    
    output$sayhi <- renderText({
        
        glue("Hello {input$greeting}, how is the going?")
    })
}


shinyApp(ui = ui, server = server)