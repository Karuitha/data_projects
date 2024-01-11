if(!require(pacman)){
    install.packages("pacman")
}

p_load(tidyverse, janitor, shiny, glue)

ui <- fluidPage(
    
    textInput("name", "What is your name"),
    
    textOutput("greeting")
)


server <- function(input, output, session){
    
    greeting <- reactive(
        
        glue("Hello {input$name}")
        
    )
    
    output$greeting <- renderText(
        
        greeting()
    )
}


shinyApp(ui = ui, server = server)