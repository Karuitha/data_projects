if(!require(pacman)){
    install.packages("pacman")
}

p_load(tidyverse, janitor, shiny, glue)

ui <- fluidPage(
    
    textInput("name", "What is your name"),
    
    textOutput("greeting")
)


server <- function(input, output, session){
    output$greeting <- renderText(
        
        glue("Hello {input$name}")
    )
}


shinyApp(ui = ui, server = server)