library(shiny)


ui <- fluidPage(
    
    textInput(inputId = "name", 
              label = "Enter Name",
              value = "John Karuitha",
              placeholder = "For example, John Karuitha"),
    
    verbatimTextOutput("greeting")
    
)



server <- function(input, output, session){
    
    output$greeting <- renderText({
        
        paste("Hello,", input$name)
        
    })
}


shinyApp(ui = ui, server = server)