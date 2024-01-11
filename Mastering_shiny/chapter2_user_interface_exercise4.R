library(shiny)


ui <- fluidPage(
    
    sliderInput(inputId = "select", 
                label = "Select number",
                value = 10,
                min = 0,
                max = 100,
                step = 5,
                animate = TRUE),
    
    textOutput("john")
)


server <- function(input, output, session){
    output$john <- renderText({
        
        print(input$select)
    })
}


shinyApp(ui = ui, server = server)