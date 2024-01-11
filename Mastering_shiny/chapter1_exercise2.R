library(shiny)


ui <- fluidPage(
    
    sliderInput(inputId = "number", label = "If X is: ",
                min = 1, max = 50, value = 40),
    
    HTML("Then x times 5 is: "),
    
    textOutput("product")
)



server <- function(input, output, session){
    
    output$product <- renderText({
        
        input$number * 5
    })
    
}


shinyApp(ui = ui, server = server)