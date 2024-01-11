library(shiny)


ui <- fluidPage(
    
    sliderInput(inputId = "number", label = "If X is: ",
                min = 1, max = 50, value = 40),
    
    
    sliderInput(inputId = "another_number", label = "And Y is: ",
                min = 1, max = 50, value = 40),
    
    HTML("Then X times Y is: "),
    
    textOutput("product")
)



server <- function(input, output, session){
    
    output$product <- renderText({
        
        input$number * input$another_number
    })
    
}


shinyApp(ui = ui, server = server)