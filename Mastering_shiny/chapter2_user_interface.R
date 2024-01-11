library(shiny)


ui <- fluidPage(
    
    radioButtons(
        
        inputId = "pet",
        label = "Choose one:",
        choiceNames = list(
            
            icon("angry"),
            icon("smile"),
            icon("sad-tear")
        ),
        
        choiceValues = list("Angry", "Happy", "Sad")
    ),
    
    fileInput("file", "Upload a file here: "),
    
    
    textOutput("my_mood")
    
)



server <- function(input, output, session){
    output$my_mood <- renderText({
        
        print(paste("I am", input$pet))
    })
}


shinyApp(ui = ui, server = server)