library(shiny)


ui <- fluidPage(
    
    selectInput(
        
        inputId = "name",
        label = "Select name",
        selected = "Ann",
        choices = list(smallnames = c("Ann", "Boy"),
                       bignames = c("Dodge", "Paul"))
    )
)


server <- function(input, output, session){
    
}


shinyApp(ui = ui, server = server)