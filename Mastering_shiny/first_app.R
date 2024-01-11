library(shiny)

ui <- fluidPage(
    
    selectInput(inputId = "dataset", label = "Dataset",
                choices = ls("package:datasets")),
    
    textInput(inputId = "name", 
              
              label = NULL,
              
              
              placeholder = "Enter name"),
    
    sliderInput(inputId = "date", 
                label = "Select a date",
                min = as.Date("2020-09-16"),
                max = as.Date("2020-09-23"),
                value = as.Date("2020-09-17"),
                timeFormat = "%F"
                
                ),
    
    
    ## Text output to summarise the data
    verbatimTextOutput("summary"),
    
    ## Table output with metrics of the data
    tableOutput("table1")
    
)


server <- function(input, output, session){
    
    dataset <- reactive({get(input$dataset, "package:datasets")})
    
    output$summary <- renderPrint({
        
        
        summary(dataset())
        
    })
    
    
    output$table1 <- renderTable({
        
        
        dataset()
        
    })
    
}


shinyApp(ui = ui, server = server)
