library(shiny)
library(tidyverse)

ui <- fluidPage(
    
    plotOutput("plot", height = "400px", width = "400px")
)


server <- function(input, output, session){
    output$plot <- renderPlot(plot(1:5, 
                                   main = "Histogram of 1 to 5",
                                   xlab = "Index",
                                   ylab = "Numbers"))
}



shinyApp(ui = ui, server = server)