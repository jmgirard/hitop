library(shiny)
library(ggplot2)

ui <- fluidPage(
  dataTableOutput("table")
)
server <- function(input, output, session) {
  output$table <- DT::renderDataTable(mtcars, options = list(pageLength = 5))
}

shinyApp(ui, server)
