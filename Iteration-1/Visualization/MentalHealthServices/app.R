library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(xlsx)
library(ggplot2)
library(reshape)
library(reshape2)

#read data

commcen_df = as.data.frame(read.xlsx("community_service_centers.xlsx", sheetName = "Sheet1"))

ui <- fluidPage(
  mainPanel(
    column(4,
      selectInput(
      inputId = "select_st",
      label = "Select State:",
      choices = unique(commcen_df$State.Territory),
      selected = unique(commcen_df$State.Territory)[1]
    )),
    column(8,
      dataTableOutput("table")
    )
  )
)

server <- function(input, output, session){
  output$table <- renderDataTable({
    subset(commcen_df, State.Territory==input$select_st, select = "Establishment.name")
  })
}


shinyApp(ui, server)