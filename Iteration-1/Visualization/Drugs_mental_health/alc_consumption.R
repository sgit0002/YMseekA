library(shiny)
library(ggplot2)
library(shinydashboard)
library(shinycssloaders)
library(wordcloud)
library(memoise)
library(plotly)
library(shinyjs)


#read the files
alcohol_consumption = read.csv("alcohol_consumption.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    title = "Alcohol Consumption over the years", status = "warning", solidHeader = TRUE,
    collapsible = TRUE,
    withSpinner(plotlyOutput("alcohol_cons_bar_plot"))
  )
)



# Define server logic required to draw a histogram
server <- function(input, output,session) {
  output$alcohol_cons_bar_plot <- renderPlotly({
    plot_ly(alcohol_consumption)  %>% add_trace(x = ~Year, y = ~Alcohol_Volume,type = 'bar') %>% layout(xaxis = list(title = "Year"), yaxis = list(title = "Alcohol Consumption ('000 lts)"))
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
