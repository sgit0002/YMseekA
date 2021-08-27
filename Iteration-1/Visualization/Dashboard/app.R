#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(shinydashboard)
library(shinycssloaders)
library(wordcloud)
library(memoise)
library(plotly)


# Read the csv file
filtered_data = read.csv("filtered_data.csv")
separations = read.csv("separations.csv")$x



# Define UI for application that draws a histogram
ui <- dashboardPage(
  # Header of the dashboard
  dashboardHeader(title = "Infographics"), # Name of the project
  dashboardSidebar(sidebarMenu(
    # All the Dashboard menu items 
    menuItem("Hospitals", tabName = "hospital", icon = icon("dashboard")),
    menuItem("Drug Use", tabName = "druguse", icon =icon("dashboard"))
    
  )),
  
  dashboardBody(
    
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    #   
    # ),
    
    tabItems(
      # Second tab content
      tabItem(tabName = "druguse",
              fluidRow()),
      #
      tabItem(tabName = "hospital",
              fluidRow(
                
                box(
                  title = "Wordcloud", status = "warning", solidHeader = TRUE,
                  collapsible = TRUE,
                  withSpinner(plotOutput("plot"))),
                
                box(
                  title = "Inputs", background = "black",  solidHeader = TRUE,
                  collapsible = TRUE,
                  selectInput(
                    "selection",
                    "Select separations",
                    separations,
                    selected = "Public Hospitals"
                  )
                )
              ),
              box(
                title = "Bar plot", status = "warning", solidHeader = TRUE,
                collapsible = TRUE,
                withSpinner(plotlyOutput("bar_plot")))
              
      )
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output,session) {

  # Define a reactive expression for the document term matrix
  terms <- reactive({
    
        
        
        b <- filtered_data[, c("Principal.diagnosis", input$selection)]
        
        b <- b[1:35,]
        
        b <- b[order(-b[,2]),]
        
        b[is.na(b)] <- 0
        
        b
        
      
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    print(v)
    wordcloud_rep(v[,1], v[,2], scale=c(4,0.1), use.r.layout = T, 
                  min.freq = min(v[,2]), max.words=max(v[,2]),
                  colors=brewer.pal(4, "Dark2"))
  })
  
  output$bar_plot <- renderPlotly({
    v <- terms()
    
    v <- v[order(-v[,2]),][1:10,]
    names(v)[1] <- "Diag"
    names(v)[2] <- "res"
    
    v[,2] <- round((v[,2]/sum(v[,2])) * 100, 2)
    labels <- v$Diag
    per <- v$res
    plot_ly(v)  %>% add_trace(x = ~Diag, y = ~res,type = 'bar')
    
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)


