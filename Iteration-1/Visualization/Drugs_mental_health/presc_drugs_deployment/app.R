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
benzo_presc = read.csv("benzodiazepines_prescription.csv")
drug_avail = read.csv("drug_availability.csv")
drug_use_st = read.csv("drug_use_state_data.csv")
drug_source = read.csv("drugs_source.csv")
missing_work_drugs = read.csv("missing_work_drugs.csv")
opioids_presc = read.csv("opioids_prescritpion.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    title = "How is the use of prescription drugs impacting mental health?", 
    box(
      selectInput(
        "pd_select_drug_type",
        "Select Drug Type:",
        c("Benzodiazepines", "Opioids"),
        selected = "Benzodiazepines"
      ),
      selectInput(
        inputId = "pd_select_year",
        label = "Select Year:",
        choices = sub('X', '', colnames(benzo_presc[,3:10])),
        selected = "Opioids"
      )
    ),
    box(
      withSpinner(plotOutput("popular_drug_wordcloud"))
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    
    
    if(input$pd_select_drug_type == "Benzodiazepines"){
      b <- benzo_presc[, c('Drug', paste('X',input$pd_select_year,sep=""))]
    }else{
      b <- opioids_presc[, c('Drug', paste('X',input$pd_select_year,sep=""))]
    }
    b <- b[order(-b[,2]),]
    
    b[is.na(b)] <- 0
    
    b
  })
  
  
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$popular_drug_wordcloud <- renderPlot({
    v <- terms()
    wordcloud_rep(v[,1], v[,2], scale=c(10,0.25), use.r.layout = T, 
                  min.freq = 1, max.words=100,
                  colors=brewer.pal(4, "Dark2"))
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)