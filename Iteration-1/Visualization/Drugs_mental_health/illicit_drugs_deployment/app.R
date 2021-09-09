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
  box(
    selectInput(
      inputId = "da_select_drug_type",
      label = "Select Drug Type:",
      choices = unique(drug_avail$DrugType),
      selected = "Base"
    )
  ),
  
  box(
    status = "warning", solidHeader = TRUE,
    collapsible = TRUE,
    splitLayout(cellWidths = c("50%", "50%"), withSpinner(plotOutput("sixteen_pie_chart")),
                withSpinner(plotOutput("seventeen_pie_chart"))),
    splitLayout(cellWidths = c("50%", "50%"),
                withSpinner(plotOutput("eighteen_pie_chart")),
                withSpinner(plotOutput("nineteen_pie_chart")))
    
  )
)



# Define server logic required to draw a histogram
server <- function(input, output,session) {

  
  #data for drug type and particular year
  sixteen_drug_type_val <- reactive({
    categories = c('Very easy', 'Easy', 'Difficult', 'Very difficult')
    val = c(drug_avail$Very.easy[drug_avail$Year == '2016' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Easy[drug_avail$Year == '2016' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Difficult[drug_avail$Year == '2016' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Very.difficult[drug_avail$Year == '2016' & drug_avail$DrugType == input$da_select_drug_type])
    
    data_six = data.frame(categories, val)
    
    data_six
  })
  seventeen_drug_type_val <- reactive({
    categories = c('Very easy', 'Easy', 'Difficult', 'Very difficult')
    val = c(drug_avail$Very.easy[drug_avail$Year == '2017' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Easy[drug_avail$Year == '2017' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Difficult[drug_avail$Year == '2017' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Very.difficult[drug_avail$Year == '2017' & drug_avail$DrugType == input$da_select_drug_type])
    
    data_six = data.frame(categories, val)
    
    data_six
  })
  eighteen_drug_type_val <- reactive({
    categories = c('Very easy', 'Easy', 'Difficult', 'Very difficult')
    val = c(drug_avail$Very.easy[drug_avail$Year == '2018' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Easy[drug_avail$Year == '2018' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Difficult[drug_avail$Year == '2018' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Very.difficult[drug_avail$Year == '2018' & drug_avail$DrugType == input$da_select_drug_type])
    
    data_six = data.frame(categories, val)
    
    data_six
  })
  nineteen_drug_type_val <- reactive({
    categories = c('Very easy', 'Easy', 'Difficult', 'Very difficult')
    val = c(drug_avail$Very.easy[drug_avail$Year == '2019' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Easy[drug_avail$Year == '2019' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Difficult[drug_avail$Year == '2019' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Very.difficult[drug_avail$Year == '2019' & drug_avail$DrugType == input$da_select_drug_type])
    
    data_six = data.frame(categories, val)
    
    data_six
  })
  
  output$sixteen_pie_chart <- renderPlot({
    data_set_six = sixteen_drug_type_val()
    pie(data_set_six$val, labels = data_set_six$categories, main = "2016")
  })
  
  output$seventeen_pie_chart <- renderPlot({
    data_set_six = seventeen_drug_type_val()
    pie(data_set_six$val, labels = data_set_six$categories, main = "2017")
  })
  
  output$eighteen_pie_chart <- renderPlot({
    data_set_six = eighteen_drug_type_val()
    pie(data_set_six$val, labels = data_set_six$categories, main = "2018")
  })
  
  output$nineteen_pie_chart <- renderPlot({
    data_set_six = nineteen_drug_type_val()
    pie(data_set_six$val, labels = data_set_six$categories, main = "2019")
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
