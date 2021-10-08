library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(wordcloud)
library(memoise)
library(plotly)
library(shinyjs)
library(xlsx)

#read data

provider_type_df = as.data.frame(read.xlsx("mental_health_sex_provider_type.xlsx", sheetName = "Sheet1"))
state_df = read.xlsx("Medicare-subsidised-mental-health-specific-services-tables.xlsx", sheetName = "Table MBS.3", startRow = 5, endRow = 67, header = TRUE ,as.data.frame = TRUE)

ui <- fluidPage(
  fluidRow(
    column(4,
           box(
             selectInput(
               inputId = "select_provider_type",
               label = "Select Provider Type:",
               choices = unique(provider_type_df$Provider_type),
               selected = unique(provider_type_df$Provider_type)[1]
             )
           )
           ),
    column(8,
           box(
             title = "Distribution of female population seeking help", status = "warning", solidHeader = TRUE,
             collapsible = TRUE,
             withSpinner(plotlyOutput("female_prov_type"))
           ),
           
           box(
             title = "Distribution of male population seeking help", status = "warning", solidHeader = TRUE,
             collapsible = TRUE,
             withSpinner(plotlyOutput("male_prov_type"))
           ))
    
    
  ),
  fluidRow(
    h2("    ")
  ),
  fluidRow(
    column(4,
           box(
             selectInput(
               inputId = "select_provider_type_2",
               label = "Select Provider Type:",
               choices = unique(state_df$Provider.type),
               selected = unique(state_df$Provider.type)[1]
             ),
             selectInput(
               inputId = "select_state",
               label = "Select State:",
               choices = unique(state_df$State.territory),
               selected = unique(state_df$State.territory)[1]
             )
           )
           ),
    column(8,
           h2("State wise distribution of service providers"),
            withSpinner(plotlyOutput("sw_prov_type"))
           
           )
  )
)

server <- function(input, output, session){

  f_plot_df <- reactive({
    sub_f_df = subset.data.frame(provider_type_df, Sex == "Female" & Provider_type == input$select_provider_type)
    data_st = data.frame(Year = c('2015-2016', '2016-2017', '2017-2018', '2018-2019', '2019-2020'), Values = as.numeric(sub_f_df[1 , 4:8]))
    data_st
  })
  
  
  m_plot_df <- reactive({
    sub_m_df = subset.data.frame(provider_type_df, Sex == "Male" & Provider_type == input$select_provider_type)
    data_st = data.frame(Year = c('2015-2016', '2016-2017', '2017-2018', '2018-2019', '2019-2020'), Values = as.numeric(sub_m_df[1 , 4:8]))
    data_st
  })
  
  sw_plot_df <- reactive({
    sub_m_df = subset.data.frame(state_df, State.territory == input$select_state & Provider.type == input$select_provider_type_2)
    data_st = data.frame(Year = c('2015-2016', '2016-2017', '2017-2018', '2018-2019', '2019-2020'), Values = as.numeric(state_df[1 , 10:14]))
  })
  
  
  output$female_prov_type <- renderPlotly({
    data_set_f = f_plot_df()
    plot_ly(data_set_f)  %>% add_trace(x = ~Year, y = ~Values,type = 'bar') %>% layout(xaxis = list(title = "Year"), yaxis = list(title = "Value"))
  })
  
  output$male_prov_type <- renderPlotly({
    data_set_m = m_plot_df()
    plot_ly(data_set_m)  %>% add_trace(x = ~Year, y = ~Values,type = 'bar') %>% layout(xaxis = list(title = "Year"), yaxis = list(title = "Value"))
  })
  
  output$sw_prov_type <- renderPlotly(({
    data_set_st = sw_plot_df()
    plot_ly(data_set_st)  %>% add_trace(x = ~Year, y = ~Values,type = 'bar') %>% layout(xaxis = list(title = "Year"), yaxis = list(title = "Value"))
    
  }))
}


# Run the application
shinyApp(ui = ui, server = server)