library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(xlsx)
library(ggplot2)
library(reshape)
library(reshape2)
#read data

impact_services_df = as.data.frame(read.xlsx("mental_services_outcome.xlsx", sheetName = "Sheet1"))

ui <- fluidPage(
  fluidRow(
    column(4,
           box(
             selectInput(
               inputId = "select_state",
               label = "Select State:",
               choices = unique(impact_services_df$State),
               selected = unique(impact_services_df$State)[1]
             ),
             selectInput(
               inputId = "select_cg",
               label = "Select Consumer Group:",
               choices = unique(impact_services_df$Consumer.Group),
               selected = unique(impact_services_df$Consumer.Group)[1]
             )
           )
    ),
    column(8,
           #h2(""),
           plotOutput("sw_prov_type")
    )
  )
)

server <- function(input, output, session){
  
  sw_pd <- reactive({
    
    sub_f_df = subset.data.frame(impact_services_df, State == input$select_state & Consumer.Group == input$select_cg)
    print(sub_f_df)
    xx_df = sub_f_df[, c('Outcome', 'X2014.15', 'X2015.16', 'X2016.17', 'X2017.18', 'X2018.19')]
    
    colnames(xx_df) <- c('Outcome', '2014-2015','2015-2016','2016-2017','2017-2018','2018-2019')
    
    yy_df = melt(xx_df, id = c("Outcome"))
    
    yy_df$value = as.numeric(yy_df$value)

    yy_df
    
  })
  
  output$sw_prov_type <- renderPlot({
    g <- ggplot(data = sw_pd(), aes(group=Outcome, x=variable, y=value, colour =Outcome,
                   linetype = Outcome)) + geom_point() + geom_line()+theme_bw()+xlab("Year") +ylab("Percentage of people")+ggtitle('Impact of mental health services in Australia')
    g
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)