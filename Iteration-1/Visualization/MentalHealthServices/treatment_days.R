library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(xlsx)
library(ggplot2)
library(reshape)
library(reshape2)

#read data
trtdys_df = as.data.frame(read.xlsx("state_wise_treatment_days.xlsx", sheetName = "Sheet1"))

ui <- fluidPage(
  fluidRow(
    plotOutput('treatment_days_st')
  )
)

server <- function(input, output, session){
  s_td <- reactive({
    trtdys_df = trtdys_df[, c('State','X2013.14', 'X2014.15', 'X2015.16', 'X2016.17', 'X2017.18', 'X2018.19')]
    colnames(trtdys_df) <- c('State', '2013-2014', '2014-2015','2015-2016','2016-2017','2017-2018','2018-2019')
    
    yy_df = melt(trtdys_df, id = c("State"))
    yy_df$value = as.numeric(yy_df$value)
    
    yy_df
  })
  
  
  output$treatment_days_st <- renderPlot({
    g <- ggplot(data = s_td(), aes(group=State, variable, value, colour =State,
                           linetype = State)) +geom_point() +geom_line()+theme_bw()+xlab("Year") +ylab("Number of treatment days")+ggtitle('Average number of treatment days')+scale_y_continuous(limits=c(0, 20), breaks=c(0, 5, 10, 15, 20))
    
    g
  })
}

# Run the application
shinyApp(ui = ui, server = server)
