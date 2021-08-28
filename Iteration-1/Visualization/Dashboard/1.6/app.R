#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages("tidyverse")
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(wordcloud)
library(plotly)
# library(tibble)
library(tidyverse)
library(dplyr)
# library(reshape)
# library(data.table)
library(conflicted)
# library(ggplot2)



conflict_prefer("box", "shinydashboard")
filtered_data <- read.csv("common_diagnosis.csv")
rate_admit <- read.csv("rate_admit.csv")



# Define UI for application that draws a histogram
ui <- dashboardPage(
  # Header of the dashboard
  dashboardHeader(title = "Infograpics"), # Name of the project
  dashboardSidebar(sidebarMenu(
    # All the Dashboard menu items 
    menuItem("Hospitals", tabName = "hospital", icon = icon("dashboard")) 
  )),
  
  dashboardBody(
    
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    # 
    # ),
    
    
    tags$style(HTML(".tabbable > .nav > li > a[data-value='Common mental issues'] {background-color: black;  color:black}")),
    
    
    tabsetPanel(
      tabItem(tabName = "hospital", tabBox(height = "1500px", width = "1000px",
                                           
                                           
                                           tabPanel("Common mental issues", h2("Common Mental Diagnosis"), 
                                                    # First tab content
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
                                                                  c("Public Hospitals", "Public Psychiatric Hospitals", "All Public Hospitals (Australia)",
                                                                    "Private Hospitals", "All Hospitals (Public + Private)"),
                                                                  selected = "Public Hospitals"
                                                                )
                                                              )
                                                            ),
                                                            
                                                            box(
                                                              title = "Bar plot", status = "warning", solidHeader = TRUE,
                                                              collapsible = TRUE,
                                                              withSpinner(plotlyOutput("bar_plot"))),
                                                            
                                                            box(
                                                              title = "Info", status = "warning",
                                                              withSpinner(htmlOutput("plot_common_text")))
                                                            
                                                    )
                                           ),
                                           # second tab rate of admittance
                                           tabPanel("Rate of admittance", h2("No. of cases admitted"),
                                                    tabItem(tabName = "hospital",
                                                            fluidRow(
                                                              
                                                              box(
                                                                title = "Wordcloud", status = "warning", solidHeader = TRUE,
                                                                collapsible = TRUE,
                                                                withSpinner(plotlyOutput("plot_rate"))),
                                                              
                                                              box(
                                                                title = "Inputs", background = "black",  solidHeader = TRUE,
                                                                collapsible = TRUE,
                                                                selectInput(
                                                                  "selection_type",
                                                                  "Select separations",
                                                                  unique(rate_admit$Separation.type),
                                                                  selected = "With specialised psychiatric care"
                                                                )
                                                              ),
                                                              
                                                              box(
                                                                title = "Inputs", background = "black",  solidHeader = TRUE,
                                                                collapsible = TRUE,
                                                                selectInput(
                                                                  "age",
                                                                  "Select the age group",
                                                                  unique(rate_admit$Age.group),
                                                                  selected = "18-24 years"
                                                                )
                                                              ),
                                                              
                                                              box(
                                                                title = "Info", status = "warning",
                                                                withSpinner(htmlOutput("plot_rate_text")))
                                                              
                                                            )
                                                            
                                                    )
                                                    
                                           ),
                                           # Third tab 
                                           tabPanel("Professional help", h2("How well does professional help assist you?"),
                                                    tabItem(tabName = "hospital",
                                                            fluidRow(
                                                              
                                                              box(
                                                                title = "Wordcloud", status = "warning", solidHeader = TRUE,
                                                                collapsible = TRUE,
                                                                withSpinner(plotlyOutput("plot_help"))),
                                                              
                                                              box(
                                                                title = "Inputs", background = "black",  solidHeader = TRUE,
                                                                collapsible = TRUE,
                                                                selectInput(
                                                                  "age_care",
                                                                  "Select separations",
                                                                  unique(rate_admit$Age.group),
                                                                  selected = "18-24 years"
                                                                )
                                                              )
                                                              
                                                            ),
                                                            
                                                            box(
                                                              title = "Info", status = "warning",
                                                              withSpinner(htmlOutput("plot_text")))
                                                    )
                                                    
                                           )
      )
      )
      
    )
    
    
  )
)



# Define server logic required
server <- function(input, output, session) {
  
  
  
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    
    names(filtered_data)[5] <- "Public Hospitals"
    names(filtered_data)[6] <- "Public Psychiatric Hospitals"
    names(filtered_data)[7] <- "All Public Hospitals (Australia)"
    names(filtered_data)[8] <- "Private Hospitals"
    names(filtered_data)[9] <- "All Hospitals (Public + Private)"
    
    
    
    b <- filtered_data[, c("Principal.diagnosis", input$selection)]
    
    b <- b[1:35,]
    
    b <- b[order(-b[,2]),]
    
    b[is.na(b)] <- 0
    
    b
    
  })
  
  rate <- reactive({
    
    c <- rate_admit %>% dplyr::filter(Age.group == input$age, Separation.type == 
                                 input$selection_type, 
                               Measure == "Separations")
    c <- c[, c(2:18)]
    
    # c[,6] <- as.numeric(c[,6])
    # print(c)
    c
    
  })
  
  hos_help <- reactive({
    d <- rate_admit %>% dplyr::filter(Age.group == input$age_care, Measure == "Separations", Sex=="Persons")
    
    d
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    # print(v)
    
   
    wordcloud_rep(v[,1], (v[,2]/sum(v[,2]))*100, scale=c(3,0.01), use.r.layout = T,
                  min.freq = 1, max.words=100,
                  colors=brewer.pal(8, "Dark2"))
  
    
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
  
  output$plot_rate <- renderPlotly({
    r <- rate()
    
    r <- spread(gather(r, variable, value, -Measure, -Sex, -Separation.type, -Age.group), Sex, value)
    
    
    r$variable <- gsub("X","", as.character(r$variable))
    
    r$Female <- (r$Female/sum(r$Female))*100
    r$Male <- (r$Male/sum(r$Male))*100
    
    plot_ly(r, x = ~variable, y = ~Male, name = 'Male', type = 'scatter', mode='lines+markers') %>% 
      add_trace(y = ~Female, name= 'Female', type='scatter', mode = 'lines+markers') # %>% 
    # add_trace(y = ~Persons, name = 'Persons', mode = 'lines+markers')
    
  })
  
  output$plot_help <- renderPlotly({
    h <- hos_help()
    # print(h)
    # h$average <- rowMeans(h[,c(5:17)])
    
    plot_ly(h, x=~Separation.type, y=~Average.annual.change..per.cent..2008.09.to.2018.19, type='bar')
  })
  
  
  output$plot_text <- renderUI({
    HTML(paste("The graph shows how well people in different 
               age groups have fared using specialized pyschiatric
               care as opposed to without specialized care.", "For example, for the age group 18-24 year olds, 
               there has been an increase in the percentage of people consulting 
               the professionals (specialized psychiatric care). This shows each year people have gone back 
               to the psychiatrics to deal with their mental problems they are going through. 
               And since that was helping them cope with their mental illness, 
               there was an increase in the number of people every year.", "The value shown in the above graph 
               indicates the average percentage of people who actually went back to 
               consult the psychiatrics is a cummulative average from 2008 to 2019.", sep="<br/><br/>"))
  })
  
  output$plot_rate_text <- renderUI({
    HTML("The rate of admittance shows how have men and women fared since 2007 to 2019 given they either reached out for help or they 
    did not. <br><br>
    <b><big><big> With psychiatric care </big></big></b>
    
          <br><br> This graph indicates 2 main aspects that are vital for any age-groups: <br> &ensp; &ensp; &ensp; 
          1. If male and/or female have been vocal about their mental health issue(s) <br> &ensp; &ensp; &ensp;
          2. Who among both the genders have been a victim of mental health issue(s) more?. <br><br> 
         
         For example, for the age group 18-24 year olds, female have been increasingly vocal about their mental health
         issue(s) with their psychiatrist(s) - every year the percentage of women going back to the psychiartists is increasing (a 
         positive sign of people understanding the importance of treating their mental health and going back to professional help,
         which shows the professional help is actually working!) <br><br>
         And since they have been consulting their psychiatrist(s), the number of 
         cases have been recorded officially. <br> However, even though men have shown an increase in opening up to 
         their psychiatrists, they fall behind women by merely 1%. <br><br>
         
    <b><big><big> Without psychiatric care </big></big></b>
         <br><br> This graph shows that men and women who have always thought of not consulting a professional for help, have been increasing 
         in the recent past (2015-2019) which is a major concern for individuals as well as the nation. <br><br>
         
         The men and women tend to not be vocal about their mental health here, which inturn might also affect the official count of people
         who do not exist on record and this might lead to a huge number of people being undetected with mental illness.
         
         "
         
         
    )
  })
  
  output$plot_common_text <- renderUI({
    HTML('Before we jump in on the future, lets play a little around to answer the only question that mankind has ever asked - <i>"Why exactly
    am I here?"</i>. If looking at this question, you wonder <i>"I \'m not here looking out for answers that the mankind
    has failed to answer yet!"</i> - you are at the right place!  We are not here for that too. We are here, for you! <br><br>
    
    Common mental health diagnosis shows which are the top 10 common mental health issues that have been recorded in 
    hospitals in & around Australia <br><br>
    
    If, for some reason, you think to yourself <i>"I guess I am an outcast having this mental disorder, and I do not want my friends or family
    or anyone to know that I am going through such a pain" </i> - think twice! Because many people go through various mental disorders 
    but they get themselves treated right. And rightly so, that is how the norm should be. You are not alone in this. You do not have to go through this 
    alone! We and the society, are all here for you! <br><br>
    
    
    <b><big>For more details on </big></b><br>
    &ensp; &ensp; &ensp;1. Schizophrenia, click <a href="https://www.healthdirect.gov.au/schizophrenia">here</a> <br>
    &ensp; &ensp; &ensp;2. Depression, click <a href="https://www.healthdirect.gov.au/depression">here</a> <br>
    &ensp; &ensp; &ensp;3. Stress, click <a href="https://www.healthdirect.gov.au/stress">here</a> <br>
    &ensp; &ensp; &ensp;4. Personality disorder, click <a href="https://www.healthdirect.gov.au/personality-related-disorders">here</a> <br>
    &ensp; &ensp; &ensp;5. Bipolar disorder, click <a href="https://www.healthdirect.gov.au/bipolar-disorder">here</a> <br>
    &ensp; &ensp; &ensp;6. Schizoaffective, click <a href="https://www.healthdirect.gov.au/schizophrenia">here</a> <br>
    &ensp; &ensp; &ensp;7. Other anxieties, click <a href="https://www.healthdirect.gov.au/anxiety">here</a> <br>
    &ensp; &ensp; &ensp;8. Schizotypal, click <a href="https://www.healthdirect.gov.au/schizophrenia">here</a> <br>
    &ensp; &ensp; &ensp;9. Alcohol influenced, click <a href="https://adf.org.au/reducing-risk/aod-mental-health/aod-mental-health-resources/">here</a> <br>
    &ensp; &ensp; &ensp;10. Recurrent depression, click <a href="https://www.medicalnewstoday.com/articles/320269">here</a> <br><br><br>
    
    <b><big>Further details on </big></b><br> 
    &ensp; &ensp; &ensp;1. Public hospitals, click <a href="https://www1.health.gov.au/internet/main/publishing.nsf/Content/hospitals2.html">here</a>.  <br>   
    &ensp; &ensp; &ensp;2. Public psychiatric hospitals, click <a href="https://www.betterhealth.vic.gov.au/health/servicesandsupport/hospitals-clinics-and-residential-options-for-mental-illness">here</a>  <br> 
    &ensp; &ensp; &ensp;3. Public private hospitals, click <a href="https://www.healthdirect.gov.au/understanding-the-public-and-private-hospital-systems">here</a> <br>    
         
         
         '
         
         
    )
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)


