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



conflict_prefer("layout", "plotly")
conflict_prefer("box", "shinydashboard")
filtered_data <- read.csv("common_diagnosis.csv")
rate_admit <- read.csv("rate_admit.csv")



# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Sidebar with a slider input for number of bins 
  
    
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "selection",
          "Select hospitals",
          c("Public Hospitals", "Public Psychiatric Hospitals", "All Public Hospitals (Australia)",
            "Private Hospitals", "All Hospitals (Public + Private)"),
          selected = "Public Hospitals"
        )
    
  , width = 3, position='right'),
  # Show a plot of the generated distribution
  mainPanel(
    
    
    
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"), withSpinner(plotOutput("plot")), withSpinner(plotlyOutput("bar_plot")))
    )
    
  )
  ),
  
    
)
help("sidebarPanel")


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
    
    # rate <- reactive({
    #     
    #     c <- rate_admit %>% dplyr::filter(Age.group == input$age, Separation.type == 
    #                                           input$selection_type, 
    #                                       Measure == "Separations")
    #     c <- c[, c(2:18)]
    #     
    #     # c[,6] <- as.numeric(c[,6])
    #     # print(c)
    #     c
    #     
    # })
    # 
    # hos_help <- reactive({
    #     d <- rate_admit %>% dplyr::filter(Age.group == input$age_care, Measure == "Separations", Sex=="Persons")
    #     
    #     d
    # })
    
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
        plot_ly(v)  %>% add_trace(x = ~Diag, y = ~res,type = 'bar') %>%
            layout(title = 'Most Common Mental Health Issues (Australia)', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Mental health'), 
                   yaxis = list(title = 'Percentage'))
        
    })
    
    # output$plot_rate <- renderPlotly({
    #     r <- rate()
    #     
    #     r <- spread(gather(r, variable, value, -Measure, -Sex, -Separation.type, -Age.group), Sex, value)
    #     
    #     
    #     r$variable <- gsub("X","", as.character(r$variable))
    #     
    #     r$Female <- (r$Female/sum(r$Female))*100
    #     r$Male <- (r$Male/sum(r$Male))*100
    #     
    #     plot_ly(r, x = ~variable, y = ~Male, name = 'Male', type = 'scatter', mode='lines+markers') %>% 
    #         add_trace(y = ~Female, name= 'Female', type='scatter', mode = 'lines+markers')  %>%
    #         layout(title = 'Proportion of mentally affected patients admitted (2007-2019, Australia)', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Years'), 
    #                yaxis = list(title = 'Percentage'))
    #     
    # })
    
    # output$plot_help <- renderPlotly({
    #     h <- hos_help()
    #     
    #     plot_ly(h, x=~Separation.type, y=~Average.annual.change..per.cent..2008.09.to.2018.19, type='bar') %>%
    #         layout(title = 'Proportion of mentally affected people seeking professional help (Australia)', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Type of care sought'), 
    #                yaxis = list(title = 'Percentage'))
    # })
    
    
    # output$plot_text <- renderUI({
    #     HTML(paste("The graph shows how well people in different 
    #            age groups have fared using specialized pyschiatric
    #            care as opposed to without specialized care.", "For example, for the age group 18-24 year olds, 
    #            there has been an increase in the percentage of people consulting 
    #            the professionals (specialized psychiatric care). This shows each year people have gone back 
    #            to the psychiatrics to deal with their mental problems they are going through. 
    #            And since that was helping them cope with their mental illness, 
    #            there was an increase in the number of people every year.", "The value shown in the above graph 
    #            indicates the average percentage of people who actually went back to 
    #            consult the psychiatrics is a cummulative average from 2008 to 2019.", sep="<br/><br/>"))
    # })
    
    # output$plot_rate_text <- renderUI({
    #     HTML("A tale of the past shows how have men and women fared since 2007 to 2019 given they either reached out for help or they 
    # did not.<br><br> Lets try to unravel the mystrious story that the line-graph holds within. <br><br>
    # 
    # <b><big><big> With psychiatric care </big></big></b><br><br>
    #       
    #       This graph indicates 2 main aspects that are vital for any age-groups: <br> &ensp; &ensp; &ensp; 
    #       1. If male and/or female have been vocal about their mental health issue(s) <br> &ensp; &ensp; &ensp;
    #       2. Who among both the genders have been a victim of mental health issue(s) more?. <br><br> 
    #      
    #       
    #      For example, for the age group 18-24 year olds, female have been increasingly vocal about their mental health
    #      issue(s) with their psychiatrist(s) - every year the percentage of women going back to the psychiartists is increasing (a 
    #      positive sign of people understanding the importance of treating their mental health and going back to professional help,
    #      which shows the professional help is actually working!) <br><br>
    #      
    #      This reveals the rather unspoken stages that you must go through when you get a sense of feeling sick in your mind: <br> &ensp; &ensp; &ensp; 
    #      1. consulting a specialized professional help <br> &ensp; &ensp; &ensp; 
    #      2. on the professional help's advice, visit a hospital, or <br>&ensp; &ensp; &ensp;
    #      3. both the above steps can be done at the same place (for e.g., Clinical services)<br><br><br><br>
    #      
    #   
    #      
    # <b><big><big> Without psychiatric care </big></big></b>
    #      <br><br> People who have been going to communities that focuses on activities and programs that help them manage their own 
    #      recovery and maximise their participation in community life (Non-Clinical services) <br>
    #      
    #      This graph shows the proportion of men and women who might not be comfortable going for professional help, have been increasing 
    #      in the recent past (2015-2019) which may result in a concern for individuals as well as the nation. <br><br>
    #      
    #      A problem can be sensed here, since men and women tend to not be vocal about their mental health as openly as others. 
    #      But Non-Clinical services is a great start for anyone who is begining to feel their mental health is not as healthy as it must be. 
    #      
    #      "
    #          
    #          
    #     )
    # })
    
   #  output$plot_common_text <- renderUI({
   #      HTML('
   #  
   #  <b> <big><big><big> 2 of the most common mental health issues in Australia </big></big></big></b> <br>
   #  
   #  <b> <big><big>Depression:</big></big></b><br>
   #  Depression is classified as a mood disorder. It may be described as feelings of 
   #  sadness, loss, or anger that interfere with a person\'s everyday activities.
   #  People experience depression in different ways. It may interfere with your daily work, resulting in lost time and lower productivity. 
   #  It can also influence relationships and some chronic health conditions.
   #  
   #  Conditions that can get worse due to depression include:<br>
   #  <ul>
   #    <li> arthritis </li>
   #    <li> asthma </li>
   #    <li> cardiovascular disease </li>
   #    <li> cancer </li>
   #    <li> diabetes </li>
   #    <li> obesity </li>
   #  </ul>
   #  
   #  &ensp; &ensp;<b><big>Symptoms:</big></b> <br>
   #  
   #  &ensp; &ensp;Depression can be more than a constant state of sadness or feeling \"blue.\" <br> &ensp; &ensp;Major depression can cause a variety of symptoms. 
   #  Some affect your mood, and others affect your body. 
   #  Symptoms may also <br> &ensp; &ensp;be ongoing, or come and go. 
   #  The symptoms of depression can be experienced differently among men, women, and children <br>&ensp; &ensp;differently.<br><br>
   #  &ensp; &ensp;Click <a href="https://www.healthline.com/health/depression#symptoms">here</a> for different types of depression symptoms. <br><br>
   #  
   #  &ensp; &ensp;<b><big>Causes:</big></b> <br>
   #  &ensp; &ensp; There are several possible causes of depression. They can range from biological to circumstantial.<br>
   #  &ensp; &ensp; Common causes include:<br>
   #          <ul>
   #            <li> Family history </li>
   #            <li> Early childhood trauma </li>
   #            <li> Brain structure </li>
   #            <li> Medical conditions </li>
   #            <li> Drug use </li>
   #            
   #          </ul>
   #  &ensp; &ensp;&ensp; &ensp; For more details on the causes, click <a href="https://www.healthline.com/health/depression#causes">here</a><br><br>
   #  
   #  <b> <big><big>Schizophrenia:</big></big></b><br>
   #  Schizophrenia is a chronic psychiatric disorder. 
   #  People with this disorder experience distortions of reality, often experiencing delusions or hallucinations.<br>
   #  Misconceptions about this disorder are common. 
   #  For example, some people think it creates a "split personality." 
   #  In fact, schizophrenia and split personality - properly termed dissociative identity disorder - are two different disorders.
   #  
   #  
   #  
   #  <br><br>&ensp; &ensp;<b><big>Symptoms:</big></b> <br>
   #  
   #  &ensp; &ensp;Symptoms may include the following: <br>
   # &ensp; &ensp;  <b> 1. Early symptoms</b><br>
   #  
   #        <ul>
   #            <li> isolating oneself from friends and family </li>
   #            <li> changing friends or social groups </li>
   #            <li> a change in focus and concentration </li>
   #            <li> sleep problems </li>
   #            <li> irritability and agitation </li>
   #            <li> difficulties with schoolwork, or poor academic performance </li>
   #            
   #          </ul>
   #  
   #  &ensp; &ensp;  <b> 2. Positive symptoms</b><br>
   #  &ensp; &ensp; \"Positive\" symptoms of schizophrenia are behaviors that aren\'t typical in otherwise healthy individuals. <br>
   #  &ensp; &ensp; These behaviors include:<br>
   #  
   #  
   #        <ul>
   #            <li> Hallucinations. </li>
   #            <li> Delusions. </li>
   #            <li> Thought disorders. </li>
   #            <li> sleep problems </li>
   #            <li> Movement disorders. </li>
   #    
   # 
   #          </ul>
   #  
   #  &ensp; &ensp;  <b>3. Negative symptoms</b><br>
   #  &ensp; &ensp; Negative symptoms of schizophrenia interrupt a person\'s typical emotions, behaviors, and abilities. <br>
   #  &ensp; &ensp; These symptoms include:
   #  
   #  <ul>
   #            <li> disorganized thinking or speech </li>
   #            <li> trouble controlling impulses </li>
   #            <li> a lack of emotion or expressions</li>
   #            <li> sleep problems </li>
   #            <li> social isolation </li>
   #    
   # 
   #          </ul>
   #  
   #  &ensp; &ensp; For more details on the symptoms, click <a href="https://www.healthline.com/health/schizophrenia#symptoms">here</a><br><br>
   #  
   #  
   #  &ensp; &ensp;<b><big>Causes:</big></b> <br>
   #  &ensp; &ensp; The exact cause of schizophrenia is unknown. <br>
   #  &ensp; &ensp; Medical researchers believe several factors can contribute, including:.<br>
   #  
   #  &ensp; &ensp;&ensp; &ensp; - biological <br>
   #  &ensp; &ensp;&ensp; &ensp; - genetic <br>
   #  &ensp; &ensp;&ensp; &ensp; - environmental <br>
   # 
   #  
   #  &ensp; &ensp;&ensp; &ensp; For more details on the causes, click <a href="https://www.healthline.com/health/schizophrenia#causes">here</a><br><br>
   #  
   #  <br><br><b><big>For more details on </big></b><br>
   #  &ensp; &ensp; &ensp;1. Schizophrenia, click <a href="https://www.healthdirect.gov.au/schizophrenia">here</a> <br>
   #  &ensp; &ensp; &ensp;2. Depression, click <a href="https://www.healthdirect.gov.au/depression">here</a> <br>
   #  &ensp; &ensp; &ensp;3. Stress, click <a href="https://www.healthdirect.gov.au/stress">here</a> <br>
   #  &ensp; &ensp; &ensp;4. Personality disorder, click <a href="https://www.healthdirect.gov.au/personality-related-disorders">here</a> <br>
   #  &ensp; &ensp; &ensp;5. Bipolar disorder, click <a href="https://www.healthdirect.gov.au/bipolar-disorder">here</a> <br>
   #  &ensp; &ensp; &ensp;6. Schizoaffective, click <a href="https://www.healthdirect.gov.au/schizophrenia">here</a> <br>
   #  &ensp; &ensp; &ensp;7. Other anxieties, click <a href="https://www.healthdirect.gov.au/anxiety">here</a> <br>
   #  &ensp; &ensp; &ensp;8. Schizotypal, click <a href="https://www.healthdirect.gov.au/schizophrenia">here</a> <br>
   #  &ensp; &ensp; &ensp;9. Alcohol influenced, click <a href="https://adf.org.au/reducing-risk/aod-mental-health/aod-mental-health-resources/">here</a> <br>
   #  &ensp; &ensp; &ensp;10. Recurrent depression, click <a href="https://www.medicalnewstoday.com/articles/320269">here</a> <br><br><br>
   #  
   #  <b><big>Further details on </big></b><br> 
   #  &ensp; &ensp; &ensp;1. Public hospitals, click <a href="https://www1.health.gov.au/internet/main/publishing.nsf/Content/hospitals2.html">here</a>.  <br>   
   #  &ensp; &ensp; &ensp;2. Public psychiatric hospitals, click <a href="https://www.betterhealth.vic.gov.au/health/servicesandsupport/hospitals-clinics-and-residential-options-for-mental-illness">here</a>  <br> 
   #  &ensp; &ensp; &ensp;3. Public private hospitals, click <a href="https://www.healthdirect.gov.au/understanding-the-public-and-private-hospital-systems">here</a> <br>    
   #       
   #       
   #       '
   #           
   #           
   #      )
   #  })
    
    
}

# Run the application
shinyApp(ui = ui, server = server)


