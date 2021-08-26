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
library(tm)
library(wordcloud)
library(memoise)
library(plotly)
library(tibble)
library(tidyverse)
library(dplyr)
library(reshape2)

# Read the common diagnosis csv file
my_data = read.csv('Overnight-admitted-mental-health-related-care-tables-1819.csv', header = T, sep = ",", skip = 4)

# Read the rate of admittance csv file
rate_admit <- read.csv('Overnight-admitted-mental-health-related-care-tables-1819-(rate-of-admittance).csv', header = T, sep = ",", skip = 4)

# Perform the encoding for all the columns
for (i in c(1:18)){
  Encoding(my_data[,i]) <- "UTF-8"
  Encoding(rate_admit[,i]) <- "UTF-8"
}


# Replace '-' with 0 
my_data[my_data == '-'] <- "0"

# Replace '-' with 0
my_data[my_data == '-'] <- "0"

# Replace "n.p." with 0
my_data[my_data == 'n.p.'] <- "0"

# # Changing the column names
names(my_data)[4] <- "Public Hospitals"
names(my_data)[5] <- "Public Psychiatric Hospitals"
names(my_data)[6] <- "All Public Hospitals (Australia)"
names(my_data)[7] <- "Private Hospitals"
names(my_data)[8] <- "All Hospitals (Public + Private)"


# Cleaning up the names of mental health
my_data$Principal.diagnosis <- gsub("Other organic mental disorders", "Physiological", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Mental and behavioural disorders due to use of alcohol", "Alcohol influenced", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Mental and behavioural disorders due to other psychoactive substance use", "Substance use", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Schizotypal and other delusional disorders", "Schizotypal", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Persistent delusional disorders", "Persistent delusion", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Acute and transient psychotic disorders", "Acute/Trasient Psychosis", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Bipolar affective disorders", "Bipolar disorder", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Recurrent depressive disorders", "Recurrent depression", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Schizoaffective disorders", "Schizoaffective", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Persistent mood (affective) disorders", "Mood disorders", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Other and unspecified mood (affective) disorders", "Unspecified mood dis.", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Phobic anxiety disorders", "Phobic anxiety", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Other anxiety disorders", "Other anxieties", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Obsessive-compulsive disorders", "OCD", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Reaction to severe stress and adjustment disorders", "Stress", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Dissociative (conversion) disorders", "Dissociative disorder", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Somatoform and other neurotic disorders", "Somatoform/other neurotic disorder", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Other behavioural syndromes associated with physiological disturbances and physical factors", "Behavioural syndromes", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Disorders of adult personality and behaviour", "Personality and behaviour disorder", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Disorders of psychological development", "Psychological", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Hyperkinetic disorders", "Hyperkinesis", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Other and unspecified disorders with onset in childhood or adolescence", "Unspecified disorders", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Other factors related to mental and behavioural disorders and substance use", "Other substance use", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Alzheimer's disease", "Alzheimer's", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Other specified mental health-related principal diagnosis", "Other specific", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Alzheimer's disease", "Alzheimer's", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Other and unspecified mood (affective) disorders", "Other mood dis.", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Mental disorder not otherwise specified", "Unspecified Mental dis.", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Depressive episode", "Depression", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Specific personality disorders", "Personality dis.", my_data$Principal.diagnosis)


# Filter the necessary portion of the dataset
my_data <- my_data[1:(nrow(my_data)-7),]

# Check the structure of each column  
# str(my_data)


# Filter out the specialized care
filtered_data <- my_data[my_data$Separation.type=='With specialised psychiatric care',]

# Convert the char to numeric data
filtered_data[,4:18] <- lapply(filtered_data[, 4:18],function(x){as.numeric(gsub(",", "", x))})

# Check for column data type
# str(filtered_data)

# List of diagnosis
separations <- colnames(filtered_data[,4:8])

#################################################
#### Pre-processing for Rate of admittance ############
#################################################

# Cleaning the data (Age.group column)
rate_admit$Age.group <- gsub("\\?", "-", rate_admit$Age.group)

# Drop the rows with subtotal columns
rate_admit <- rate_admit[!(rate_admit$Age.group=="Subtotal" | rate_admit$Age.group==""),]

# Check the data type of columns
str(rate_admit)

# Change the data type from char to integer
rate_admit[,5:20] <- lapply(rate_admit[, 5:20],function(x){as.numeric(gsub(",", "", x))})

# Input 1 selection
sepraration_type <- unique(rate_admit$Separation.type)

# Input 2 selection
age_group <- unique(rate_admit$Age.group)

names(rate_admit)[5] <- "2007"
names(rate_admit)[6] <- "2008"
names(rate_admit)[7] <- "2009"
names(rate_admit)[8] <- "2010"
names(rate_admit)[9] <- "2011"
names(rate_admit)[10] <- "2012"
names(rate_admit)[11] <- "2013"
names(rate_admit)[12] <- "2014"
names(rate_admit)[13] <- "2015"
names(rate_admit)[14] <- "2016"
names(rate_admit)[15] <- "2017"
names(rate_admit)[16] <- "2018"
names(rate_admit)[17] <- "2019"

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
        tabItem(tabName = "hospital", tabBox(height = "1100px", width = "1000px",
          
            
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
                                   sepraration_type,
                                   selected = "With specialised psychiatric care"
                                 )
                               ),
                               
                               box(
                                 title = "Inputs", background = "black",  solidHeader = TRUE,
                                 collapsible = TRUE,
                                 selectInput(
                                   "age",
                                   "Select the age group",
                                   age_group,
                                   selected = "0-4 years"
                                 )
                               ),
                               
                             )
                       
                     )
                     
                    ),
            # Third tab 
            tabPanel("Professional help", h2("How well does professional help assist?"),
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
                                   "age",
                                   "Select separations",
                                   age_group,
                                   selected = "With specialised psychiatric care"
                                 )
                               )
                               
                             )
                             
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

        b <- filtered_data[, c("Principal.diagnosis", input$selection)]
        
        b <- b[1:35,]
        
        b <- b[order(-b[,2]),]
        
        b[is.na(b)] <- 0
        
        b

  })
  
  rate <- reactive({
    
    c <- rate_admit %>% filter(Age.group == input$age, Separation.type == 
                                 input$selection_type, 
                               Measure == "Separations")
    c <- c[, c(1:17)]
    
    c[,6] <- as.numeric(c[,6])
    
    c
    
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
  
  output$plot_rate <- renderPlotly({
    r <- rate()
    
    r <- spread(melt(r, id=c("Measure", "Sex", "Separation.type", "Age.group")), Sex, value)
    print(r)
    r$Female <- (r$Female/sum(r$Female))*100
    r$Male <- (r$Male/sum(r$Male))*100

    plot_ly(r, x = ~variable, y = ~Male, name = 'Male', type = 'scatter', mode='lines+markers') %>% 
      add_trace(y = ~Female, name= 'Female', type='scatter', model = 'lines+markers') # %>% 
      # add_trace(y = ~Persons, name = 'Persons', mode = 'lines+markers')
     
    
    
 
  })
}

# Run the application
shinyApp(ui = ui, server = server)



