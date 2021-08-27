library(shiny)
library(ggplot2)
library(shinydashboard)
library(shinycssloaders)
library(wordcloud)
library(memoise)
library(plotly)



#read the files
alcohol_consumption = read.csv("alcohol_consumption.csv")
benzo_presc = read.csv("benzodiazepines_prescription.csv")
drug_avail = read.csv("drug_availability.csv")
drug_use_st = read.csv("drug_use_state_data.csv")
drug_source = read.csv("drugs_source.csv")
missing_work_drugs = read.csv("missing_work_drugs.csv")
opioids_presc = read.csv("opioids_prescritpion.csv")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  # Header of the dashboard
  dashboardHeader(title = "YMseekA"), # Name of the project
  dashboardSidebar(sidebarMenu(
    # All the Dashboard menu items 
    menuItem("Drug Use and Mental Health", tabName = "druguse", icon =icon("dashboard"))
    
  )),
  
  dashboardBody(
    
    tabItems(
      #Drug tab
      tabItem(tabName = "druguse",
              fluidRow(
        
              box(
                title = "How does alcohol affect mental health?", status = "warning", solidHeader = TRUE,
                collapsible = TRUE,
                withSpinner(plotlyOutput("alcohol_cons_bar_plot"))),
              box(
                h4(strong("A lot of people don’t think alcohol is a drug – but it’s the most widely used and easily accessible drug in Australia.")),
                p("Alcohol is the most commonly used legal drug in Australia. It’s a depressant which means that it slows down the brain."),
                p("The consumption of alcohol in Australia is increasing year by year as evident in the plot shown."),
                h5(strong("What is the connection between alcohol and mental health?")),
                p("Alcohol can have a major impact on mental health. Because alcohol is a depressant, it slows your body down and changes the chemical makeup in your brain."),
                p("This has many effects. It can alter:"),
                tags$ul(tags$li("mood"),tags$li("energy levels"), tags$li("sleeping patterns"), tags$li("concentration"), tags$li("and many other things")),
                p("Alcohol also reduces inhibitions and impacts decision making,
                  which can lead to us making decisions whilst drinking that
                  we would not normally make sober. Like:"),
                tags$ul(tags$li("increases in risky behaviour"),tags$li("increases in aggression"), tags$li("self harm and suicide in people who may already be going
through a tough time.")),
                p(strong("What happens if I stop drinking?")),
                p("There are many benefits that can come from reducing or
cutting out alcohol use. These may include:"),
                tags$ul(tags$li("more energy"),tags$li("better sleep"), tags$li("saving money"), tags$li("better physical health"), tags$li("improved mood.")),
                p("Some of these benefits you might notice within a couple of
days, whereas others can have a bigger impact the longer
you reduce your use."),
                p("It can be tricky giving up drinking if you’ve been doing
it for a long time, because your body has to get used to
going without it. If you’re dependent on alcohol and you
suddenly stop drinking, you might get withdrawal symptoms
including sweating, feeling sick, anxiety, irritability, problems
sleeping, hallucinations, tremors (e.g. shaking hands) and
even seizures."),
                p("Because of this, it’s a good idea to have a chat to a general
practitioner (GP) to discuss the safest way of cutting down
on your drinking.")
                
              )
              
              ),
              fluidRow(
                box(
                  title = "How is the use of prescription drugs impacting mental health?", status = "warning", solidHeader = TRUE,
                  collapsible = TRUE,
                  selectInput(
                    "pd_select_drug_type",
                    "Select Drug Type:",
                    c("Benzodiazepines", "Opioids"),
                    selected = "Benzodiazepines"
                  ),
                  selectInput(
                    inputId = "pd_select_year",
                    label = "Select Year:",
                    choices = colnames(benzo_presc[,3:10]),
                    selected = "Opioids"
                  ),
                  p("Information coming soon")
                ),
                box(
                  status = "warning", solidHeader = TRUE,
                  collapsible = TRUE,
                  withSpinner(plotOutput("popular_drug_wordcloud"))
                )
              ),
              fluidRow(
                box(
                  title="How is the use of illicit drugs impacting the mental health?",
                  status = "warning", solidHeader = TRUE,
                  collapsible = TRUE,
                  selectInput(
                    inputId = "da_select_drug_type",
                    label = "Select Drug Type:",
                    choices = unique(drug_avail$DrugType),
                    selected = "Base"
                  ),
                  p("Information coming soon")
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
    )
  )
)
)



# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    
    
    if(input$pd_select_drug_type == "Benzodiazepines"){
      b <- benzo_presc[, c('Drug', input$pd_select_year)]
    }else{
      b <- opioids_presc[, c('Drug', input$pd_select_year)]
    }
    b <- b[order(-b[,2]),]
    
    b[is.na(b)] <- 0
    
    b
  })
  
  sixteen_drug_type_val <- reactive({
    categories = c('Very easy', 'Easy', 'Difficult', 'Very difficult')
    val = c(drug_avail$Very.easy[drug_avail$Year == '2016' & drug_avail$DrugType == input$da_select_drug_type],
               drug_avail$Easy[drug_avail$Year == '2016' & drug_avail$DrugType == input$da_select_drug_type],
               drug_avail$Difficult[drug_avail$Year == '2016' & drug_avail$DrugType == input$da_select_drug_type],
               drug_avail$Very.difficult[drug_avail$Year == '2016' & drug_avail$DrugType == input$da_select_drug_type])
    
    data_six = data.frame(categories, val)
    print(data_six)
    data_six
  })
  seventeen_drug_type_val <- reactive({
    categories = c('Very easy', 'Easy', 'Difficult', 'Very difficult')
    val = c(drug_avail$Very.easy[drug_avail$Year == '2017' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Easy[drug_avail$Year == '2017' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Difficult[drug_avail$Year == '2017' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Very.difficult[drug_avail$Year == '2017' & drug_avail$DrugType == input$da_select_drug_type])
    
    data_six = data.frame(categories, val)
    print(data_six)
    data_six
  })
  eighteen_drug_type_val <- reactive({
    categories = c('Very easy', 'Easy', 'Difficult', 'Very difficult')
    val = c(drug_avail$Very.easy[drug_avail$Year == '2018' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Easy[drug_avail$Year == '2018' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Difficult[drug_avail$Year == '2018' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Very.difficult[drug_avail$Year == '2018' & drug_avail$DrugType == input$da_select_drug_type])
    
    data_six = data.frame(categories, val)
    print(data_six)
    data_six
  })
  nineteen_drug_type_val <- reactive({
    categories = c('Very easy', 'Easy', 'Difficult', 'Very difficult')
    val = c(drug_avail$Very.easy[drug_avail$Year == '2019' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Easy[drug_avail$Year == '2019' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Difficult[drug_avail$Year == '2019' & drug_avail$DrugType == input$da_select_drug_type],
            drug_avail$Very.difficult[drug_avail$Year == '2019' & drug_avail$DrugType == input$da_select_drug_type])
    
    data_six = data.frame(categories, val)
    print(data_six)
    data_six
  })
  
  output$alcohol_cons_bar_plot <- renderPlotly({
    plot_ly(alcohol_consumption)  %>% add_trace(x = ~Year, y = ~Alcohol_Volume,type = 'bar') %>% layout(xaxis = list(title = "Year"), yaxis = list(title = "Alcohol Consumption ('000 lts)"))
    
  })
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$popular_drug_wordcloud <- renderPlot({
    v <- terms()
    wordcloud_rep(v[,1], v[,2], scale=c(10,0.25), use.r.layout = T, 
                  min.freq = 1, max.words=100,
                  colors=brewer.pal(4, "Dark2"))
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
