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
                withSpinner(plotlyOutput("alcohol_cons_bar_plot"))
                
                ),
              box(
                status = "warning", solidHeader = TRUE, collapsible = TRUE,
                h3(strong("A lot of people don’t think alcohol is a drug – but it’s the most widely used and easily accessible drug in Australia.")),
                p("Alcohol is the most commonly used legal drug in Australia. It’s a depressant which means that it slows down the brain."),
                p("The consumption of alcohol in Australia is increasing year by year as evident in the plot shown."),
                h4(strong("What is the connection between alcohol and mental health?")),
                p("Alcohol can have a major impact on mental health. Because alcohol is a depressant, it slows your body down and changes the chemical makeup in your brain."),
                p("This has many effects. It can alter:"),
                tags$ul(tags$li("mood"),tags$li("energy levels"), tags$li("sleeping patterns"), tags$li("concentration"), tags$li("and many other things")),
                p("Alcohol also reduces inhibitions and impacts decision making,
                  which can lead to us making decisions whilst drinking that
                  we would not normally make sober. Like:"),
                tags$ul(tags$li("increases in risky behaviour"),tags$li("increases in aggression"), tags$li("self harm and suicide in people who may already be going
through a tough time.")),
                
                h4(strong("What happens if I stop drinking?")),
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
                    choices = sub('X', '', colnames(benzo_presc[,3:10])),
                    selected = "Opioids"
                  ),
                  box(width = 12, id = "benzo_box",
                    h4(strong("What are Benzodiazepines?")),
                    p("Benzodiazepines are medications that lower the activity of the nerves in the brain and cause you to be drowsy. 
                    They can be used to treat problems such as general anxiety disorder, panic attacks, difficulty sleeping, alcohol withdrawal, and seizures."),
                    h4(strong("How do they work?")),
                    p("Benzodiazepines act on the brain and central nervous system by increasing the calming effect of the brain's naturally-occurring chemical messengers. The sedative and calming effects of benzodiazepines will relieve the symptoms of anxiety and promote sleep for most people, with few side-effects in the short term."),
                    p(strong("Benzodiazepines treat the symptoms and not the cause of anxiety and insomnia. Once medication is stopped, symptoms may return if the underlying cause of the anxiety or insomnia has not been addressed.")),
                    h4(strong("Side effects")),
                    p(tags$ul(
                      tags$li("When used in older people for long-term use this can increase the risk of memory problems, drowsiness, falls, and motor vehicle accidents."),
                      tags$li("Severe side effects of this medication are trouble breathing, severe drowsiness, slowed heart rate, low blood pressure, and fainting."),
                      tags$li("Benzodiazepines should be used with caution when taking other medications that cause drowsiness, such as opioid pain medications, as this can lead to overdose, hospitalization, and possibly death.")
                    )),
                    h4(strong("What are some examples of Benzodiazepines?")),
                    p("The most popularly prescribed benzodiazepines in Australia are shown in the wordcloud, below are some of the examples"),
                    p(tags$ul(
                      tags$li("Valium (diazepam)"),
                      tags$li("Restoril (temazepam)"),
                      tags$li("Xanax (alprazolam)")
                    ))
                  )
                ),
                box(
                  status = "warning", solidHeader = TRUE,
                  collapsible = TRUE,
                  withSpinner(plotOutput("popular_drug_wordcloud")),
                  
                  box(width = 12, id = "opioid_box",
                      h4(strong("What are Opioids?")),
                      p("Opioids are a broad group of pain-relieving drugs that work by interacting with opioid receptors in your cells."),
                      h4(strong("How do they work?")),
                      p("When opioid medications travel through your blood and attach to opioid receptors in your brain cells, the cells release signals that muffle your perception of pain and boost your feelings of pleasure."),
                      p("What makes opioid medications effective for treating pain can also make them dangerous. At lower doses, opioids may make you feel sleepy, but higher doses can slow your breathing and heart rate, which can lead to death. And the feelings of pleasure that result from taking an opioid can make you want to continue experiencing those feelings, which may lead to addiction."),
                      h4(strong("Side effects")),
                      p("Generally, people who use opioids may experience the following:"),
                      p(tags$ul(
                        tags$li("extreme relaxation"),
                        tags$li("drowsiness and clumsiness"),
                        tags$li("confusion, slurred speech,"),
                        tags$li("slow breathing and heartbeat.")
                      )),
                      p("Long-term effects include:"),
                      p(tags$ul(
                        tags$li("increased tolerance"),
                        tags$li("constipation"),
                        tags$li("dependence"),
                        tags$li("damage to vital organs such as the lungs, brain and heart."))
                      ),
                      h4(strong("What are some examples of Opioids?")),
                      p("The most popularly prescribed opioids in Australia are shown in the wordcloud, below are some of the examples"),
                      p(tags$ul(
                        tags$li("Endone or OxyContin (oxycodone)"),
                        tags$li("Panadeine, Panadeine Forte and Nurofen Plus (codeine)"),
                        tags$li("Subutex or Suboxone (buprenorphine)")
                      ))
                  )
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
                  
                  box(
                    width = 12,
                    h5(strong("Many people who are addicted to drugs are also diagnosed with other mental disorders and vice versa. Compared with the general population, people addicted to drugs are roughly twice as likely to suffer from mood and anxiety disorders, with the reverse also true.")),
                    h4(strong("How does use of illicit drugs affects mental health?")),
                    p("There are three main types of drugs – depressants, stimulants and hallucinogens. They all cause your mind and body to react in different ways."),
                    p(tags$ul(
                      tags$li("Depressants slow your body down; your breathing and heart rate can slow down, you can experience nausea and vomiting, and your ability to think and react to what is happening around you can be affected. Alcohol, heroin, cannabis, sedatives and inhalants are all depressants."),
                      tags$li("Cannabis can cause depression, acute panic attacks or ongoing anxiety and paranoia, even in people who have never previously shown signs of having a mental health condition."),
                      tags$li("Stimulants speed your body up. They increase your heart rate, body temperature and blood pressure. People using stimulants can feel an increase in confidence, motivation and energy, and a decrease in the need for sleep.")
                    )),
                    h4(strong("How easily are these drugs available?")),
                    p("As evident by the plot, each kind of drug as per opinions of the users it is becoming more and more easy to get access to these drugs and thus more and more people are using them."),
                    h4(strong("Why quit?")),
                    tags$ul(
                      tags$li("Low self-esteem and drug addiction are often linked together and is considered a major reason that addict begins to use in the first place. Addicts and alcoholics abuse substances in an attempt to overcome negative thoughts and feelings—only to get stuck in the vicious cycle of addiction—and then experiencing lower and lower self-esteem."),
                      tags$li("When you introduce large “addict-like” amounts of drugs and alcohol into your system over an extended period of time the effects on your physical appearance can be devastating. Prolonged substance abuse causes liver damage, vitamin deficiencies, lack of quality sleep, and a reduction of your body's ability to resist and combat infections."),
                      tags$li("Numerous studies have shown a link between substance abuse and memory loss—particularly with long-term use. Many drugs, including alcohol, cause two immediate types of memory loss: brownouts, where you temporarily forget events that happened while drinking and using, and blackouts, where there is absolutely no recollection of anything that transpired."),
                      tags$li("When you enter recovery and stops self-medicating you can begin to address any mental health issues with a professional that can, if needed, prescribe the proper medication. Therapy, counseling, and lifestyle coaching can all help as well. But the bottom line is that your depression and anxiety can be treated without the negative consequences of abusing substances.")
                      ) 
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
    )
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
