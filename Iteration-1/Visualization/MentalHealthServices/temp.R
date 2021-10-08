library(shiny)
library(ggplot2)
library(tidyverse)

theme_set(theme_minimal())

ui <- fluidPage(
  #Create input for domain (single) and variable (multiple)
  selectInput("domain", "Domain", choices = ""),
  selectInput("varSelection", "Choose 2 or 3 variables", multiple = T, choices = ""),
  
  #Set plot for output
  plotOutput("myPlot")
)

server <- function(input, output, session) {
  
  #Load your data
  df <- tibble(d = c(1,1,2,2),
               year = c(2015, 2016, 2015, 2016),
               v1 = c(3,5,4,10),
               v2 = c(7,11,13,18),
               v3 = c(1,2,3,4))
  
  #Update the domain input according to the data
  updateSelectInput(session, "domain", choices = sort(unique(df$d)))
  
  #Update the variable list (assumed all but d and year are variables of interest)
  updateSelectInput(session, "varSelection", 
                    choices = colnames(df %>% select(-d, -year)))
  
  #Load the chart function
  draw_chart <- function(df, listv, d){
    df2 <- df %>%
      gather("variable", "value", 3:5)
    df3 <- df2 %>%
      filter(variable %in% listv)
    
    df4 <- df3 %>%
      group_by(d, year) %>%
      summarise(value = mean(value)) %>%
      mutate(variable = "m")
    
    df5 <- bind_rows(df3, df4) 
    
    df5 <- df5 %>%
      mutate(year = as.character(year)) %>%
      mutate(year = as.Date(year, "%Y"))
    
    df5 <- df5 %>%
      mutate(year = lubridate::year(year))
    
    df5 <- df5 %>%
      filter(d == 1)
    # format(df5$year, "%Y")
    # Visualization
    print(df5)
    ggplot(df5, aes(x = year, y = value)) + 
      geom_line(aes(color = variable, linetype = variable)) + 
      scale_color_discrete() +
      scale_x_continuous(breaks = c(2015,2016))
  }
  
  #Render the plot
  output$myPlot = renderPlot({
    #Only render if there are 2 or 3 variables selected
    req(between(length(input$varSelection), 2, 3))
    draw_chart(df, input$varSelection, input$domain)
  })
  
}

shinyApp(ui, server)
