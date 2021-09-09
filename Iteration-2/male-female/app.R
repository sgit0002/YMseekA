#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reshape2)
library(tidyr)


# Loading the data
mf_df <- read.xlsx2("2019-aihw-suicide-and-self-harm-monitoring-nmd-suicide-icd-10-x60-x84-y87-0.xlsx", sheetName="Table NMD S3", startRow = 2, endRow = 53)

names(mf_df)
df <- spread(melt(mf_df, id=c("Sex", "Mechanism", "Measure")), Mechanism, value)
mf_df <- df[(df$Sex=='Males' | df$Sex=='Females' ), ]
mf_df <- mf_df[mf_df$Measure=='Number',]

mf_df[,3] <- gsub("X", "", mf_df$variable)

write.csv(mf_df, "history-suicide.csv")

unique(mf_df$variable)
# Perform the encoding for all the columns
for (i in c(1:5)){
    Encoding(mf_df[,i]) <- "UTF-8"
    
}

# Check for structure
str(mf_df)

# Impute missing values
# Replace '-' with 0 
mf_df[mf_df == 'n.p.'] <- "0"

# Convert all the characters to numbers
mf_df[,3:5] <- lapply(mf_df[, 3:5],function(x){as.numeric(gsub(",", "", x))})



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
