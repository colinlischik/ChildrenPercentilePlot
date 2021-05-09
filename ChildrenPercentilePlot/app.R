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

# Define plot types
plottypes = c("Height", "Weight", "Head circumference")

# Define possible percentiles
percentiles = c("DE - Robert-Koch-Institut")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("rawdata", "Please upload the data for your child here (.csv, .tsv, .xls and .xlsx files are allowed)", accept = c(".csv", ".tsv", ".xlsx", ".xls")),
            textInput("name", "Name of the child"),
            dateInput("birthdate", "Date of birth"),
            radioButtons("plottype", "Which plot do you want to see?", plottypes),
            selectInput("percentileSource", "Which percentiles should be applied to the plot?", percentiles),
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
        title <- paste(input$name, input$birthdate)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
