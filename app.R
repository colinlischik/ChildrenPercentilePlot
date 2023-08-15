library(shiny)
library(ggplot2)
library(readxl)
library(data.table)
library(plotly)
library(lubridate)
# Install and load the RColorBrewer package
if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
  install.packages("RColorBrewer")
}
library(RColorBrewer)

# Define UI
ui <- fluidPage(
  titlePanel("Child Growth Data"),
  sidebarLayout(
    sidebarPanel(
      fileInput("dataFile", "Upload Raw Data (xlsx)"),
      selectInput("dateColumn", "Select Date Column:", NULL),
      selectInput("weightColumn", "Select Weight Column:", NULL),
      selectInput("heightColumn", "Select Height Column:", NULL),
      selectInput("headColumn", "Select Head Circumference Column:", NULL),
      selectInput("percentileSelect", "Select Percentile:", choices = c("DE - Robert-Koch-Institut"), selected = "DE - Robert-Koch-Institut"),
      actionButton("plotButton", "Plot")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Weight", plotlyOutput("plot_weight", height = "100%")),
        tabPanel("Head Circumference", plotlyOutput("plot_head", height = "100%")),
        tabPanel("Height", plotlyOutput("plot_height", height = "100%"))
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  observeEvent(input$dataFile, {
    req(input$dataFile)
    data <- read_excel(input$dataFile$datapath)
    column_names <- colnames(data)
    updateSelectInput(session, "dateColumn", choices = column_names)
    updateSelectInput(session, "weightColumn", choices = column_names, selected = column_names[2])
    updateSelectInput(session, "heightColumn", choices = column_names, selected = column_names[3])
    updateSelectInput(session, "headColumn", choices = column_names, selected = column_names[4])
  })
  
  observeEvent(input$plotButton, {
    req(input$dataFile, input$dateColumn, input$headColumn, input$weightColumn, input$heightColumn)
    
    # Read raw data
    rawdata <- read_excel(input$dataFile$datapath)
    rawdata$Date <- as.Date(rawdata[[input$dateColumn]])
    rawdata$`head circumference` <- as.numeric(rawdata[[input$headColumn]])
    rawdata$weight <- as.numeric(rawdata[[input$weightColumn]])
    rawdata$height <- as.numeric(rawdata[[input$heightColumn]])
    
    # set parameters from meta data
    metadata <- read_excel(input$dataFile$datapath, sheet = "meta data")
    childname = metadata$value[metadata$key == "name"]
    birthdate <- as.Date(as.numeric(metadata$value[metadata$key == "birthdate"]), origin = "1899-12-30")
    sex = metadata$value[metadata$key == "sex"]
    
    # load percentiles
    if (sex == "F" && identical(input$percentileSelect, "DE - Robert-Koch-Institut")) {
      file_path <- "percentiles/DE - Robert-Koch-Institut - girls.xlsx"
    } else if (sex == "M" && identical(input$percentileSelect, "DE - Robert-Koch-Institut")) {
      file_path <- "percentiles/DE - Robert-Koch-Institut - boys.xlsx"
    }
    percentile_head = read_excel(file_path, sheet = "percentile head circumference")
    percentile_weight = read_excel(file_path, sheet = "percentile weight")
    percentile_height = read_excel(file_path, sheet = "percentile height")
    
    # prepare raw data
    rawdata$age = time_length(interval(birthdate, rawdata$Datum), "years")
    
    # prepare weight raw data
    percentile_weight$date = birthdate + months(percentile_weight$age_months)
    percentile_weight = percentile_weight[, -which(names(percentile_weight) %in% c("age", "Alter", "age_months"))]
    percentile_weight <- melt(setDT(percentile_weight), id.vars = c("date"), variable.name = "type")
    
    rawdata_weight = data.frame(value = rawdata$weight, date = rawdata$Date, type = rep("value", length(rawdata$Date)))
    rawdata_weight = rawdata_weight[!is.na(rawdata_weight$value),]
    rawdata_weight$value = rawdata_weight$value/1000
    rawdata_weight = rbind(rawdata_weight, percentile_weight)
    rawdata_weight$type = factor(rawdata_weight$type, levels = c("value", "P3", "P10", "P25", "P50", "P75", "P90", "P97"))
    rawdata_weight$date = as.Date(rawdata_weight$date)
    
    # prepare head raw data
    percentile_head$date = birthdate + months(percentile_head$age_months)
    percentile_head = percentile_head[, -which(names(percentile_head) %in% c("age", "Alter", "age_months"))]
    percentile_head <- melt(setDT(percentile_head), id.vars = c("date"), variable.name = "type")
    
    rawdata_head = data.frame(value = rawdata$`head circumference`, date = rawdata$Date, type = rep("value", length(rawdata$Date)))
    rawdata_head = rawdata_head[!is.na(rawdata_head$value),]
    rawdata_head = rbind(rawdata_head, percentile_head)
    rawdata_head$type = factor(rawdata_head$type, levels = c("value", "P3", "P10", "P25", "P50", "P75", "P90", "P97"))
    rawdata_head$date = as.Date(rawdata_head$date)
    
    #prepare height raw data
    percentile_height$date = birthdate + months(percentile_height$age_months)
    percentile_height = percentile_height[, -which(names(percentile_height) %in% c("age", "Alter", "age_months"))]
    percentile_height <- melt(setDT(percentile_height), id.vars = c("date"), variable.name = "type")
    
    rawdata_height = data.frame(value = rawdata$height, date = rawdata$Date, type = rep("value", length(rawdata$Date)))
    rawdata_height = rawdata_height[!is.na(rawdata_height$value),]
    rawdata_height = rbind(rawdata_height, percentile_height)
    rawdata_height$type = factor(rawdata_height$type, levels = c("value", "P3", "P10", "P25", "P50", "P75", "P90", "P97"))
    rawdata_height$date = as.Date(rawdata_height$date)
    
    # Calculate the range of available data
    x_range_weight <- c(min(rawdata_weight$date[rawdata_weight$type == "value"], na.rm = TRUE) - 1, max(rawdata_weight$date[rawdata_weight$type == "value"], na.rm = TRUE) + 1)
    y_range_weight <- c(min(rawdata_weight$value[rawdata_weight$type == "value"], na.rm = TRUE) - 0.100, max(rawdata_weight$value[rawdata_weight$type == "value"], na.rm = TRUE) + 0.100)
    
    plotly_weight = plot_ly() %>%
      add_trace(
        data = rawdata_weight[rawdata_weight$type == "value",],
        x = ~date,
        y = ~value,
        type = "scatter",
        mode = "lines+markers",
        marker = list(color = "black"),
        line = list(color = "black"),
        name = "value"
      ) %>%
      add_trace(
        data = rawdata_weight[rawdata_weight$type != "value",],
        x = ~date,
        y = ~value,
        type = "scatter",
        mode = "lines",
        line = list(color = ~type),
        name = ~type
      ) %>%
      layout(
        title = paste("weight of", childname),
        xaxis = list(
          title = "date",
          range = x_range_weight
        ),
        yaxis = list(
          title = "weight [kg]",
          range = y_range_weight
        ),
        colorway = brewer.pal(8, "YlOrRd"),
        showlegend = TRUE
      )
    
    output$plot_weight <- renderPlotly({
      plotly_weight
    })
    
    # plot the head circumference
    # Calculate the range of available data
    x_range_head <- c(min(rawdata_head$date[rawdata_head$type == "value"], na.rm = TRUE) - 1, max(rawdata_head$date[rawdata_head$type == "value"], na.rm = TRUE) + 1)
    y_range_head <- c(min(rawdata_head$value[rawdata_head$type == "value"], na.rm = TRUE) - 1, max(rawdata_head$value[rawdata_head$type == "value"], na.rm = TRUE) + 1)
    
    plotly_head = plot_ly() %>%
      add_trace(
        data = rawdata_head[rawdata_head$type == "value",],
        x = ~date,
        y = ~value,
        type = "scatter",
        mode = "lines+markers",
        marker = list(color = "black"),
        line = list(color = "black"),
        name = "value"
      ) %>%
      add_trace(
        data = rawdata_head[rawdata_head$type != "value",],
        x = ~date,
        y = ~value,
        type = "scatter",
        mode = "lines",
        line = list(color = ~type),
        name = ~type
      ) %>%
      layout(
        title = paste("head circumference of", childname),
        xaxis = list(
          title = "date",
          range = x_range_head
        ),
        yaxis = list(
          title = "head circumference [cm]",
          range = y_range_head
        ),
        colorway = brewer.pal(8, "YlOrRd"),
        showlegend = TRUE
      )
    
    output$plot_head <- renderPlotly({
      plotly_head
    })
    
    # plot the height
    # Calculate the range of available data
    x_range_height <- c(min(rawdata_height$date[rawdata_height$type == "value"], na.rm = TRUE) - 1, max(rawdata_height$date[rawdata_height$type == "value"], na.rm = TRUE) + 1)
    y_range_height <- c(min(rawdata_height$value[rawdata_height$type == "value"], na.rm = TRUE) - 5, max(rawdata_height$value[rawdata_height$type == "value"], na.rm = TRUE) + 5)
    
    plotly_height = plot_ly() %>%
      add_trace(
        data = rawdata_height[rawdata_height$type == "value",],
        x = ~date,
        y = ~value,
        type = "scatter",
        mode = "lines+markers",
        marker = list(color = "black"),
        line = list(color = "black"),
        name = "value"
      ) %>%
      add_trace(
        data = rawdata_height[rawdata_height$type != "value",],
        x = ~date,
        y = ~value,
        type = "scatter",
        mode = "lines",
        line = list(color = ~type),
        name = ~type
      ) %>%
      layout(
        title = paste("height of", childname),
        xaxis = list(
          title = "date",
          range = x_range_height
        ),
        yaxis = list(
          title = "height [cm]",
          range = y_range_height
        ),
        colorway = brewer.pal(8, "YlOrRd"),
        showlegend = TRUE
      )
    
    output$plot_height <- renderPlotly({
      plotly_height
    })
})
}

# Run the app
shinyApp(ui, server)