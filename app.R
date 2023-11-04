library(shiny)
library(ggplot2)
library(readxl)
library(data.table)
library(plotly)
library(lubridate)
library(RColorBrewer)
library(shinythemes)
library(fontawesome)
library(rlang)

# Define UI
ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Child Growth Data"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("comparisonType", "Select Comparison Type:",
                   choices = c("Single child with percentiles", "2 Children comparison"),
                   selected = "Single child with percentiles"
      ),
      fileInput("dataFile", "Upload Raw Data (xlsx)"),
      selectInput("dateColumn", "Select Date Column:", NULL),
      selectInput("weightColumn", "Select Weight Column:", NULL),
      selectInput("heightColumn", "Select Height Column:", NULL),
      selectInput("headColumn", "Select Head Circumference Column:", NULL),
      conditionalPanel(
        condition = "input.comparisonType == 'Single child with percentiles'",
        selectInput("percentileSelect", "Select Percentile:", choices = c("DE - Robert-Koch-Institut"), selected = "DE - Robert-Koch-Institut")
      ),
      conditionalPanel(
        condition = "input.comparisonType == '2 Children comparison'",
        fileInput("dataFile2", "Upload Second Raw Data (xlsx)"),
        selectInput("dateColumn2", "Select Date Column:", NULL),
        selectInput("weightColumn2", "Select Weight Column:", NULL),
        selectInput("heightColumn2", "Select Height Column:", NULL),
        selectInput("headColumn2", "Select Head Circumference Column:", NULL)
      ),
      actionButton("plotButton", "Plot")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Weight", plotlyOutput("plot_weight"), style = "height: 100vh; overflow-y: scroll;"),
        tabPanel("Head Circumference", plotlyOutput("plot_head"), style = "height: 100vh; overflow-y: scroll;"),
        tabPanel("Height", plotlyOutput("plot_height"), style = "height: 100vh; overflow-y: scroll;")
      ),
      div(
        style = "position: fixed; bottom: 0; width: 100%; padding: 10px;",
        div(
          "Created by Colin Lischik ", style = "margin-right: 20px; font-weight: bold;",
          tags$a(href = "https://www.lischik.eu", "(lischik.eu)")
        ),
        tags$a(href = "https://github.com/colinlischik/ChildrenPercentilePlot", "GitHub Repository (incl. template and how-to)", style = "margin-right: 20px;"),
        tags$a(href = "https://github.com/colinlischik/ChildrenPercentilePlot/blob/main/test%20data/template.xlsx", "raw data template", style = "margin-right: 20px;")
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
  
  observeEvent(input$dataFile2, {
    req(input$dataFile2)
    data <- read_excel(input$dataFile2$datapath)
    column_names <- colnames(data)
    updateSelectInput(session, "dateColumn2", choices = column_names)
    updateSelectInput(session, "weightColumn2", choices = column_names, selected = column_names[2])
    updateSelectInput(session, "heightColumn2", choices = column_names, selected = column_names[3])
    updateSelectInput(session, "headColumn2", choices = column_names, selected = column_names[4])
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
    
    plotly_weight <- plot_ly() %>%
      add_trace(
        data = rawdata_weight[rawdata_weight$type == "value",],
        x = ~date,
        y = ~value,
        type = "scatter",
        mode = "lines+markers",
        marker = list(color = "white"),  # Adjust marker color for visibility
        line = list(color = "white"),    # Adjust line color for visibility
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
        title = paste("Weight of", childname),
        xaxis = list(
          title = "Date",
          range = x_range_weight,
          gridcolor = "rgba(100, 100, 100, 0.5)"
        ),
        yaxis = list(
          title = "Weight [kg]",
          range = y_range_weight,
          gridcolor = "rgba(100, 100, 100, 0.5)"
        ),
        paper_bgcolor = "rgba(0,0,0,0)",   # Set background color to transparent
        plot_bgcolor = "rgba(0,0,0,0)",    # Set plot area color to transparent
        colorway = brewer.pal(8, "Spectral"),  # Use a different color palette
        showlegend = TRUE
      )
    
    output$plot_weight <- renderPlotly({
      plotly_weight
    })
    
    # plot the head circumference
    # Calculate the range of available data
    x_range_head <- c(min(rawdata_head$date[rawdata_head$type == "value"], na.rm = TRUE) - 1, max(rawdata_head$date[rawdata_head$type == "value"], na.rm = TRUE) + 1)
    y_range_head <- c(min(rawdata_head$value[rawdata_head$type == "value"], na.rm = TRUE) - 1, max(rawdata_head$value[rawdata_head$type == "value"], na.rm = TRUE) + 1)
    
    plotly_head <- plot_ly() %>%
      add_trace(
        data = rawdata_head[rawdata_head$type == "value",],
        x = ~date,
        y = ~value,
        type = "scatter",
        mode = "lines+markers",
        marker = list(color = "white"),  # Adjust marker color for visibility
        line = list(color = "white"),    # Adjust line color for visibility
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
        title = paste("Head Circumference of", childname),
        xaxis = list(
          title = "Date",
          range = x_range_head,
          gridcolor = "rgba(100, 100, 100, 0.5)"
        ),
        yaxis = list(
          title = "Head Circumference [cm]",
          range = y_range_head,
          gridcolor = "rgba(100, 100, 100, 0.5)"
        ),
        paper_bgcolor = "rgba(0,0,0,0)",   # Set background color to transparent
        plot_bgcolor = "rgba(0,0,0,0)",    # Set plot area color to transparent
        colorway = brewer.pal(8, "Spectral"),  # Use a different color palette
        showlegend = TRUE
      )
    
    output$plot_head <- renderPlotly({
      plotly_head
    })
    
    # plot the height
    # Calculate the range of available data
    x_range_height <- c(min(rawdata_height$date[rawdata_height$type == "value"], na.rm = TRUE) - 1, max(rawdata_height$date[rawdata_height$type == "value"], na.rm = TRUE) + 1)
    y_range_height <- c(min(rawdata_height$value[rawdata_height$type == "value"], na.rm = TRUE) - 5, max(rawdata_height$value[rawdata_height$type == "value"], na.rm = TRUE) + 5)
    
    plotly_height <- plot_ly() %>%
      add_trace(
        data = rawdata_height[rawdata_height$type == "value",],
        x = ~date,
        y = ~value,
        type = "scatter",
        mode = "lines+markers",
        marker = list(color = "white"),  # Adjust marker color for visibility
        line = list(color = "white"),    # Adjust line color for visibility
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
        title = paste("Height of", childname),
        xaxis = list(
          title = "Date",
          range = x_range_height,
          gridcolor = "rgba(100, 100, 100, 0.5)"
        ),
        yaxis = list(
          title = "Height [cm]",
          range = y_range_height,
          gridcolor = "rgba(100, 100, 100, 0.5)"
        ),
        paper_bgcolor = "rgba(0,0,0,0)",   # Set background color to transparent
        plot_bgcolor = "rgba(0,0,0,0)",    # Set plot area color to transparent
        colorway = brewer.pal(8, "Spectral"),  # Use a different color palette
        showlegend = TRUE
      )
    
    output$plot_height <- renderPlotly({
      plotly_height
    })
})
}

# Run the app
shinyApp(ui, server)