#---
#title: "hw2"
#output: html_document
#author: "Matej Popovski"
#date: "`r Sys.Date()`"
#---

# Exploring the Paradox: Skin Cancer and Geographic Latitude
  
# Install required packages if not installed
list.of.packages <- c("shiny", "ggplot2", "dplyr", "tidyverse", "leaflet", "readr", "DT")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(leaflet)
library(readr)
library(DT)  # For interactive tables

# Load cancer dataset (modify path accordingly)
cancer_data <- read_csv("cancer.csv")

# Load latitude/longitude dataset
lat_long_data <- read_csv("longitude-latitude.csv")

# Ensure the country names match (standardize if necessary)
lat_long_data <- lat_long_data %>%
  select(Country, Latitude, Longitude)  # Keep only necessary columns

# Merge cancer data with latitude/longitude data
data <- left_join(cancer_data, lat_long_data, by = "Country")

# Define UI
ui <- fluidPage(
  titlePanel("Skin Cancer vs Geographic Latitude"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = unique(data$Year), selected = max(data$Year)),
      sliderInput("latitude_range", "Select Latitude Range:", 
                  min = min(data$Latitude, na.rm = TRUE), 
                  max = max(data$Latitude, na.rm = TRUE), 
                  value = c(min(data$Latitude, na.rm = TRUE), max(data$Latitude, na.rm = TRUE))),
      checkboxGroupInput("cancer_type", "Select Cancer Type:",
                         choices = c("Malignant skin melanoma", "Non-melanoma skin cancer"),
                         selected = "Malignant skin melanoma")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Scatter Plot", plotOutput("scatterPlot")),
        tabPanel("Map View", leafletOutput("mapPlot")),
        tabPanel("Sorted Table", DTOutput("sortedTable"))  # New tab for the table
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  filtered_data <- reactive({
    data %>%
      filter(Year == input$year, 
             Latitude >= input$latitude_range[1], 
             Latitude <= input$latitude_range[2]) %>%
      select(Country, Latitude, Longitude, all_of(input$cancer_type))
  })
  
  output$scatterPlot <- renderPlot({
    req(input$cancer_type)
    
    df <- filtered_data() %>%
      pivot_longer(cols = input$cancer_type, names_to = "Cancer_Type", values_to = "Rate")
    
    ggplot(df, aes(x = Latitude, y = Rate, color = Cancer_Type)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Skin Cancer Rates vs Latitude",
           x = "Latitude",
           y = "Cancer Rate (per 100,000 people)",
           color = "Cancer Type") +
      theme_minimal()
  })
  
  output$mapPlot <- renderLeaflet({
    df <- filtered_data()
    
    leaflet(df) %>%
      addTiles() %>%
      addCircles(
        lng = ~Longitude, lat = ~Latitude,
        weight = 1, 
        radius = ~sqrt(rowSums(df[input$cancer_type], na.rm = TRUE)) * 5000,  # Bubble size based on total cases
        popup = ~paste0(
          "<b>", Country, "</b><br>",  # Display country name in bold
          if ("Malignant skin melanoma" %in% input$cancer_type) 
            paste0("Malignant skin melanoma: ", round(df[["Malignant skin melanoma"]], 0), "<br>") else "",
          if ("Non-melanoma skin cancer" %in% input$cancer_type) 
            paste0("Non-melanoma skin cancer: ", round(df[["Non-melanoma skin cancer"]], 0))
        )
      )
  })
  
  
output$sortedTable <- renderDT({
    df <- filtered_data()
    
    # Round all numeric columns to whole numbers
    df[input$cancer_type] <- round(df[input$cancer_type], 0)
    
    # Sort the table by the first selected cancer type in descending order
    df_sorted <- df %>%
      arrange(desc(df[[input$cancer_type[1]]])) 
    
    datatable(df_sorted, options = list(pageLength = 10), rownames = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


# Note: on the world map view, there are a couple of country bubbles that cannot be clicked
# because they are covered by a bigger bubble, but that can be fixed by adjusting the lattitude.