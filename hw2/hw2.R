---
  title: "hw2"
output: html_document
author: "Matej Popovski"
date: "`r Sys.Date()`"
---

# Exploring the Paradox: Skin Cancer and Geographic Latitude
  
# The aim of this analysis is to investigate the correlation between geographic 
# latitude and skin cancer incidence across the world. Given that UV radiation is 
# strongest near the equator, the expectation is that countries closer to the equator 
# would exhibit higher rates of skin cancer. However, the data tells a different and 
# counterintuitive story—skin cancer rates are significantly higher in northern and 
# southern latitudes, while equatorial regions report lower incidence rates.
  
# Using Shiny, I built a dynamic visualization tool that allows users to filter 
# cancer rates by year, latitude, and skin cancer type. This interactive approach 
# helps us uncover surprising trends and explore possible explanations for this 
# unexpected pattern.
  
  
  

# Install required packages if not installed
list.of.packages <- c("shiny", "ggplot2", "dplyr", "tidyverse", "leaflet", "readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(leaflet)
library(readr)

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
        tabPanel("Map View", leafletOutput("mapPlot"))
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
        weight = 1, radius = ~sqrt(df[[input$cancer_type[1]]]) * 5000,
        popup = ~paste(
          "<b>", Country, "</b><br>",
          input$cancer_type[1], ": ", round(df[[input$cancer_type[1]]])
        )
      )
  })
}  # <-- Ensure this bracket is properly placed to close the server function

# Run the application 
shinyApp(ui = ui, server = server)  # Ensure this is at the very end
