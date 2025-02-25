#---
#title: "hw2"
#output: html_document
#author: "Matej Popovski"
#date: "`r Sys.Date()`"
#---

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

# The first plot, a scatter plot with a regression line, allows for dynamic exploration 
# of the relationship between latitude and skin cancer rates using Shiny’s interactive queries. 
# By adjusting the latitude range dynamically, I was able to observe two distinct trends. 
# When selecting a latitude range between -40 to 0 degrees (Southern Hemisphere closer to 
# the equator), the regression line decreased, suggesting a lower incidence of skin cancer. 
# However, when adjusting the latitude range to 0 to 70 degrees (Northern Hemisphere moving 
# away from the equator), the regression line started increasing, revealing a higher number 
# of reported cases. This directly supports the argument that UV exposure alone is not the main 
# driving factor of skin cancer, as populations living in equatorial regions—where sunlight is 
# strongest—do not exhibit the highest rates. Instead, genetics, healthcare accessibility, and 
# lifestyle choices appear to have a much greater influence on skin cancer prevalence.

# The second visualization, a world map with interactive bubbles, provides a geospatial 
# perspective on skin cancer incidence across different countries. Each country is represented 
# by a bubble, where the size corresponds to the number of skin cancer cases per 100,000 
# people per year. By leveraging Shiny’s dynamic interactivity, users can hover over any 
# country to view precise cancer rates, allowing for an intuitive exploration of regional 
# differences. The map reveals a striking pattern—countries at higher latitudes, such as 
# Australia, New Zealand, Canada, and Northern Europe, have the largest bubbles, indicating 
# the highest incidence rates. Meanwhile, equatorial regions display significantly smaller 
# bubbles, further reinforcing the unexpected conclusion that higher UV exposure does not 
# necessarily correlate with more skin cancer cases. Instead, factors like genetic susceptibility, 
# healthcare infrastructure, and early detection efforts play a crucial role in the reported 
# incidence rates, emphasizing that sun exposure alone is not the dominant cause of skin cancer 
# on a global scale.


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

# Understanding the Sorted Table: A Tool for Comparative Analysis
# The sorted table provides a structured and numerical overview of the selected data, 
# making it an essential component for comparing skin cancer incidence across different 
# countries. Unlike the scatter plot and world map, which offer visual trends and 
# geographical insights, the table presents a precise, ranked list of countries based 
# on their reported cases of Malignant Skin Melanoma and Non-Melanoma Skin Cancer. 
# This allows users to quickly identify the countries with the highest or lowest cancer 
# rates, aiding in deeper analysis of regional patterns. The ability to filter by year 
# and latitude range makes it even more valuable for understanding temporal trends and 
# geographic disparities. Additionally, by rounding all values to whole numbers, the 
# table ensures clarity and readability, making it a powerful tool for data-driven 
# decision-making and research.

# Conclusion
# First and foremost, this analysis operates under the assumption that all countries 
# accurately report their skin cancer cases, ensuring that the data used is reliable 
# and comparable across regions. Based on the findings, the results challenge the 
# conventional belief that greater sun exposure leads to higher skin cancer rates. 
# In reality, the data shows that countries near the equator, where people are exposed 
# to intense sunlight year-round, report significantly fewer skin cancer cases compared 
# to those in higher latitudes. Interestingly, continents with highly developed 
# cosmetic and pharmaceutical industries—such as North America, Europe, and 
# Australia—exhibit the highest skin cancer rates. This raises an important question: 
# Could lifestyle factors, dietary habits, and the use of skincare and cosmetic 
# products contribute to skin cancer more than previously thought? While UV radiation 
# remains a known risk factor, this analysis suggests that modern living conditions, 
# healthcare accessibility, and possibly even chemical exposure from cosmetic products 
# and processed foods may play a much larger role in skin cancer prevalence than we 
# currently assume. This warrants further investigation into how our daily choices and 
# environment influence long-term health outcomes.

