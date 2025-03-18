#---
#title: "hw3"
#output:
#author: "Matej Popovski"
#date: "16 Mar 2025"
#---

# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(lubridate)
library(maps)
library(RColorBrewer)
library(plotly)
library(stringr)

# Load dataset for time-series analysis
df <- read_csv("cbp_resp.csv")

# Ensure fiscal_year is numeric and filter out invalid rows
df <- df %>% 
  filter(!is.na(fiscal_year) & !is.na(encounter_count) & !is.na(month_abbv) & 
           !is.na(demographic) & !is.na(encounter_type)) %>%
  mutate(fiscal_year = as.numeric(fiscal_year))

# Define a mapping of month abbreviations to numeric months
month_order <- c("JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6,
                 "JUL" = 7, "AUG" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)

# Convert month abbreviations to numeric values & create date column
df <- df %>%
  mutate(month_num = month_order[month_abbv]) %>%
  mutate(date = as.Date(paste(fiscal_year, month_num, "01", sep = "-")))

# Filter data only for "Inadmissibles" encounter type
df <- df %>% filter(encounter_type == "Inadmissibles")

# Load dataset for the world map
immigrants <- read_csv("from_where_immigrants.csv")

# Rename columns for easier merging
colnames(immigrants) <- c("region", "total_immigrants")

# Exclude "Other" and USA
immigrants <- immigrants %>%
  filter(region != "Other" & region != "United States")

# Compute percentage contribution of each country
total_immigrants_all <- sum(immigrants$total_immigrants)
immigrants <- immigrants %>%
  mutate(percentage = (total_immigrants / total_immigrants_all) * 100)

# Load world map data
world_map <- map_data("world")
map_data_merged <- left_join(world_map, immigrants, by = "region")

# Ensure USA remains uncolored
map_data_merged$percentage[map_data_merged$region == "USA"] <- NA

# Set countries with 0% immigration to NA
map_data_merged$percentage[map_data_merged$percentage == 0] <- NA

# Define a custom color palette for the world map
color_palette <- rev(brewer.pal(10, "Spectral"))

# Load state-level immigration data for the U.S.
state_data <- read_csv("per_state.csv")

# Rename columns
colnames(state_data) <- c("state", "total_immigrants", "percentage")

# Convert 'percentage' column to numeric
state_data <- state_data %>%
  mutate(
    percentage = as.numeric(str_replace(percentage, "%", "")), 
    percentage = percentage / 100 
  )

# Load US map data
us_states <- map_data("state")
state_data$state <- tolower(state_data$state)

# Merge with state data
us_map <- left_join(us_states, state_data, by = c("region" = "state"))

# Define color palette for the U.S. map
us_color_palette <- colorRampPalette(c("white", "red"))(100)

# Define UI
ui <- fluidPage(
  titlePanel("U.S. Immigration Analysis (2021-2024)"),
  tabsetPanel(
    
    # Time-Series Analysis
    tabPanel("Immigration Trends",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_demo", "Choose a Demographic:", 
                             choices = c("All Demographics", unique(df$demographic)), 
                             selected = "All Demographics")
               ),
               mainPanel(
                 plotOutput("regressionPlot")
               )
             )
    ),
    
    # Global Immigration Map
    tabPanel("Global Immigration Map",
             sidebarLayout(
               sidebarPanel(
                 helpText("Hover over a country to see total immigrants and percentage share.")
               ),
               mainPanel(
                 plotlyOutput("immigrationMap", height = "700px")
               )
             )
    ),
    
    # U.S. State-Level Immigration Map
    tabPanel("U.S. Immigration Map",
             sidebarLayout(
               sidebarPanel(
                 helpText("Hover over a state to see immigration details.")
               ),
               mainPanel(
                 plotlyOutput("stateMap", height = "700px")
               )
             )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Time-Series Plot
  output$regressionPlot <- renderPlot({
    filtered_data <- reactive({
      if (input$selected_demo == "All Demographics") {
        df %>% count(date, wt = encounter_count, name = "total_encounters")
      } else {
        df %>% filter(demographic == input$selected_demo) %>%
          count(date, wt = encounter_count, name = "total_encounters")
      }
    })
    
    ggplot(filtered_data(), aes(x = date, y = total_encounters)) +
      geom_point(color = "#C10000", alpha = 0.5) +
      geom_smooth(method = "loess", color = "#0F4962", se = FALSE) +
      geom_smooth(method = "lm", formula = y ~ poly(x, 3), color = "purple", se = FALSE, linetype = "dashed") +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      scale_y_continuous(labels = comma_format()) +
      labs(title = paste("Inadmissibles Encounters for:", input$selected_demo),
           subtitle = "Per Month, Jan 2020 - Sep 2024",
           caption = "Source: U.S. Customs and Border Patrol") +
      theme_minimal()
  })
  
  # Global Immigration Map
  output$immigrationMap <- renderPlotly({
    p <- ggplot(map_data_merged, aes(x = long, y = lat, group = group, fill = percentage, text = paste(
      "Country: ", region, "<br>",
      "Total Immigrants: ", scales::comma(total_immigrants), "<br>",
      "Percentage: ", round(percentage, 2), "%"
    ))) +
      geom_polygon(color = "black") +
      scale_fill_gradientn(
        colors = color_palette,  
        name = "Immigrant Share (%)",
        breaks = seq(0, max(map_data_merged$percentage, na.rm = TRUE), length.out = 10),
        labels = function(x) paste0(round(x, 1), "%"),
        na.value = "white"
      ) +
      labs(title = "Global Immigration Percentage to the U.S.",
           subtitle = "Shading represents each country's share of total U.S. immigrants",
           caption = "Source: U.S. Customs and Border Patrol") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # U.S. Immigration Map
  output$stateMap <- renderPlotly({
    p <- ggplot(us_map, aes(x = long, y = lat, group = group, fill = percentage, text = paste(
      "State: ", region, "<br>",
      "Total Immigrants: ", scales::comma(total_immigrants), "<br>",
      "Percentage of Total: ", round(percentage * 100, 2), "%"
    ))) +
      geom_polygon(color = "black") +
      scale_fill_gradientn(
        colors = us_color_palette, 
        name = "Immigrant Share (%)",
        na.value = "gray90"
      ) +
      labs(title = "State-Level Immigration Intensity (2021-2024)",
           caption = "Source: U.S. Customs and Border Patrol") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

