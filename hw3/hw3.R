#---
#title: "hw3"
#output:
#author: "Matej Popovski"
#date: "16 Mar 2025"
#---

# 1. Shiny and regression:

# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(lubridate)

# Load dataset
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

# **Filter data only for "Inadmissibles" encounter type**
df <- df %>% filter(encounter_type == "Inadmissibles")

# Create the Shiny app
ui <- fluidPage(
  
  # App title
  titlePanel("Immigrant Encounters (Inadmissibles) by Demographic"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel with dropdown input
    sidebarPanel(
      selectInput("selected_demo", "Choose a Demographic:", 
                  choices = unique(df$demographic), 
                  selected = unique(df$demographic)[1])
    ),
    
    # Main panel with plot
    mainPanel(
      plotOutput("regressionPlot")
    )
  )
)

server <- function(input, output) {
  
  # Reactive filtered data based on selected demographic
  filtered_data <- reactive({
    df %>%
      filter(demographic == input$selected_demo) %>%
      count(date, wt = encounter_count, name = "total_encounters")
  })
  
  # Render plot
  output$regressionPlot <- renderPlot({
    
    ggplot(filtered_data(), aes(x = date, y = total_encounters)) +
      # Red scatter points
      geom_point(color = "#C10000", alpha = 0.5) +
      
      # LOESS smooth trend line
      geom_smooth(method = "loess", color = "#0F4962", se = FALSE) +
      
      # Best-fit polynomial regression curve
      geom_smooth(method = "lm", formula = y ~ poly(x, 3), color = "purple", se = FALSE, linetype = "dashed") +
      
      # Format x-axis to show yearly breaks
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      
      # Format y-axis with comma separators
      scale_y_continuous(labels = comma_format()) +
      
      # Labels and title
      labs(
        x = NULL,
        y = NULL,
        title = paste("Inadmissibles Encounters for:", input$selected_demo),
        subtitle = "Per Month, Jan 2020 - Sep 2024",
        caption = "Source: U.S. Customs and Border Patrol"
      ) +
      
      # Custom theme styling
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        
        # Title formatting
        plot.title = element_text(size = 20, face = "bold"),
        
        # Horizontal grid lines
        panel.grid.major.y = element_line(color = "gray60", linewidth = 0.25),
        panel.grid.minor.y = element_line(color = "gray90", linewidth = 0.25),
        
        # X-axis text margin
        axis.text.x = element_text(margin = margin(t = -10))
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

### ----------



##2 World Map Shiny


# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(maps)
library(RColorBrewer)
library(plotly)

# Load the dataset
immigrants <- read_csv("from_where_immigrants.csv")

# Rename columns for easier merging
colnames(immigrants) <- c("region", "total_immigrants")

# Exclude "Other" as it's not a real country & Exclude USA
immigrants <- immigrants %>%
  filter(region != "Other" & region != "United States")

# Calculate total immigration count (for percentage calculation)
total_immigrants_all <- sum(immigrants$total_immigrants)

# Compute percentage contribution of each country
immigrants <- immigrants %>%
  mutate(percentage = (total_immigrants / total_immigrants_all) * 100)  # Convert to percentage

# Load world map data
world_map <- map_data("world")

# Merge immigration data with world map data
map_data_merged <- left_join(world_map, immigrants, by = "region")

# Ensure USA remains uncolored by setting its percentage to NA
map_data_merged$percentage[map_data_merged$region == "USA"] <- NA

# Set countries with 0% immigration to NA so they remain white
map_data_merged$percentage[map_data_merged$percentage == 0] <- NA

# Define a custom 10-color palette and reverse it (red = strongest, blue = weakest)
color_palette <- rev(brewer.pal(10, "Spectral"))

# Shiny UI
ui <- fluidPage(
  titlePanel("Interactive Global Immigration Map to the U.S. (Zoom & Hover Enabled)"),
  sidebarLayout(
    sidebarPanel(
      helpText("Hover over a country to see the total number of illegal immigrants and their percentage share. 
               Use scroll/drag to zoom in and out.")
    ),
    mainPanel(
      plotlyOutput("immigrationMap", height = "700px")
    )
  )
)

# Shiny Server
server <- function(input, output) {
  
  output$immigrationMap <- renderPlotly({
    
    # Create the ggplot map
    p <- ggplot(map_data_merged, aes(x = long, y = lat, group = group, fill = percentage, text = paste(
      "Country: ", region, "<br>",
      "Total Immigrants: ", scales::comma(total_immigrants), "<br>",
      "Percentage: ", round(percentage, 2), "%"
    ))) +
      geom_polygon(color = "black") +
      scale_fill_gradientn(
        colors = color_palette,  # Reversed so red is strongest
        name = "Immigrant Share (%)",
        breaks = seq(0, max(map_data_merged$percentage, na.rm = TRUE), length.out = 10),
        labels = function(x) paste0(round(x, 1), "%"),
        na.value = "white"  # Ensures USA and 0% immigration countries remain uncolored (white)
      ) +
      labs(title = "Global Immigration Percentage to the U.S.",
           subtitle = "Shading represents each country's share of total U.S. immigrants",
           x = "", y = "",
           caption = "Source: U.S. Customs and Border Patrol") +
      theme_minimal() +
      theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
    
    # Convert ggplot to interactive Plotly object and enable zooming
    ggplotly(p, tooltip = "text") %>%
      layout(
        geo = list(
          scope = "world",
          showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = "natural earth")  # Enables zoom & drag
        )
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)




##--- 3:


# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(maps)
library(RColorBrewer)
library(plotly)
library(stringr)  # For string manipulation

# Load state-level immigration data
state_data <- read_csv("per_state.csv")

# Rename columns for consistency
colnames(state_data) <- c("state", "total_immigrants", "percentage")

# Convert 'percentage' column: Remove '%' and convert to numeric
state_data <- state_data %>%
  mutate(
    percentage = as.numeric(str_replace(percentage, "%", "")),  # Remove % symbol
    percentage = percentage / 100  # Convert to decimal (e.g., 19.07% → 0.1907)
  )

# Ensure no NA or Inf values
state_data <- state_data %>%
  mutate(
    total_immigrants = ifelse(is.na(total_immigrants), 0, total_immigrants),
    percentage = ifelse(is.na(percentage) | is.infinite(percentage), 0, percentage)
  )

# Load US map data
us_states <- map_data("state")

# Convert state names to lowercase for merging
state_data$state <- tolower(state_data$state)

# Merge state-level data with map data
us_map <- left_join(us_states, state_data, by = c("region" = "state"))

# Define a 100-color palette (White = Highest, Red = Lowest)
color_palette <- colorRampPalette(c("white", "red"))(100)  # Opposite of before

# Shiny UI
ui <- fluidPage(
  titlePanel("U.S. Immigration Map (2021-2024)"),
  sidebarLayout(
    sidebarPanel(
      helpText("Hover over a state to see immigration details. 
               Darker red means fewer immigrants, white means the most.")
    ),
    mainPanel(
      plotlyOutput("stateMap", height = "700px")
    )
  )
)

# Shiny Server
server <- function(input, output) {
  
  # State-Level Map
  output$stateMap <- renderPlotly({
    
    p <- ggplot(us_map, aes(
      x = long, y = lat, group = group, fill = percentage,
      text = paste(
        "State: ", region, "<br>",
        "Total Immigrants: ", scales::comma(total_immigrants), "<br>",
        "Percentage of Total: ", round(percentage * 100, 2), "%"  # Convert back to % format for display
      )
    )) +
      geom_polygon(color = "black") +
      scale_fill_gradientn(
        colors = color_palette, 
        name = "Immigrant Share (%)",
        breaks = seq(min(us_map$percentage, na.rm = TRUE), 
                     max(us_map$percentage, na.rm = TRUE), length.out = 10),
        labels = function(x) paste0(round(x * 100, 2), "%"),
        na.value = "gray90"  # Keeps missing values in a light gray color
      ) +
      labs(title = "State-Level Immigration Intensity (2021-2024)",
           subtitle = "Shading represents total immigrants in each state",
           x = "", y = "",
           caption = "Source: U.S. Customs and Border Patrol") +
      theme_minimal() +
      theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        geo = list(
          scope = "usa",
          showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = "albers usa")
        )
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


## -----



# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(maps)
library(RColorBrewer)
library(plotly)
library(stringr)

# Load state-level immigration data
state_data <- read_csv("per_state.csv")

# Rename columns for consistency
colnames(state_data) <- c("state", "total_immigrants", "percentage")

# Convert 'percentage' column: Remove '%' and convert to numeric
state_data <- state_data %>%
  mutate(
    percentage = as.numeric(str_replace(percentage, "%", "")),  # Remove % symbol
    percentage = percentage / 100  # Convert to decimal (e.g., 19.07% → 0.1907)
  )

# Ensure no NA or Inf values
state_data <- state_data %>%
  mutate(
    total_immigrants = ifelse(is.na(total_immigrants), 0, total_immigrants),
    percentage = ifelse(is.na(percentage) | is.infinite(percentage), 0, percentage)
  )

# Load US map data
us_states <- map_data("state")

# Convert state names to lowercase for merging
state_data$state <- tolower(state_data$state)

# Merge state-level data with map data
us_map <- left_join(us_states, state_data, by = c("region" = "state"))

# Define a 100-color palette (White = Highest, Red = Lowest)
color_palette <- colorRampPalette(c("white", "red"))(100)  # Opposite of before

# Load city (AOR region) data from cbp_resp.csv
df <- read_csv("cbp_resp.csv")

# Aggregate city-level (AOR) data
city_data <- df %>%
  filter(!is.na(aor_abbv) & !is.na(encounter_count)) %>%
  group_by(aor_abbv) %>%
  summarise(total_immigrants = sum(encounter_count, na.rm = TRUE)) %>%
  ungroup()

# Ensure no NA values in city data
city_data <- city_data %>%
  mutate(total_immigrants = ifelse(is.na(total_immigrants), 0, total_immigrants))

# Define maximum bubble size relative to states
max_state_immigrants <- max(us_map$total_immigrants, na.rm = TRUE)
max_city_immigrants <- max(city_data$total_immigrants, na.rm = TRUE)

# Prevent division by zero error
if (max_city_immigrants == 0) max_city_immigrants <- 1

# Scale bubble sizes relative to states
city_data <- city_data %>%
  mutate(size = (total_immigrants / max_city_immigrants) * (max_state_immigrants / 10))

# Temporary fix: Assign random x and y values (should be replaced with real city locations)
set.seed(123)  # Ensure reproducibility
city_data$x <- runif(nrow(city_data), -125, -66)  # Longitude (USA range)
city_data$y <- runif(nrow(city_data), 25, 49)     # Latitude (USA range)

# Shiny UI
ui <- fluidPage(
  titlePanel("U.S. Immigration Maps (2021-2024)"),
  tabsetPanel(
    
    # First Tab: State-Level Choropleth Map
    tabPanel("State-Level Immigration",
             sidebarLayout(
               sidebarPanel(
                 helpText("Hover over a state to see immigration details. 
                          Darker red means fewer immigrants, white means the most.")
               ),
               mainPanel(
                 plotlyOutput("stateMap", height = "700px")
               )
             )
    ),
    
    # Second Tab: City-Level Bubble Map
    tabPanel("City-Level Immigration",
             sidebarLayout(
               sidebarPanel(
                 helpText("Bubble size represents the number of immigrants per city (AOR region).")
               ),
               mainPanel(
                 plotlyOutput("cityMap", height = "700px")
               )
             )
    )
  )
)

# Shiny Server
server <- function(input, output) {
  
  # State-Level Map
  output$stateMap <- renderPlotly({
    
    p1 <- ggplot(us_map, aes(
      x = long, y = lat, group = group, fill = percentage,
      text = paste(
        "State: ", region, "<br>",
        "Total Immigrants: ", scales::comma(total_immigrants), "<br>",
        "Percentage of Total: ", round(percentage * 100, 2), "%"  # Convert back to % format for display
      )
    )) +
      geom_polygon(color = "black") +
      scale_fill_gradientn(
        colors = color_palette, 
        name = "Immigrant Share (%)",
        breaks = seq(min(us_map$percentage, na.rm = TRUE), 
                     max(us_map$percentage, na.rm = TRUE), length.out = 10),
        labels = function(x) paste0(round(x * 100, 2), "%"),
        na.value = "gray90"
      ) +
      labs(title = "State-Level Immigration Intensity (2021-2024)",
           subtitle = "Shading represents total immigrants in each state",
           x = "", y = "",
           caption = "Source: U.S. Customs and Border Patrol") +
      theme_minimal() +
      theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
    
    ggplotly(p1, tooltip = "text") %>%
      layout(
        geo = list(
          scope = "usa",
          showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = "albers usa")
        )
      )
  })
  
  # City-Level Bubble Map
  output$cityMap <- renderPlotly({
    
    p2 <- ggplot() +
      borders("state", colour = "gray50", fill = "gray90") +  # State borders in the background
      geom_point(data = city_data, aes(
        x = x, y = y,
        size = size, text = paste(
          "City/AOR: ", aor_abbv, "<br>",
          "Total Immigrants: ", scales::comma(total_immigrants)
        )
      ), color = "blue", alpha = 0.6) +
      scale_size_continuous(range = c(2, 10), name = "City Immigration") +  # Proper scaling
      labs(title = "City-Level Immigration Intensity (2021-2024)",
           subtitle = "Bubble size represents total immigrants per city",
           x = "", y = "",
           caption = "Source: U.S. Customs and Border Patrol") +
      theme_minimal() +
      theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
    
    ggplotly(p2, tooltip = "text")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)











