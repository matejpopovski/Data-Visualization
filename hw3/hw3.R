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




##---

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(maps)

# Load dataset
df <- read_csv("cbp_resp.csv")

# Aggregate data: total encounters by land border region
border_aggregated <- df %>%
  filter(!is.na(land_border_region) & !is.na(encounter_count)) %>%
  group_by(land_border_region) %>%
  summarise(total_immigrants = sum(encounter_count, na.rm = TRUE))

# Print to check border names
print(border_aggregated)

# Load U.S. state map data
us_map <- map_data("state")

# Manually map border regions to corresponding states (approximate)
border_state_map <- data.frame(
  land_border_region = c("Northern Land Border", "Southern Land Border"),
  state = c("montana", "texas")  # Main representative state
)

# Merge aggregated data with state-level mapping
border_data_merged <- left_join(border_aggregated, border_state_map, by = "land_border_region")

# Merge with U.S. map data
us_map$region <- as.character(us_map$region)  # Ensure regions match
map_data_merged <- left_join(us_map, border_data_merged, by = c("region" = "state"))

# Replace NA values with 0 for missing regions
map_data_merged$total_immigrants[is.na(map_data_merged$total_immigrants)] <- 0

# Apply log scale to encounters for better contrast
map_data_merged$log_immigrants <- log10(map_data_merged$total_immigrants + 1)

# Create the U.S. border map
ggplot(map_data_merged, aes(x = long, y = lat, group = group, fill = log_immigrants)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkred", name = "Log Immigrants") +  # Log scale for better contrast
  labs(title = "Illegal Immigrant Entry Points in the U.S.",
       subtitle = "Shading represents the number of encounters at different borders (Log Scale)",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

# ---



















