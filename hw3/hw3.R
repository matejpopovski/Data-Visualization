#---
#title: "hw3"
#output:
#author: "Matej Popovski"
#date: "16 Mar 2025"
#---

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load the dataset
df <- read_csv("cbp_resp.csv")

# Check the first few rows
head(df)

# Check fiscal_year column
str(df$fiscal_year)
unique(df$fiscal_year)  # See what values exist

# Ensure fiscal_year is numeric and filter out invalid rows
df <- df %>% 
  filter(!is.na(fiscal_year) & !is.na(encounter_count)) %>%
  mutate(fiscal_year = as.numeric(fiscal_year))

# Aggregate data to get total encounters per year
yearly_totals <- df %>%
  group_by(fiscal_year) %>%
  summarise(total_immigrants = sum(encounter_count, na.rm = TRUE))

# Check if yearly_totals has data
print(yearly_totals)

# Create the plot
ggplot(yearly_totals, aes(x = fiscal_year, y = total_immigrants)) +
  geom_point(color = "blue") +  # Scatter points
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  # Regression line
  labs(title = "Total Number of Immigrants Per Year",
       x = "Year",
       y = "Total Immigrants") +
  theme_minimal()

## -----------

# Aggregate data: sum of encounters by citizenship (country)
immigration_by_country <- df %>%
  filter(!is.na(citizenship) & !is.na(encounter_count)) %>%
  group_by(citizenship) %>%
  summarise(total_immigrants = sum(encounter_count, na.rm = TRUE))

# Load world map data
world_map <- map_data("world")

# Rename country column to match map data
immigration_by_country <- immigration_by_country %>%
  rename(region = citizenship)

# Merge immigration data with map data
map_data_merged <- left_join(world_map, immigration_by_country, by = "region")

# Replace NA values with 0 for countries with no data
map_data_merged$total_immigrants[is.na(map_data_merged$total_immigrants)] <- 0

# Apply log scale to total_immigrants for better contrast
map_data_merged$log_immigrants <- log10(map_data_merged$total_immigrants + 1)  # Avoid log(0)

# Create the map with log-scale coloring
ggplot(map_data_merged, aes(x = long, y = lat, group = group, fill = log_immigrants)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkred", name = "Log Immigrants") +  # Log scale for better contrast
  labs(title = "Global Immigration Intensity to the U.S.",
       subtitle = "Shading represents the number of immigrants from each country (Log Scale)",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

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





# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

# Load the dataset
df <- read_csv("cbp_resp.csv")

# Ensure fiscal_year is numeric and filter out invalid rows
df <- df %>% 
  filter(!is.na(fiscal_year) & !is.na(encounter_count) & !is.na(month_abbv)) %>%
  mutate(fiscal_year = as.numeric(fiscal_year))

# Define a mapping of month abbreviations to numeric months
month_order <- c("JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6,
                 "JUL" = 7, "AUG" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)

# Convert month abbreviations to numeric values
df <- df %>%
  mutate(month_num = month_order[month_abbv]) %>%
  mutate(date = as.Date(paste(fiscal_year, month_num, "01", sep = "-")))  # Create Year-Month date

# Aggregate data to get total encounters per Year-Month
monthly_totals <- df %>%
  group_by(date, month_abbv) %>%
  summarise(total_immigrants = sum(encounter_count, na.rm = TRUE)) %>%
  ungroup()

# Create the plot with Year-Month on x-axis
ggplot(monthly_totals, aes(x = date, y = total_immigrants, color = month_abbv)) +
  geom_point(alpha = 0.6, size = 2) +  # Scatter points for each month
  geom_smooth(se = FALSE, span = 0.3) +  # Smooth trend line for monthly variations
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 1) +  # Regression line
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +  # Format x-axis to show Month-Year
  labs(title = "Total Number of Immigrants Per Month and Year",
       subtitle = "Colored scatter for each month, with smooth trend and linear regression",
       x = "Year-Month",
       y = "Total Immigrants",
       color = "Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "right")









